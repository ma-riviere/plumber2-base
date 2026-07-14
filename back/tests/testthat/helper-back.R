# Shared helpers for the back tests. testthat sources this before the test
# files. The in-process pattern is adapted from the spike (fiery::fake_request +
# pa$test_request, headers come back lower-cased, construction wrapped in
# suppressMessages to keep output pristine).

suppressWarnings(suppressMessages(library(plumber2)))

# Rejected-credential log lines are exercised deliberately all over the auth
# tests; keep the output pristine.
options(back.quiet_auth_log = TRUE)

# back/ holds _server.yml, constructor.R and R/. Resolve it whether tests run
# from the repo root or from the test dir (testthat sets wd to the test dir).
BACK_DIR <- local({
    candidates <- c(".", "back", file.path("..", ".."))
    hit <- Find(function(d) file.exists(file.path(d, "_server.yml")), candidates)
    if (is.null(hit)) {
        stop("cannot locate back/_server.yml from ", getwd())
    }
    normalizePath(hit)
})

# Make the service functions available to helpers regardless of whether an API has
# been built yet (building one re-sources these into the global env, harmlessly).
service_files <- c(
    "config.R",
    "db.R",
    "problems.R",
    "permissions.R",
    "auth.R",
    "keys.R",
    "users.R",
    "mgmt.R",
    "formula_safety.R",
    "datasets.R",
    "models.R",
    "jobs.R",
    "ratelimit.R",
    "request_log.R",
    "maintenance.R"
)
for (f in service_files) {
    source(file.path(BACK_DIR, "R", f), local = FALSE)
}

MIGRATIONS_DIR <- normalizePath(file.path(BACK_DIR, "..", "db", "migrations"))
SHARED_DDL_PATH <- normalizePath(file.path(BACK_DIR, "..", "db", "schema-shared.sql"))
source(normalizePath(file.path(BACK_DIR, "..", "db", "migrate-lib.R")), local = FALSE)

# Build the full API exactly as production does: assemble from _server.yml (runs
# the constructor, then parses route files) and add the not-found fallback last.
# Runs with the working directory set to back/ so the constructor's relative
# source() calls resolve. Lifecycle hooks (pool, mirai) do NOT fire under
# test_request, so DB-backed tests set the pool themselves via set_app_pool().
build_test_api <- function() {
    withr::with_dir(BACK_DIR, {
        pa <- suppressMessages(plumber2::api("_server.yml"))
        pa <- add_fallback_route(pa)
        # Silence logging for pristine test output: the console logger prints access
        # lines on every request, and even plumber2's logger_null() still cat()s
        # error conditions (every abort_*) to stdout. A true no-op logger avoids
        # both. Logging is exercised live in test-live.R.
        pa$set_logger(function(event, message, request = NULL, ...) invisible(NULL))
        pa
    })
}

# Run one in-process request and normalise the response. Headers are lower-cased;
# body is decoded to a character string.
do_request <- function(pa, path, method = "get", headers = list(), content = "") {
    req <- fiery::fake_request(path, method = method, headers = headers, content = content)
    res <- suppressMessages(pa$test_request(req))
    list(
        status = res$status,
        headers = res$headers,
        body = if (is.raw(res$body)) rawToChar(res$body) else as.character(res$body)
    )
}

# A live pool against the dev Postgres, or skip. Health only issues SELECT 1, so
# no schema/tables are required and the shared dev schema is never written to.
dev_pool_or_skip <- function() {
    config <- get_config()
    pool <- tryCatch(db_pool(config), error = function(e) NULL)
    if (is.null(pool) || !db_healthcheck(pool)) {
        if (!is.null(pool)) {
            pool::poolClose(pool)
        }
        testthat::skip("dev Postgres (127.0.0.1:5433) not reachable")
    }
    pool
}

# A pool pinned to a freshly migrated throwaway schema (dropped on exit), for
# tests that write (users, api_keys). Mirrors db/tests/helper-db.R.
local_migrated_pool <- function(env = parent.frame()) {
    admin <- tryCatch(
        DBI::dbConnect(
            RPostgres::Postgres(),
            host = Sys.getenv("PGHOST", "127.0.0.1"),
            port = as.integer(Sys.getenv("PGPORT", "5433")),
            dbname = Sys.getenv("PGDATABASE", "apps"),
            user = "admin",
            password = "admin",
            options = "-c client_min_messages=warning"
        ),
        error = function(e) NULL
    )
    if (is.null(admin)) {
        testthat::skip("dev Postgres (127.0.0.1:5433) not reachable")
    }
    schema <- sprintf("bb_test_%d_%d", Sys.getpid(), sample.int(1e6L, 1L))
    quoted <- DBI::dbQuoteIdentifier(admin, schema)
    DBI::dbExecute(admin, sprintf("CREATE SCHEMA %s", quoted))
    DBI::dbExecute(admin, sprintf("SET search_path TO %s", quoted))
    # Shared tables (users/datasets/models) land in the scratch schema too
    # (role/schema = NULL), keeping tests isolated from the real shared schema.
    run_shared_ddl(admin, SHARED_DDL_PATH, role = NULL, schema = NULL)
    run_migrations(admin, MIGRATIONS_DIR)
    # Each scratch schema restarts serial ids at 1, so the process-global
    # last_used_at / last_seen_at throttle caches (keyed by those ids/subs) would
    # otherwise carry stale "recently written" entries across schemas and skip
    # the writes a test then asserts. Reset them per fresh schema.
    reset_key_touch_cache()
    reset_user_seen_cache()
    pool <- pool::dbPool(
        drv = RPostgres::Postgres(),
        host = Sys.getenv("PGHOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("PGPORT", "5433")),
        dbname = Sys.getenv("PGDATABASE", "apps"),
        user = "admin",
        password = "admin",
        options = sprintf("-c client_min_messages=warning -c search_path=%s", schema)
    )
    withr::defer(
        {
            try(pool::poolClose(pool), silent = TRUE)
            try(DBI::dbExecute(admin, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", quoted)), silent = TRUE)
            try(DBI::dbDisconnect(admin), silent = TRUE)
        },
        envir = env
    )
    pool
}

# ---- Auth fixtures ------------------------------------------------------------
# RS256 at+jwt fixtures adapted from spike/tests/helper-spike.R; iss/aud/namespace
# line up with with_auth_env() below so fixture tokens verify against the config
# the api under test was built with.

TEST_AUTH0_DOMAIN <- "issuer.test"
TEST_AUTH0_ISS <- "https://issuer.test/"
TEST_AUTH0_AUD <- "https://base-api.test"
TEST_CLAIM_NS <- "https://plumber-base.test/"

# Set the auth env vars (and optionally bypass mode) for the calling test, so
# build_test_api() constructs an api wired to the fixture tenant.
with_auth_env <- function(bypass = FALSE, env = parent.frame()) {
    withr::local_envvar(
        ENVIRONMENT = "dev",
        BYPASS_AUTH = if (bypass) "true" else NA,
        AUTH0_DOMAIN = TEST_AUTH0_DOMAIN,
        AUTH0_AUDIENCE = TEST_AUTH0_AUD,
        AUTH0_CLAIM_NAMESPACE = TEST_CLAIM_NS,
        .local_envir = env
    )
}

new_jwt_fixture <- function(kid = "test-key-1") {
    key <- openssl::rsa_keygen(2048)
    jwk <- yyjsonr::read_json_str(jose::write_jwk(as.list(key)$pubkey))
    jwk$kid <- kid
    list(key = key, kid = kid, jwks = list(keys = list(jwk)))
}

# Point the JWKS cache at the fixture (call AFTER build_test_api, which resets
# the cache to an HTTP fetcher for the fixture domain).
use_fixture_jwks <- function(fixture) {
    set_jwks_fetcher(function() list(keys = fixture$jwks$keys, max_age = NULL))
}

sign_access_token <- function(
    fixture,
    iss = TEST_AUTH0_ISS,
    aud = TEST_AUTH0_AUD,
    sub = "auth0|user-1",
    roles = character(),
    email_verified = TRUE,
    exp = as.numeric(Sys.time()) + 600,
    iat = as.numeric(Sys.time()),
    typ = "at+jwt",
    kid = fixture$kid,
    key = fixture$key,
    # The guard runs profile "rfc9068" (Auth0 token_dialect rfc9068_profile),
    # which requires client_id + jti presence; NULL omits them for negative tests.
    client_id = "test-client",
    jti = "test-jti",
    extra_claims = list()
) {
    claims <- list(iss = iss, aud = aud, sub = sub, exp = exp, iat = iat)
    claims[[paste0(TEST_CLAIM_NS, "roles")]] <- roles
    if (!is.null(email_verified)) {
        claims[[paste0(TEST_CLAIM_NS, "email_verified")]] <- email_verified
    }
    if (!is.null(client_id)) {
        claims$client_id <- client_id
    }
    if (!is.null(jti)) {
        claims$jti <- jti
    }
    claims <- c(claims, extra_claims)
    jose::jwt_encode_sig(
        do.call(jose::jwt_claim, claims),
        key = key,
        header = list(typ = typ, kid = kid)
    )
}

bearer_header <- function(token) list(Authorization = paste("Bearer", token))

# Full endpoint-test context: fixture-tenant env (+ optional bypass), the real
# assembled api, a migrated scratch-schema pool wired into app_state, and a
# fixture JWKS. Used by the auth and domain endpoint tests.
auth_api <- function(bypass = FALSE, env = parent.frame()) {
    with_auth_env(bypass = bypass, env = env)
    pa <- build_test_api()
    pool <- local_migrated_pool(env = env)
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL), envir = env)
    fixture <- new_jwt_fixture()
    use_fixture_jwks(fixture)
    withr::defer(set_jwks_fetcher(NULL), envir = env)
    list(pa = pa, pool = pool, fixture = fixture)
}

# ---- Domain-test helpers --------------------------------------------------

# JSON-body request (POST/PATCH endpoints).
do_json_request <- function(pa, path, method, body, headers = list()) {
    do_request(
        pa,
        path,
        method = method,
        headers = c(headers, list(Content_Type = "application/json")),
        content = yyjsonr::write_json_str(body, auto_unbox = TRUE)
    )
}

# Multipart CSV upload body (spike test-06 pattern). `fields` adds plain parts;
# `include_file = FALSE` builds a fields-only body (missing-part tests).
multipart_csv <- function(csv_text = "", filename = "data.csv", fields = list(), include_file = TRUE) {
    boundary <- "testBoundary42x"
    extra <- vapply(
        names(fields),
        function(name) {
            paste0(
                "--",
                boundary,
                "\r\n",
                "Content-Disposition: form-data; name=\"",
                name,
                "\"\r\n\r\n",
                fields[[name]],
                "\r\n"
            )
        },
        character(1)
    )
    file_part <- if (include_file) {
        paste0(
            "--",
            boundary,
            "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"",
            filename,
            "\"\r\n",
            "Content-Type: text/csv\r\n\r\n",
            csv_text,
            "\r\n"
        )
    } else {
        ""
    }
    list(
        content = paste0(file_part, paste0(extra, collapse = ""), "--", boundary, "--\r\n"),
        headers = list(Content_Type = paste0("multipart/form-data; boundary=", boundary))
    )
}

# Pump the later loop until the job leaves pending/running (async fit tests).
wait_for_job <- function(pa, job_id, headers, timeout = 15) {
    deadline <- Sys.time() + timeout
    while (Sys.time() < deadline) {
        later::run_now(0.05)
        res <- do_request(pa, paste0("http://t/v1/jobs/", job_id), headers = headers)
        body <- yyjsonr::read_json_str(res$body)
        if (!body$status %in% c("pending", "running")) {
            return(body)
        }
    }
    stop("job did not finish within the timeout")
}
