# Helpers for the in-process integration tests (test-routes.R). Adapted from the
# spike (do_request / test_request) and db/tests/helper-db.R (scratch schema).

# All Set-Cookie header values (there are several: fiery's client-id cookie plus
# any the handler set). res$headers[["set-cookie"]] would return only the first.
set_cookie_values <- function(res) {
    unlist(res$headers[names(res$headers) == "set-cookie"], use.names = FALSE)
}

do_request <- function(pa, path, method = "get", headers = list(), content = "") {
    req <- fiery::fake_request(path, method = method, headers = headers, content = content)
    res <- suppressMessages(pa$test_request(req))
    list(
        status = res$status,
        headers = res$headers,
        set_cookies = set_cookie_values(res),
        body = if (is.raw(res$body)) rawToChar(res$body) else as.character(res$body)
    )
}

# Connect to the dev Postgres as the superuser so a throwaway schema can be
# created; skips the calling test if the database is unreachable.
fe_admin_connect_or_skip <- function() {
    con <- tryCatch(
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
    if (is.null(con)) {
        testthat::skip("dev Postgres (127.0.0.1:5433) not reachable")
    }
    con
}

TEST_SESSION_KEY <- "0123456789abcdef0123456789abcdef"
TEST_CLAIM_NS <- "https://plumber-base.test/"

# Assemble the full front-end api against an isolated scratch schema, parsing the
# real route files. The datastore's storr tables land in the scratch schema,
# which is dropped on exit; the users table is migrated into it so the auth flow
# can provision users. Access logging is disabled to keep test output clean.
# `bypass` toggles guest mode; `auth0` overrides tenant values (e.g. a webfakes
# base URL in `domain`); `backend_url` points the backend client somewhere (by
# default a canned fake backend is started, see backend_fake_app).
local_front_api <- function(env = parent.frame(), bypass = TRUE, auth0 = list(), backend_url = NULL, admin = FALSE) {
    con <- fe_admin_connect_or_skip()
    schema <- sprintf("fe_test_%d_%d", Sys.getpid(), sample.int(1e6L, 1L))
    quoted <- DBI::dbQuoteIdentifier(con, schema)
    DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", quoted))
    DBI::dbExecute(con, sprintf("SET search_path TO %s", quoted))
    withr::defer(
        {
            try(DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", quoted)), silent = TRUE)
            try(DBI::dbDisconnect(con), silent = TRUE)
        },
        envir = env
    )

    if (is.null(backend_url)) {
        backend_url <- local_backend_fake(env = env, admin = admin)
    }

    base_dir <- dirname(r_dir)
    config <- list(
        environment = "dev",
        host = "127.0.0.1",
        port = 8080L,
        bypass_auth = bypass,
        app_url = "http://t",
        backend_url = backend_url,
        session_key = TEST_SESSION_KEY,
        pg = list(),
        auth0 = utils::modifyList(
            list(
                domain = "",
                client_id = "fe-client",
                client_secret = "fe-secret",
                audience = "https://base-api.test",
                claim_namespace = TEST_CLAIM_NS,
                mgmt_client_id = "",
                mgmt_client_secret = ""
            ),
            auth0
        )
    )
    state <- list(
        config = config,
        con = con,
        manifest = yyjsonr::read_json_file(file.path(base_dir, "dist", "manifest.json")),
        translations = load_translations(file.path(base_dir, "assets", "translations.json")),
        template = paste(
            readLines(file.path(base_dir, "assets", "templates", "shell.html"), warn = FALSE),
            collapse = "\n"
        ),
        dist_dir = normalizePath(file.path(base_dir, "dist")),
        store_tables = list(data = "fe_store_data", keys = "fe_store_keys")
    )
    # The auth flow provisions rows in `users` (shared DDL, applied into the
    # scratch schema); migrations follow for the app tables.
    run_shared_ddl(
        con,
        normalizePath(file.path(base_dir, "..", "db", "schema-shared.sql")),
        role = NULL,
        schema = NULL
    )
    run_migrations(con, normalizePath(file.path(base_dir, "..", "db", "migrations")))
    pa <- assemble_api(state, env = globalenv(), enable_access_log = FALSE)
    route_files <- c(
        "health.R",
        "auth.R",
        "pages.R",
        "partials_home.R",
        "partials_explore.R",
        "partials_model.R",
        "partials_admin.R",
        "account.R",
        "profile.R"
    )
    suppressMessages(do.call(
        plumber2::api_parse,
        c(list(pa), lapply(route_files, function(f) file.path(base_dir, "routes", f)))
    ))
}

# --- auth-flow helpers -------------------------------------------------------

extract_cookie <- function(res, name) {
    hit <- grep(paste0("^", name, "="), res$set_cookies, value = TRUE)
    if (length(hit) == 0) {
        return(NULL)
    }
    sub(";.*$", "", hit[[1]])
}

# Perform the guest login (bypass mode) and return the session Cookie value to
# replay on subsequent requests.
guest_cookie <- function(pa) {
    res <- do_request(pa, "http://t/login")
    testthat::expect_equal(res$status, 302L)
    cookie <- extract_cookie(res, "fb_session")
    testthat::expect_false(is.null(cookie))
    cookie
}

# The CSRF token for a logged-in session, read from the shell's meta tag.
csrf_token_for <- function(pa, cookie) {
    res <- do_request(pa, "http://t/home", headers = list(Cookie = cookie))
    found <- regmatches(res$body, regexec('name="csrf-token" content="([^"]*)"', res$body))[[1]]
    testthat::expect_equal(length(found), 2L)
    found[2]
}

# A logged-in guest session plus everything a state-changing request needs to
# pass the gate (CSRF token + same-origin header).
guest_session <- function(pa) {
    cookie <- guest_cookie(pa)
    list(cookie = cookie, token = csrf_token_for(pa, cookie))
}

action_headers <- function(session, ...) {
    c(
        list(Cookie = session$cookie, Origin = "http://t", X_CSRF_Token = session$token),
        list(...)
    )
}

# A raw POST probe route so CSRF enforcement (which lives in the gate, not in
# any specific endpoint) can be exercised without domain routes.
add_csrf_probe <- function(pa) {
    probe <- routr::Route$new()
    probe$add_handler("post", "/csrf-probe", function(request, response, keys, ...) {
        response$status <- 200L
        response$type <- "text/plain"
        response$body <- charToRaw("probe-ok")
        FALSE
    })
    plumber2::api_add_route(pa, "probe", route = probe)
}

# A resolved config list like get_config() would return, pointed at a fixture
# tenant. `domain` may be a plain host or a full http:// URL (webfakes).
test_config <- function(domain = "tenant.test", app_url = "http://t") {
    list(
        environment = "dev",
        bypass_auth = FALSE,
        app_url = app_url,
        session_key = TEST_SESSION_KEY,
        auth0 = list(
            domain = domain,
            client_id = "fe-client",
            client_secret = "fe-secret",
            audience = "https://base-api.test",
            claim_namespace = TEST_CLAIM_NS
        )
    )
}

# A stand-in for the firesale datastore: an environment gives the same
# $session$auth read/write surface the helpers use.
fake_datastore <- function() {
    list(session = new.env(parent = emptyenv()))
}

# --- fake Auth0 (webfakes) ----------------------------------------------------

# Minimal Auth0 stand-in for the token/revoke endpoints. Runs in a subprocess,
# so the RSA key travels as a PEM path (externalptrs cannot cross). Test
# protocol for /oauth/token (authorization_code): the `code` carries the nonce
# to embed in the ID token; an "unverified:" prefix flips email_verified off.
# Refresh grants rotate the refresh token and reject anything but the current
# one (Auth0 rotation reuse detection). GET /stats exposes the counters.
auth0_fake_app <- function(key_pem_path, claim_ns) {
    app <- webfakes::new_app()
    app$use(webfakes::mw_urlencoded())
    app$use(webfakes::mw_json())
    app$locals$n_token <- 0L
    app$locals$n_revoke <- 0L
    app$locals$n_mgmt_token <- 0L
    app$locals$n_mgmt_patch <- 0L
    app$locals$last_patch <- NULL
    app$locals$current_rt <- "rt-1"

    app$post("/oauth/token", function(req, res) {
        locals <- req$app$locals
        locals$n_token <- locals$n_token + 1L
        now <- as.numeric(Sys.time())
        if (identical(req$form$grant_type, "authorization_code")) {
            code <- req$form$code
            email_verified <- !startsWith(code, "unverified:")
            nonce <- sub("^unverified:", "", code)
            iss <- paste0("http://", req$get_header("Host"), "/")
            claims <- list(
                iss = iss,
                aud = "fe-client",
                sub = "auth0|fe-user",
                sid = "sid-1",
                nonce = nonce,
                email = "user@example.test",
                email_verified = email_verified,
                nickname = "tester",
                exp = now + 600,
                iat = now
            )
            claims[[paste0(claim_ns, "roles")]] <- "user"
            id_token <- jose::jwt_encode_sig(
                do.call(jose::jwt_claim, claims),
                key = openssl::read_key(key_pem_path),
                header = list(typ = "JWT", kid = "fe-test-key")
            )
            res$send_json(
                list(
                    access_token = "at-1",
                    id_token = id_token,
                    refresh_token = locals$current_rt,
                    expires_in = 900,
                    token_type = "Bearer"
                ),
                auto_unbox = TRUE
            )
        } else if (identical(req$form$grant_type, "client_credentials")) {
            locals$n_mgmt_token <- locals$n_mgmt_token + 1L
            res$send_json(
                list(access_token = "mgmt-token-1", expires_in = 3600, token_type = "Bearer"),
                auto_unbox = TRUE
            )
        } else if (identical(req$form$grant_type, "refresh_token")) {
            if (!identical(req$form$refresh_token, locals$current_rt)) {
                res$set_status(403L)$send_json(list(error = "invalid_grant"), auto_unbox = TRUE)
            } else {
                locals$current_rt <- paste0("rt-rotated-", locals$n_token)
                res$send_json(
                    list(
                        access_token = paste0("at-refreshed-", locals$n_token),
                        refresh_token = locals$current_rt,
                        expires_in = 900,
                        token_type = "Bearer"
                    ),
                    auto_unbox = TRUE
                )
            }
        } else {
            res$set_status(400L)$send_json(list(error = "unsupported_grant_type"), auto_unbox = TRUE)
        }
    })

    app$post("/oauth/revoke", function(req, res) {
        req$app$locals$n_revoke <- req$app$locals$n_revoke + 1L
        res$set_status(200L)$send("")
    })

    # new_regexp: webfakes' :id segments do not match the | in decoded Auth0
    # subs (auth0|xyz).
    app$patch(webfakes::new_regexp("^/api/v2/users/(?<uid>.+)$"), function(req, res) {
        locals <- req$app$locals
        locals$n_mgmt_patch <- locals$n_mgmt_patch + 1L
        locals$last_patch <- list(id = req$params$uid, nickname = req$json$nickname)
        res$send_json(list(nickname = req$json$nickname), auto_unbox = TRUE)
    })

    app$get("/stats", function(req, res) {
        locals <- req$app$locals
        res$send_json(
            list(
                n_token = locals$n_token,
                n_revoke = locals$n_revoke,
                n_mgmt_token = locals$n_mgmt_token,
                n_mgmt_patch = locals$n_mgmt_patch,
                last_patch = locals$last_patch,
                current_rt = locals$current_rt
            ),
            auto_unbox = TRUE
        )
    })

    app
}

# Launch the fake tenant; returns list(base_url, stats). Cleaned up with `env`.
local_auth0_fake <- function(fixture, env = parent.frame()) {
    pem <- tempfile(fileext = ".pem")
    openssl::write_pem(fixture$key, pem)
    withr::defer(unlink(pem), envir = env)
    process <- webfakes::local_app_process(
        auth0_fake_app(pem, TEST_CLAIM_NS),
        .local_envir = env
    )
    base_url <- sub("/+$", "", process$url())
    list(
        base_url = base_url,
        stats = function() {
            httr2::request(paste0(base_url, "/stats")) |>
                httr2::req_perform() |>
                httr2::resp_body_json()
        }
    )
}

# --- fake backend (webfakes) ---------------------------------------------------

# Canned back stand-in serving the JSON shapes of the real /v1 endpoints
# (fixtures mirror back's *_json helpers). Behavior knobs are baked into
# the fixtures: dataset/model/key id 404 answers 404; POST /v1/models routes on
# the formula ("boom" -> 422, "cap ~ x" -> 429, "slow ~ x" -> a running job);
# POST /v1/keys with name "dup" -> 409. `admin = TRUE` adds view:admin to the
# scopes /v1/me reports (and that session_scopes caches).
backend_fake_app <- function(admin = FALSE) {
    app <- webfakes::new_app()
    app$use(webfakes::mw_json())

    problem <- function(res, status, title, detail) {
        res$set_status(status)
        res$set_header("Content-Type", "application/problem+json")
        res$send(yyjsonr::write_json_str(
            list(title = title, status = status, detail = detail),
            auto_unbox = TRUE
        ))
    }

    ds1 <- list(
        id = 1L,
        name = "cars",
        description = "Speed and stopping distances",
        n_rows = 50L,
        n_cols = 2L,
        created_at = "2026-07-01T10:00:00Z",
        updated_at = "2026-07-01T10:00:00Z"
    )
    ds2 <- list(
        id = 2L,
        name = "trees",
        description = NULL,
        n_rows = 31L,
        n_cols = 3L,
        created_at = "2026-06-15T09:30:00Z",
        updated_at = "2026-06-15T09:30:00Z"
    )
    ds1_summary <- list(
        speed = list(type = "numeric", n_missing = 0L, min = 4, max = 25, mean = 15.4),
        dist = list(type = "numeric", n_missing = 0L, min = 2, max = 120, mean = 42.98)
    )
    model7 <- list(
        id = 7L,
        dataset_id = 1L,
        formula = "dist ~ speed",
        metrics = list(
            r_squared = 0.6511,
            rmse = 15.0688,
            aic = 419.157,
            summary_text = "Call:\nlm(formula = dist ~ speed, data = data)\n\nCoefficients: (fixture)"
        ),
        created_at = "2026-07-02T10:00:00Z"
    )

    app$get("/v1/me", function(req, res) {
        scopes <- c(
            "write:datasets",
            "write:models",
            "manage:keys",
            if (admin) c("view:admin", "manage:admin:roles")
        )
        res$send_json(
            list(
                user = list(
                    id = 1L,
                    auth0_sub = NULL,
                    email = NULL,
                    nickname = "guest",
                    is_guest = TRUE,
                    created_at = "2026-07-01T00:00:00Z"
                ),
                auth = "bypass",
                scopes = as.list(scopes)
            ),
            auto_unbox = TRUE
        )
    })

    app$get("/v1/datasets", function(req, res) {
        items <- list(ds1, ds2)
        if (!is.null(req$query$min_rows)) {
            items <- Filter(function(ds) ds$n_rows >= as.integer(req$query$min_rows), items)
        }
        if (!is.null(req$query$max_rows)) {
            items <- Filter(function(ds) ds$n_rows <= as.integer(req$query$max_rows), items)
        }
        res$send_json(list(items = items, next_after = NULL), auto_unbox = TRUE)
    })
    app$post("/v1/datasets", function(req, res) {
        created <- utils::modifyList(ds1, list(id = 3L, name = "uploaded", description = NULL))
        res$set_status(201L)$send_json(created, auto_unbox = TRUE)
    })
    app$get("/v1/datasets/:id", function(req, res) {
        if (identical(req$params$id, "1")) {
            res$send_json(c(ds1, list(summary = ds1_summary)), auto_unbox = TRUE)
        } else if (identical(req$params$id, "2")) {
            res$send_json(
                c(ds2, list(summary = list(x = list(type = "character", n_missing = 0L, n_unique = 5L)))),
                auto_unbox = TRUE
            )
        } else {
            problem(res, 404L, "Not Found", "no such dataset")
        }
    })
    app$get("/v1/datasets/:id/data", function(req, res) {
        if (!identical(req$params$id, "1")) {
            return(problem(res, 404L, "Not Found", "no such dataset"))
        }
        offset <- as.integer(req$query$offset %||% "0")
        limit <- as.integer(req$query$limit %||% "50")
        data <- datasets::cars
        idx <- seq_len(nrow(data))
        idx <- idx[idx > offset & idx <= offset + limit]
        res$send_json(
            list(
                n_rows = nrow(data),
                offset = offset,
                columns = as.list(names(data)),
                rows = data[idx, , drop = FALSE]
            ),
            auto_unbox = TRUE
        )
    })
    app$get("/v1/datasets/:id/data.csv", function(req, res) {
        if (!identical(req$params$id, "1")) {
            return(problem(res, 404L, "Not Found", "no such dataset"))
        }
        res$set_header("Content-Disposition", 'attachment; filename="cars.csv"')
        res$set_type("text/csv")
        res$send("speed,dist\n4,2\n4,10\n")
    })
    app$patch("/v1/datasets/:id", function(req, res) {
        if (!identical(req$params$id, "1")) {
            return(problem(res, 404L, "Not Found", "no such dataset"))
        }
        res$send_json(utils::modifyList(ds1, list(name = req$json$name)), auto_unbox = TRUE)
    })
    app$delete("/v1/datasets/:id", function(req, res) {
        if (!identical(req$params$id, "1")) {
            return(problem(res, 404L, "Not Found", "no such dataset"))
        }
        res$set_status(204L)$send("")
    })

    app$post("/v1/models", function(req, res) {
        formula <- req$json$formula %||% ""
        if (identical(formula, "boom")) {
            problem(res, 422L, "Unprocessable Entity", "formula uses disallowed function 'system'")
        } else if (identical(formula, "cap ~ x")) {
            problem(res, 429L, "Too Many Requests", "too many jobs in flight")
        } else if (identical(formula, "slow ~ x")) {
            res$set_status(202L)$send_json(list(job_id = "job-running"), auto_unbox = TRUE)
        } else {
            res$set_status(202L)$send_json(list(job_id = "job-done"), auto_unbox = TRUE)
        }
    })
    app$get("/v1/jobs/:id", function(req, res) {
        base <- list(
            id = req$params$id,
            kind = "fit_model",
            created_at = "2026-07-02T10:00:00Z",
            updated_at = "2026-07-02T10:00:05Z"
        )
        if (identical(req$params$id, "job-running")) {
            res$send_json(c(base, list(status = "running")), auto_unbox = TRUE)
        } else if (identical(req$params$id, "job-done")) {
            res$send_json(c(base, list(status = "done", result = list(model_id = 7L))), auto_unbox = TRUE)
        } else if (identical(req$params$id, "job-error")) {
            res$send_json(c(base, list(status = "error", error = "fit failed: singular matrix")), auto_unbox = TRUE)
        } else {
            problem(res, 404L, "Not Found", "no such job")
        }
    })
    app$get("/v1/models", function(req, res) {
        res$send_json(list(items = list(model7), next_after = NULL), auto_unbox = TRUE)
    })
    app$get("/v1/models/:id", function(req, res) {
        if (identical(req$params$id, "7")) {
            res$send_json(model7, auto_unbox = TRUE)
        } else {
            problem(res, 404L, "Not Found", "no such model")
        }
    })
    app$delete("/v1/models/:id", function(req, res) {
        if (identical(req$params$id, "7")) {
            res$set_status(204L)$send("")
        } else {
            problem(res, 404L, "Not Found", "no such model")
        }
    })

    app$get("/v1/keys", function(req, res) {
        res$send_json(
            list(
                items = list(
                    list(
                        id = 9L,
                        name = "ci",
                        key_prefix = "pbk_abcd",
                        scopes = list("write:datasets"),
                        last_used_at = NULL,
                        expires_at = NULL,
                        revoked = FALSE,
                        created_at = "2026-07-01T00:00:00Z"
                    ),
                    list(
                        id = 8L,
                        name = "old",
                        key_prefix = "pbk_dead",
                        scopes = list(),
                        last_used_at = "2026-07-01T00:00:00Z",
                        expires_at = NULL,
                        revoked = TRUE,
                        created_at = "2026-06-01T00:00:00Z"
                    )
                )
            ),
            auto_unbox = TRUE
        )
    })
    app$post("/v1/keys", function(req, res) {
        if (identical(req$json$name, "dup")) {
            return(problem(res, 409L, "Conflict", "a key with this name already exists"))
        }
        res$set_status(201L)$send_json(
            list(
                id = 10L,
                name = req$json$name,
                secret = paste0("pbk_", strrep("f0", 32)),
                key_prefix = "pbk_f0f0",
                scopes = req$json$scopes %||% list()
            ),
            auto_unbox = TRUE
        )
    })
    app$delete("/v1/keys/:id", function(req, res) {
        if (identical(req$params$id, "9")) {
            res$set_status(204L)$send("")
        } else {
            problem(res, 404L, "Not Found", "no such active key")
        }
    })

    app$get("/v1/admin/users", function(req, res) {
        res$send_json(
            list(
                items = list(
                    list(
                        id = 1L,
                        auth0_sub = NULL,
                        email = NULL,
                        nickname = "guest",
                        is_guest = TRUE,
                        created_at = "2026-07-01T00:00:00Z",
                        last_seen_at = "2026-07-05T00:00:00Z",
                        n_datasets = 2L,
                        n_models = 1L,
                        n_api_keys = 1L,
                        role = NULL
                    ),
                    list(
                        id = 2L,
                        auth0_sub = "auth0|u2",
                        email = "dev@example.com",
                        nickname = "dev",
                        is_guest = FALSE,
                        created_at = "2026-07-01T00:00:00Z",
                        last_seen_at = "2026-07-05T00:00:00Z",
                        n_datasets = 0L,
                        n_models = 0L,
                        n_api_keys = 0L,
                        role = "dev"
                    )
                )
            ),
            auto_unbox = TRUE
        )
    })
    app$get("/v1/admin/roles", function(req, res) {
        res$send_json(
            list(
                default_role = "user",
                items = list(
                    list(id = "rol_admin", name = "admin", description = "Administrator", in_yaml = TRUE),
                    list(id = "rol_dev", name = "dev", description = "Developer", in_yaml = TRUE),
                    list(id = "rol_beta", name = "beta", description = "Beta tester", in_yaml = FALSE)
                )
            ),
            auto_unbox = TRUE
        )
    })
    # Role update: guests are refused like the real BE; changing user 2 to
    # admin echoes the applied role.
    app$put("/v1/admin/users/:id/role", function(req, res) {
        if (identical(req$params$id, "1")) {
            res$set_status(422L)$send_json(
                list(title = "Unprocessable Entity", detail = "guest users have no Auth0 identity"),
                auto_unbox = TRUE
            )
            return()
        }
        role_id <- req$json$role_id %||% ""
        role <- switch(role_id, rol_admin = "admin", rol_dev = "dev", rol_beta = "beta", NULL)
        res$send_json(
            list(user_id = as.integer(req$params$id), role = role, role_id = if (nzchar(role_id)) role_id),
            auto_unbox = TRUE
        )
    })
    app$get("/v1/admin/sessions", function(req, res) {
        res$send_json(list(items = list(list(namespace = "fe_sessions", n = 3L))), auto_unbox = TRUE)
    })
    # Bare non-JSON denial, like the BE guard's abort path (no problem+json body).
    app$get("/v1/bare-403", function(req, res) {
        res$set_status(403L)$set_type("text/plain")$send("Forbidden")
    })
    app$get("/v1/admin/requests", function(req, res) {
        res$send_json(
            list(
                window_hours = as.integer(req$query$hours %||% "24"),
                items = list(list(
                    service = "back",
                    method = "GET",
                    path = "/v1/datasets",
                    status = 200L,
                    n = 42L,
                    avg_ms = 12.5,
                    max_ms = 40L
                ))
            ),
            auto_unbox = TRUE
        )
    })

    app
}

# Launch the fake backend; returns its base url. Cleaned up with `env`.
local_backend_fake <- function(env = parent.frame(), admin = FALSE) {
    process <- webfakes::local_app_process(backend_fake_app(admin = admin), .local_envir = env)
    sub("/+$", "", process$url())
}

# --- RS256 fixtures (mirrors back's helper) ------------------------------

new_jwt_fixture <- function(kid = "fe-test-key") {
    key <- openssl::rsa_keygen(2048)
    jwk <- yyjsonr::read_json_str(jose::write_jwk(as.list(key)$pubkey))
    jwk$kid <- kid
    list(key = key, kid = kid, jwks = list(keys = list(jwk)))
}

use_fixture_jwks <- function(fixture, env = parent.frame()) {
    set_jwks_fetcher(function() list(keys = fixture$jwks$keys, max_age = NULL))
    withr::defer(set_jwks_fetcher(NULL), envir = env)
}

sign_id_token <- function(
    fixture,
    iss,
    aud = "fe-client",
    sub = "auth0|fe-user",
    nonce = "nonce-1",
    email = "user@example.test",
    email_verified = TRUE,
    nickname = "tester",
    roles = character(),
    exp = as.numeric(Sys.time()) + 600,
    iat = as.numeric(Sys.time()),
    kid = fixture$kid,
    key = fixture$key,
    extra_claims = list()
) {
    claims <- list(
        iss = iss,
        aud = aud,
        sub = sub,
        nonce = nonce,
        email = email,
        email_verified = email_verified,
        nickname = nickname,
        exp = exp,
        iat = iat
    )
    claims[[paste0(TEST_CLAIM_NS, "roles")]] <- roles
    claims <- c(claims, extra_claims)
    jose::jwt_encode_sig(
        do.call(jose::jwt_claim, claims),
        key = key,
        header = list(typ = "JWT", kid = kid)
    )
}
