# Service configuration. This is the ONLY place in back that reads
# environment variables; every other module receives the validated config list.
# get_config() also enforces the "never in prod" startup assertions: it stops
# with a clear message rather than trusting convention.

get_config <- function() {
    environment <- tolower(env_str("ENVIRONMENT", "dev"))
    if (!environment %in% c("dev", "prod")) {
        cli::cli_abort("ENVIRONMENT must be {.val dev} or {.val prod}, not {.val {environment}}.")
    }
    is_prod <- environment == "prod"

    config <- list(
        environment = environment,
        is_prod = is_prod,
        host = env_str("HOST", if (is_prod) "" else "127.0.0.1"),
        port = env_int("PORT", 8081L),
        bypass_auth = env_flag("BYPASS_AUTH", FALSE),
        log_level = tolower(env_str("LOG_LEVEL", "info")),
        rate_limit_per_min = env_int("RATE_LIMIT_PER_MIN", 120L),
        # Stricter buckets for the expensive endpoints. IP-level flood control
        # is the edge's job (Cloudflare rate rule): fireproof's auth route
        # always dispatches first, so no in-app pre-auth limiter can exist.
        rate_limit_uploads_per_min = env_int("RATE_LIMIT_UPLOADS_PER_MIN", 10L),
        rate_limit_fits_per_min = env_int("RATE_LIMIT_FITS_PER_MIN", 6L),
        # Layered upload limits: Traefik buffering (~12MB) > this byte cap >
        # parsed row/col caps.
        max_upload_bytes = env_int("MAX_UPLOAD_BYTES", 10485760L),
        max_dataset_rows = env_int("MAX_DATASET_ROWS", 50000L),
        max_dataset_cols = env_int("MAX_DATASET_COLS", 100L),
        max_jobs_per_user = env_int("MAX_JOBS_PER_USER", 2L),
        # A runaway fit must not hold a mirai daemon forever: the dispatcher
        # cancels it after this walltime and the job is failed.
        fit_timeout_seconds = env_int("FIT_TIMEOUT_SECONDS", 60L),
        request_log_retention_days = env_int("REQUEST_LOG_RETENTION_DAYS", 30L),
        db = list(
            host = env_str("PGHOST", "127.0.0.1"),
            port = env_int("PGPORT", 5433L),
            dbname = env_str("PGDATABASE", "apps"),
            user = env_str("PGUSER", "plumber_base"),
            password = env_str("PGPASSWORD", "plumber_base")
        ),
        auth0 = list(
            domain = env_str("AUTH0_DOMAIN", ""),
            audience = env_str("AUTH0_AUDIENCE", ""),
            # Prefix of the roles/email_verified custom claims set by the Auth0
            # post-login Action; normalized to end with "/" so a missing trailing
            # slash cannot silently break every claim lookup.
            claim_namespace = normalize_claim_namespace(env_str("AUTH0_CLAIM_NAMESPACE", "")),
            # M2M credentials for the admin role endpoints (same client the FE
            # uses for profile edits). Optional: when absent the role endpoints
            # answer 503 and the user listing omits roles (dev without Auth0).
            mgmt_client_id = env_str("AUTH0_MGMT_CLIENT_ID", ""),
            mgmt_client_secret = env_str("AUTH0_MGMT_CLIENT_SECRET", "")
        )
    )

    assert_prod_safety(config)
    config
}

# In prod, insecure conveniences are forbidden and the identity provider is
# mandatory. Missing values in dev are fine (guest bypass covers them).
assert_prod_safety <- function(config) {
    if (!config$is_prod) {
        return(invisible())
    }
    problems <- character()
    if (config$bypass_auth) {
        problems <- c(problems, "BYPASS_AUTH must not be enabled when ENVIRONMENT=prod")
    }
    if (!nzchar(config$host)) {
        problems <- c(problems, "HOST must be set explicitly when ENVIRONMENT=prod")
    }
    if (!nzchar(config$auth0$domain)) {
        problems <- c(problems, "AUTH0_DOMAIN must be set when ENVIRONMENT=prod")
    }
    if (!nzchar(config$auth0$audience)) {
        problems <- c(problems, "AUTH0_AUDIENCE must be set when ENVIRONMENT=prod")
    }
    if (!nzchar(config$auth0$claim_namespace)) {
        problems <- c(problems, "AUTH0_CLAIM_NAMESPACE must be set when ENVIRONMENT=prod")
    }
    if (length(problems)) {
        cli::cli_abort(c(
            "Invalid production configuration:",
            setNames(problems, rep("x", length(problems)))
        ))
    }
    invisible()
}

env_str <- function(name, default = "") {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value)) default else value
}

env_int <- function(name, default) {
    raw <- Sys.getenv(name, unset = "")
    if (!nzchar(raw)) {
        return(as.integer(default))
    }
    value <- suppressWarnings(as.integer(raw))
    if (is.na(value)) {
        cli::cli_abort("{.envvar {name}} must be an integer, not {.val {raw}}.")
    }
    value
}

normalize_claim_namespace <- function(namespace) {
    if (!nzchar(namespace)) {
        return("")
    }
    sub("/?$", "/", namespace)
}

# Truthy strings only; anything else (including unset) is the default.
env_flag <- function(name, default = FALSE) {
    raw <- tolower(Sys.getenv(name, unset = ""))
    if (!nzchar(raw)) {
        return(default)
    }
    raw %in% c("1", "true", "yes", "on")
}
