# Environment-driven configuration. This is the single place the front-end reads
# environment variables; every other module receives a resolved config list. The
# prod-guard assertions (mirroring the back service's) refuse to start an unsafe
# server: BYPASS_AUTH is never allowed in prod, and the Auth0 / session secrets
# are mandatory whenever authentication is actually in force.

get_config <- function() {
    environment <- tolower(env_get("ENVIRONMENT", "dev"))
    if (!environment %in% c("dev", "prod")) {
        cli::cli_abort("ENVIRONMENT must be {.val dev} or {.val prod}, not {.val {environment}}")
    }
    bypass_auth <- env_flag("BYPASS_AUTH", FALSE)

    config <- list(
        environment = environment,
        host = env_get("HOST", "0.0.0.0"),
        port = env_int("PORT", 8080L),
        bypass_auth = bypass_auth,
        log_level = tolower(env_get("LOG_LEVEL", "info")),
        pg = list(
            host = env_get("PGHOST", "127.0.0.1"),
            port = env_int("PGPORT", 5433L),
            dbname = env_get("PGDATABASE", "apps"),
            user = env_get("PGUSER", "plumber_base"),
            password = env_get("PGPASSWORD", "plumber_base")
        ),
        app_url = env_get("APP_URL", "http://localhost:8080"),
        backend_url = env_get("BACKEND_URL", "http://127.0.0.1:8081"),
        backend_public_url = env_get("BACKEND_PUBLIC_URL", "http://localhost:8081"),
        auth0 = list(
            domain = env_get("AUTH0_DOMAIN", ""),
            client_id = env_get("AUTH0_CLIENT_ID", ""),
            client_secret = env_get("AUTH0_CLIENT_SECRET", ""),
            audience = env_get("AUTH0_AUDIENCE", ""),
            # Prefix of the roles custom claim on the ID token (same Action as
            # the BE's access-token claims); normalized to end with "/".
            claim_namespace = normalize_claim_namespace(env_get("AUTH0_CLAIM_NAMESPACE", "")),
            mgmt_client_id = env_get("AUTH0_MGMT_CLIENT_ID", ""),
            mgmt_client_secret = env_get("AUTH0_MGMT_CLIENT_SECRET", "")
        ),
        session_key = env_get("SESSION_KEY", ""),
        chat = chat_config(bypass_auth)
    )

    validate_config(config)
    config
}

# Dataset-chatbot knobs. The provider keys and the router URL arrive from the
# platform harness env file (deploy/compose.yml), so the feature auto-hides
# when the deployment has none: `enabled` stays FALSE unless a model source is
# actually reachable. CHAT_ALLOW_GUESTS is honored only under BYPASS_AUTH,
# where every session is a guest and the app would otherwise be untestable.
chat_config <- function(bypass_auth) {
    llama_base_url <- sub("/+$", "", env_get("LLAMA_BASE_URL", ""))
    fallback_provider <- env_get("CHAT_FALLBACK_PROVIDER", "")
    fallback_model <- env_get("CHAT_FALLBACK_MODEL", "")
    has_model_source <- nzchar(llama_base_url) || (nzchar(fallback_provider) && nzchar(fallback_model))
    list(
        enabled = env_flag("CHAT_ENABLED", FALSE) && has_model_source,
        allow_guests = bypass_auth && env_flag("CHAT_ALLOW_GUESTS", FALSE),
        llama_base_url = llama_base_url,
        llama_api_key = env_get("LLAMA_API_KEY", ""),
        llama_provider = env_get("CHAT_LLAMA_PROVIDER", "llama.cpp"),
        llama_models = split_csv(env_get("CHAT_LLAMA_MODELS", "")),
        fallback_provider = fallback_provider,
        fallback_model = fallback_model,
        websearch = nzchar(env_get("TAVILY_API_KEY", "")),
        pi_bin = env_get("CHAT_PI_BIN", "pi"),
        duckdb_bin = env_get("CHAT_DUCKDB_BIN", "duckdb"),
        # Fixed (not tempdir()) so a restart can sweep the workdirs the previous
        # process leaked: api_on("end") does not run on SIGTERM.
        workdir_root = env_get("CHAT_WORKDIR_ROOT", file.path(dirname(tempdir()), "plumber2-chat")),
        max_sessions = env_int("CHAT_MAX_SESSIONS", 4L),
        daily_turns = env_int("CHAT_DAILY_TURNS", 50L)
    )
}

# --- helpers ---------------------------------------------------------------

validate_config <- function(config) {
    if (identical(config$environment, "prod") && config$bypass_auth) {
        cli::cli_abort("BYPASS_AUTH must never be enabled when ENVIRONMENT is {.val prod}")
    }

    # In prod the OIDC flow sends the client secret, authorization codes and
    # refresh tokens to AUTH0_DOMAIN: it must be HTTPS. Plain http:// is only for
    # dev/test tenants (webfakes). A bare host (no scheme) becomes https later.
    if (identical(config$environment, "prod") && startsWith(tolower(config$auth0$domain), "http://")) {
        cli::cli_abort("AUTH0_DOMAIN must use https when ENVIRONMENT is {.val prod}")
    }

    # Auth is bypassed only in dev; whenever it is in force the login secrets are
    # required. This covers prod (which can never bypass) and non-bypass dev.
    if (!config$bypass_auth) {
        required <- c(
            AUTH0_DOMAIN = config$auth0$domain,
            AUTH0_CLIENT_ID = config$auth0$client_id,
            AUTH0_CLIENT_SECRET = config$auth0$client_secret,
            AUTH0_AUDIENCE = config$auth0$audience,
            AUTH0_CLAIM_NAMESPACE = config$auth0$claim_namespace,
            SESSION_KEY = config$session_key
        )
        missing <- names(required)[!nzchar(required)]
        if (length(missing) > 0) {
            cli::cli_abort(c(
                "Missing required configuration for authenticated operation:",
                "x" = "{.envvar {missing}}",
                "i" = "Set these variables, or enable {.envvar BYPASS_AUTH} in a dev environment."
            ))
        }
    }
    invisible(config)
}

env_get <- function(name, default) {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value) || !nzchar(value)) default else value
}

env_int <- function(name, default) {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value) || !nzchar(value)) {
        return(as.integer(default))
    }
    parsed <- suppressWarnings(as.integer(value))
    if (is.na(parsed)) {
        cli::cli_abort("{.envvar {name}} must be an integer, got {.val {value}}")
    }
    parsed
}

normalize_claim_namespace <- function(namespace) {
    if (!nzchar(namespace)) {
        return("")
    }
    sub("/?$", "/", namespace)
}

split_csv <- function(value) {
    parts <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
    parts[nzchar(parts)]
}

env_flag <- function(name, default) {
    value <- Sys.getenv(name, unset = NA_character_)
    if (is.na(value) || !nzchar(value)) {
        return(default)
    }
    tolower(value) %in% c("true", "1", "yes", "on")
}
