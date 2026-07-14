# Programmatic API assembly, shared by the production constructor and the tests.
# assemble_api() wires the server-side datastore, static file serving, security
# headers, access logging and the client-id cookie onto a fresh plumber2 API and
# publishes the shared application state; the caller parses the route files into
# the returned object afterwards.

assemble_api <- function(state, env = parent.frame(), enable_access_log = TRUE) {
    config <- state$config
    is_prod <- identical(config$environment, "prod")

    driver <- storr::driver_dbi(
        state$store_tables$data,
        state$store_tables$keys,
        state$con
    )

    # strict_transport_security = NULL disables firesafety's app-level http->https
    # 308 redirect (installed whenever HSTS is set). The service runs behind a
    # TLS-terminating proxy and sees plain http internally, so it must not self
    # redirect; HSTS is applied at the edge (Traefik/Cloudflare).
    #
    # Strict CSP: no inline/eval scripts anywhere (htmx config lives in a meta
    # tag, CSRF attach in the external app.js). style-src 'self' works because
    # Bootstrap's JS animates through the CSSOM (never style attributes) and
    # htmx's indicator-style injection is disabled. img-src keeps data: for
    # Bootstrap's embedded SVGs and https: for the IdP-hosted avatar in the
    # profile modal (images cannot execute script).
    api <- plumber2::api(host = config$host, port = config$port, env = env) |>
        plumber2::api_datastore(driver, gc_interval = 3600, max_age = 604800) |>
        plumber2::api_security_headers(
            content_security_policy = plumber2::csp(
                default_src = "self",
                script_src = "self",
                script_src_attr = "none",
                style_src = "self",
                img_src = c("self", "data:", "https:"),
                font_src = "self",
                connect_src = "self",
                object_src = "none",
                base_uri = "self",
                form_action = "self",
                frame_ancestors = "none",
                upgrade_insecure_requests = TRUE
            ),
            strict_transport_security = NULL,
            x_frame_options = "DENY"
        ) |>
        plumber2::api_statics("/static", state$dist_dir) |>
        add_auth_gate_route(config) |>
        add_download_route(state)

    if (enable_access_log) {
        api <- plumber2::api_logger(
            api,
            logger = plumber2::logger_console(),
            access_log_format = plumber2::common_log_format
        )
    }

    api$set_data("state", state)

    # Server-side sessions are keyed by the client-id cookie. In prod it is a
    # __Host--prefixed secure cookie; in dev a plain host cookie for http.
    # session_cookie_converter (NOT fiery::session_id_cookie) emits SameSite=Lax:
    # fiery hardcodes Strict/None, and Strict drops the cookie on the OIDC
    # redirect back from Auth0, breaking the state/nonce lookup in /callback.
    api$set_client_id_converter(session_cookie_converter(
        name = session_cookie_name(is_prod),
        secure = is_prod
    ))

    plumber2::api_on(api, "end", function(...) {
        try(DBI::dbDisconnect(state$con), silent = TRUE)
    })

    # In prod the only network path is Traefik, which sanitizes X-Forwarded-*
    # (only the CF-authenticated peer's values survive): trusting them makes
    # request$ip log the real client. Never in dev, where the app is directly
    # reachable and the headers are client-controlled.
    api$trust <- is_prod

    api
}

# CSV download proxy as a raw routr route: a plumber2 handler cannot emit a
# body that bypasses its negotiated serializer (spike addendum), while a raw
# handler can write the backend's bytes and headers through untouched. The
# route is added right after the auth gate, so only authenticated sessions
# reach it. The body is buffered in memory (uploads are size-capped) and
# httpuv writes it to the client off the R thread.
add_download_route <- function(api, state) {
    route <- routr::Route$new()
    route$add_handler("get", "/datasets/:id/download", function(request, response, keys, ..., arg_list) {
        id <- suppressWarnings(as.integer(keys$id))
        result <- tryCatch(
            {
                if (is.na(id)) {
                    stop(backend_error(404L, "Not Found", "no such dataset"))
                }
                be_fetch_csv(state, arg_list$datastore, id)
            },
            fe_auth_expired = function(e) e,
            fe_backend_error = function(e) e
        )
        if (inherits(result, "fe_auth_expired")) {
            response$status <- 302L
            response$set_header("Location", "/login")
            return(FALSE)
        }
        if (inherits(result, "fe_backend_error")) {
            respond_problem(response, result$status, result$title, result$detail)
            return(FALSE)
        }
        response$status <- 200L
        response$type <- result$content_type
        response$set_header("Content-Disposition", result$content_disposition)
        response$set_header("Cache-Control", "private, no-store")
        response$body <- result$body
        FALSE
    })
    plumber2::api_add_route(api, "downloads", route = route)
}

# One dedicated DBI connection for the datastore's storr driver and health check.
# NOTICE chatter (e.g. storr's create-if-not-exists) is silenced for clean logs.
# TCP keepalives let the DB/network drop of an idle connection be detected and
# reaped, rather than surfacing as a hung query later; a dead connection then
# fails the health check, which restarts the container (storr keeps a single
# persistent connection, so pool-style transparent reconnect is not available).
connect_pg <- function(pg) {
    con <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = pg$host,
        port = pg$port,
        dbname = pg$dbname,
        user = pg$user,
        password = pg$password,
        keepalives = 1L,
        keepalives_idle = 60L,
        keepalives_interval = 10L,
        keepalives_count = 5L
    )
    DBI::dbExecute(con, "SET client_min_messages TO WARNING")
    con
}

# Build the shared application state: fingerprinted assets (rebuilt in dev or when
# missing), the loaded manifest, translations, the shell template and the DB
# connection. Paths resolve relative to `base_dir` (the front root).
build_state <- function(config, base_dir = ".") {
    dist_dir <- file.path(base_dir, "dist")
    if (!dir.exists(dist_dir) || identical(config$environment, "dev")) {
        build_assets(file.path(base_dir, "assets"), dist_dir)
    }
    list(
        config = config,
        con = connect_pg(config$pg),
        manifest = yyjsonr::read_json_file(file.path(dist_dir, "manifest.json")),
        translations = load_translations(file.path(base_dir, "assets", "translations.json")),
        template = paste(
            readLines(file.path(base_dir, "assets", "templates", "shell.html"), warn = FALSE),
            collapse = "\n"
        ),
        dist_dir = normalizePath(dist_dir),
        store_tables = list(data = "fe_store_data", keys = "fe_store_keys")
    )
}
