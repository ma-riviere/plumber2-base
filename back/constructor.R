# _server.yml constructor: assembles the base plumber2 API. It runs FIRST, before
# any route file is parsed, and is `source()`d into the global environment. That
# placement is deliberate: route-file handlers are evaluated in a child of this
# environment, so the helpers sourced here and the shared app_state are visible to
# them by lexical scope. All env-var-driven, programmatic setup lives here; route
# files hold only endpoints.

for (helper in c(
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
)) {
    source(file.path("R", helper), local = FALSE)
}

# JSON via yyjsonr instead of the jsonlite-based reqres defaults. Registering
# under the same name overwrites the registry entry; this must run before any
# route file is parsed (endpoints resolve serializers from the registry at parse
# time). yyjsonr honors jsonlite's "scalar" class, so the explicit unbox()
# markers in route returns keep working. Wire deltas vs the jsonlite defaults:
# full float precision (no digits = 4 rounding) and R NULL -> null (not {}).
plumber2::register_serializer(
    "json",
    function(...) {
        opts <- yyjsonr::opts_write_json(...)
        function(x) yyjsonr::write_json_str(x, opts = opts)
    },
    mime_type = "application/json"
)
# Handlers expect request bodies as plain named lists (body$formula,
# unlist(body$scopes)): disable yyjsonr's default data.frame promotion so
# shapes match what the jsonlite parser produced.
plumber2::register_parser(
    "json",
    function(...) {
        opts <- yyjsonr::opts_read_json(arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE, ...)
        function(raw, directives) yyjsonr::read_json_raw(raw, opts = opts)
    },
    mime_types = c("application/json", "text/json")
)

# Reads env vars and runs the prod safety assertions once, at build time.
config <- get_config()
permissions <- load_permissions("permissions.yaml")
configure_jwks(config$auth0$domain)
set_app_config(config)
set_app_permissions(permissions)

# ISO 8601 UTC timestamp; proves the /v1 root is live. Scalars are unboxed so the
# JSON serializer emits "pong": "..." rather than a one-element array.
ping_handler <- function() {
    list(pong = jsonlite::unbox(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
}

# Lifecycle hooks fire only on a real api_run(): open the pool and the mirai
# worker pool at start, tear both down at end. Two daemons is plenty for the async
# work planned later (spike finding 5).
#
# The not-found fallback is added here, not at construction, on purpose: ignite
# registers the OpenAPI/docs route (which serves /openapi.json and /__docs__/*)
# just before firing "start", so adding the fallback now guarantees it is
# dispatched LAST, after that route. Adding it earlier would 404 the docs spec.
# In-process tests (which never ignite) add it via the test helper instead.
on_start <- function(server, ...) {
    add_fallback_route(server)
    mirai::daemons(2L)
    set_app_pool(db_pool(config))
    # Jobs still live in the table were orphaned by a restart; fail them so
    # polling clients terminate cleanly.
    db_recover_stale_jobs(app_pool())
    # Hourly tick: request_log retention pruning + rate-bucket sweep.
    schedule_maintenance(config)
    invisible()
}

# The rapidoc docs page cannot run under the strict API CSP: its HTML boots the
# web component through inline <script> (spec URL is computed in JS) and the
# component styles its shadow DOM inline. This handler, dispatched before the
# ignite-time docs route, relaxes the policy for the docs paths only - a static
# OpenAPI rendering with no user input, so the relaxation is contained.
DOCS_CSP <- paste(
    "default-src 'self'; script-src 'self' 'unsafe-inline';",
    "style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self' data:;",
    "object-src 'none'; base-uri 'self'; form-action 'self'; frame-ancestors 'none'"
)

add_docs_csp_route <- function(api) {
    docs_route <- routr::Route$new()
    relax_csp <- function(request, response, keys, ...) {
        response$set_header("Content-Security-Policy", DOCS_CSP)
        TRUE
    }
    for (path in c("/__docs__", "/__docs__/", "/__docs__/*")) {
        docs_route$add_handler("get", path, relax_csp)
    }
    plumber2::api_add_route(api, "docs_csp", route = docs_route)
}

on_end <- function(...) {
    pool <- app_pool()
    if (!is.null(pool) && pool::dbIsValid(pool)) {
        pool::poolClose(pool)
    }
    set_app_pool(NULL)
    mirai::daemons(0L)
    invisible()
}

# /v1 prefix for the programmatic route: annotation @root is broken in plumber2
# 0.2.0 (spike finding 7). Annotation route files write full /v1/... paths
# instead, which also keeps the generated OpenAPI paths accurate.
v1_route <- routr::Route$new()
v1_route$root <- "/v1"

# storr::driver_environment() is intentional: back keeps no server-side
# session state (guards re-validate every request, see R/auth.R), but fireproof
# requires a datastore attached before its guards. The short max_age just keeps
# the per-client principal entries from accumulating.
#
# strict_transport_security = NULL disables HSTS at the app. With HSTS enabled,
# firesafety installs an HTTP->HTTPS 308 redirect that would break every request
# reaching the app as plain HTTP behind the TLS-terminating proxy (Traefik /
# Cloudflare). TLS and HSTS are the edge's responsibility.
# Route dispatch order: fireproof's auth route is ALWAYS first (it inserts
# itself at the head of the stack), then the rate-limit + upload-size route,
# then the endpoint routes (parsed from route files after this constructor),
# then the not-found fallback (added at start, after the ignite-time docs
# route).
pa <- plumber2::api() |>
    plumber2::api_datastore(storr::driver_environment(), max_age = 900) |>
    plumber2::api_auth_guard(jwt_guard(config, permissions), "jwt") |>
    plumber2::api_auth_guard(api_key_guard(), "api_key") |>
    add_principal_limits_route(config) |>
    add_docs_csp_route() |>
    plumber2::api_security_headers(
        # Strict API policy: responses are JSON (never rendered as a document),
        # so nothing may load or frame them. The docs page overrides this (see
        # add_docs_csp_route above).
        content_security_policy = plumber2::csp(
            default_src = "none",
            frame_ancestors = "none",
            base_uri = "none",
            form_action = "none"
        ),
        strict_transport_security = NULL,
        x_frame_options = "DENY"
    ) |>
    plumber2::api_logger(
        logger = plumber2::logger_console(),
        access_log_format = plumber2::combined_log_format
    ) |>
    plumber2::api_doc_add(
        plumber2::openapi(
            info = plumber2::openapi_info(
                title = "back",
                version = "0.1.0",
                description = "JSON REST API for the plumber-base app (datasets, models, users, admin)."
            )
        )
    ) |>
    plumber2::api_add_route("v1", route = v1_route) |>
    plumber2::api_get(
        "/ping",
        ping_handler,
        route = "v1",
        serializers = plumber2::get_serializers("json")
    ) |>
    plumber2::api_on("start", on_start) |>
    plumber2::api_on("end", on_end) |>
    plumber2::api_on("after-request", log_request)

# Central auth rules, default-deny: EVERY /v1 path requires authentication (an
# unmatched /v1 path yields 401 before the 404 fallback, deliberately not leaking
# path existence), and key management is JWT-only (an API key must never mint or
# revoke keys). routr dispatches the most specific matching rule, so /v1/keys*
# overrides /v1/*. Scope checks live in handlers (require_scope).
#
# The bypass guard exists ONLY when BYPASS_AUTH is set (never in prod, enforced
# by the startup assertion): in a production router the guard and its flows are
# simply absent.
if (config$bypass_auth) {
    pa <- pa |>
        plumber2::api_auth_guard(BypassGuard$new(scopes_for_roles(character(), permissions)), "bypass") |>
        plumber2::api_auth("all", "/v1/*", auth_flow = api_key || jwt || bypass) |>
        plumber2::api_auth("all", "/v1/keys", auth_flow = jwt || bypass) |>
        plumber2::api_auth("all", "/v1/keys/*", auth_flow = jwt || bypass)
} else {
    pa <- pa |>
        plumber2::api_auth("all", "/v1/*", auth_flow = api_key || jwt) |>
        plumber2::api_auth("all", "/v1/keys", auth_flow = jwt) |>
        plumber2::api_auth("all", "/v1/keys/*", auth_flow = jwt)
}

# In prod the only network path is Traefik, which sanitizes X-Forwarded-* (only
# the CF-authenticated peer's values survive): trusting them makes request$ip
# log the real client. Never in dev, where the app is directly reachable and
# the headers are client-controlled.
pa$trust <- identical(config$environment, "prod")

pa
