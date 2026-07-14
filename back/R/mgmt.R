# Auth0 Management API access for the admin role endpoints. auth0r's
# Auth0Management handles client-credentials token acquisition and expiry
# refresh internally, so one client is constructed lazily (first use) and kept
# for the process lifetime. User-role lookups are cached briefly (mirrors
# shiny-base's admin cache) so rendering the admin user list does not issue one
# Management API call per user per request; role changes invalidate the entry.

ROLES_CACHE_SECONDS <- 300L

mgmt_state <- new.env(parent = emptyenv())

mgmt_configured <- function(config = app_config()) {
    nzchar(config$auth0$domain) &&
        nzchar(config$auth0$mgmt_client_id) &&
        nzchar(config$auth0$mgmt_client_secret)
}

# An injected client (tests) wins over configuration.
mgmt_available <- function(config = app_config()) {
    !is.null(mgmt_state$client) || mgmt_configured(config)
}

mgmt_client <- function(config = app_config()) {
    if (!is.null(mgmt_state$client)) {
        return(mgmt_state$client)
    }
    if (!mgmt_configured(config)) {
        reqres::abort_http_problem(503L, detail = "Auth0 management client is not configured")
    }
    mgmt_state$client <- auth0r::Auth0Management$new(
        domain = config$auth0$domain,
        client_id = config$auth0$mgmt_client_id,
        client_secret = config$auth0$mgmt_client_secret
    )
    mgmt_state$client
}

# Test seams: inject a fake client / drop all cached state.
set_mgmt_client <- function(client) {
    mgmt_state$client <- client
    invisible()
}

reset_mgmt_state <- function() {
    rm(list = ls(mgmt_state), envir = mgmt_state)
    invisible()
}

# Role names for one auth0 sub, cached for ROLES_CACHE_SECONDS.
user_roles_cached <- function(auth0_sub, config = app_config()) {
    if (is.null(mgmt_state$roles)) {
        mgmt_state$roles <- new.env(parent = emptyenv())
    }
    now <- as.numeric(Sys.time())
    hit <- mgmt_state$roles[[auth0_sub]]
    if (!is.null(hit) && (now - hit$fetched_at) < ROLES_CACHE_SECONDS) {
        return(hit$roles)
    }
    roles <- mgmt_client(config)$get_user_roles(auth0_sub)
    mgmt_state$roles[[auth0_sub]] <- list(roles = roles, fetched_at = now)
    roles
}

invalidate_user_roles <- function(auth0_sub) {
    if (!is.null(mgmt_state$roles) && !is.null(mgmt_state$roles[[auth0_sub]])) {
        rm(list = auth0_sub, envir = mgmt_state$roles)
    }
    invisible()
}
