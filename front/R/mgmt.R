# Auth0 Management API access, used only to persist nickname edits back to the
# user's Auth0 profile. Uses a dedicated M2M application with minimal scopes
# (read:users update:users). auth0r's Auth0Management owns token acquisition
# and expiry, timeouts, retries and path encoding; this file keeps one lazy
# client per (domain, mgmt client) pair - fine for one process (move to the
# datastore if the FE is ever replicated).

mgmt_cache <- new.env(parent = emptyenv())

mgmt_client <- function(config) {
    key <- paste0(config$auth0$domain, "|", config$auth0$mgmt_client_id)
    if (!identical(mgmt_cache$key, key)) {
        mgmt_cache$client <- auth0r::Auth0Management$new(
            domain = config$auth0$domain,
            client_id = config$auth0$mgmt_client_id,
            client_secret = config$auth0$mgmt_client_secret
        )
        mgmt_cache$key <- key
    }
    mgmt_cache$client
}

# Test seam: drop the cached client (e.g. between webfakes servers).
reset_mgmt_cache <- function() {
    rm(list = ls(mgmt_cache), envir = mgmt_cache)
    invisible()
}

mgmt_update_nickname <- function(config, sub, nickname) {
    mgmt_client(config)$update_user(sub, nickname = nickname)
    invisible()
}
