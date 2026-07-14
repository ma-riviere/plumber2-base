# Auth0 Management API client, used only to persist nickname edits back to the
# user's Auth0 profile. Uses a dedicated M2M application with minimal scopes
# (read:users update:users). The client-credentials token is cached in memory
# until shortly before expiry - fine for one process (move to the datastore if
# the FE is ever replicated).

mgmt_cache <- new.env(parent = emptyenv())

mgmt_token <- function(config) {
    now <- as.numeric(Sys.time())
    if (!is.null(mgmt_cache$token) && (mgmt_cache$expires_at - now) > 60) {
        return(mgmt_cache$token)
    }
    resp <- httr2::request(paste0(auth0_base_url(config$auth0$domain), "/oauth/token")) |>
        httr2::req_timeout(10) |>
        httr2::req_body_form(
            grant_type = "client_credentials",
            client_id = config$auth0$mgmt_client_id,
            client_secret = config$auth0$mgmt_client_secret,
            audience = paste0(auth0_base_url(config$auth0$domain), "/api/v2/")
        ) |>
        httr2::req_perform() |>
        be_parse_json()
    mgmt_cache$token <- resp$access_token
    mgmt_cache$expires_at <- now + (resp$expires_in %||% 3600)
    mgmt_cache$token
}

# Test seam: drop the cached token (e.g. between webfakes servers).
reset_mgmt_cache <- function() {
    rm(list = ls(mgmt_cache), envir = mgmt_cache)
    invisible()
}

mgmt_update_nickname <- function(config, sub, nickname) {
    httr2::request(paste0(
        auth0_base_url(config$auth0$domain),
        "/api/v2/users/",
        utils::URLencode(sub, reserved = TRUE)
    )) |>
        httr2::req_method("PATCH") |>
        httr2::req_auth_bearer_token(mgmt_token(config)) |>
        httr2::req_timeout(10) |>
        httr2::req_body_raw(
            yyjsonr::write_json_str(list(nickname = nickname), auto_unbox = TRUE),
            type = "application/json"
        ) |>
        httr2::req_perform()
    invisible()
}
