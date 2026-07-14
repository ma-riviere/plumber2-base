# Auth0 OIDC client for the FE, built on auth0r (Auth0Client + Auth0Verifier).
# This file keeps the app's config-first function signatures and owns the
# process-wide client/verifier instances; all protocol logic (PKCE, form-encoded
# client_secret_post exchange, full ID-token validation, JWKS caching, rotation
# persist-before-return) lives in the package. Confidential client; both the
# client secret and the PKCE verifier are sent to the token endpoint (RFC 9700
# recommends PKCE even for confidential clients).

auth0_state <- new.env(parent = emptyenv())

auth0_base_url <- function(domain) {
    if (!nzchar(domain %||% "")) {
        return("")
    }
    base <- if (grepl("^https?://", domain)) domain else paste0("https://", domain)
    sub("/+$", "", base)
}

auth0_issuer <- function(domain) {
    paste0(auth0_base_url(domain), "/")
}

# Reset the cached verifier; the next validation rebuilds it from the config.
# (The verifier is resolved config-first at call time because this file is
# sourced into more than one environment - plumber2 evaluates the constructor
# in its own env while tests and route handlers resolve the global copy - so
# construction-time state would not be shared reliably across them.)
configure_jwks <- function(base_url) {
    auth0_state$verifier_key <- NULL
    auth0_state$verifier <- NULL
    invisible()
}

# Tests inject a fixture fetcher (returning list(keys, max_age)); NULL resets
# to the real HTTP fetcher.
set_jwks_fetcher <- function(fetcher = NULL) {
    auth0_state$fixture_fetcher <- fetcher
    auth0_state$verifier_key <- NULL
    auth0_state$verifier <- NULL
    invisible()
}

# One long-lived Auth0Verifier per tenant (its JWKS cache must survive across
# requests); rebuilt when the config's tenant or the fixture fetcher changes.
# An empty domain (bypass mode) yields a reject-everything verifier.
app_verifier <- function(config) {
    base_url <- auth0_base_url(config$auth0$domain)
    key <- paste0(base_url, "|", !is.null(auth0_state$fixture_fetcher))
    if (!identical(auth0_state$verifier_key, key)) {
        auth0_state$verifier <- auth0r::Auth0Verifier$new(
            domain = base_url,
            jwks_fetcher = auth0_state$fixture_fetcher
        )
        auth0_state$verifier_key <- key
    }
    auth0_state$verifier
}

# The token-endpoint timeout is kept tight: the refresh grant runs on the hot
# path (a near-expiry access token blocks the whole single-threaded FE while
# the call is in flight), and a hung endpoint must not stall every other
# request for long.
AUTH0_TOKEN_TIMEOUT_SECONDS <- 5

# One Auth0Client per (domain, client_id); rebuilt when the config changes
# (test apps vary the fake tenant URL between builds).
app_auth0_client <- function(config) {
    key <- paste0(config$auth0$domain, "|", config$auth0$client_id)
    if (!identical(auth0_state$client_key, key)) {
        auth0_state$client <- auth0r::Auth0Client$new(
            domain = config$auth0$domain,
            client_id = config$auth0$client_id,
            client_secret = config$auth0$client_secret,
            timeout = AUTH0_TOKEN_TIMEOUT_SECONDS
        )
        auth0_state$client_key <- key
    }
    auth0_state$client
}

random_url_token <- auth0r::oauth_random_token

pkce_challenge <- auth0r::pkce_challenge

# offline_access requests the rotating refresh token; the API must have "Allow
# Offline Access" enabled in Auth0.
build_authorize_url <- function(config, state, nonce, challenge) {
    app_auth0_client(config)$authorize_url(
        redirect_uri = paste0(config$app_url, "/callback"),
        scope = "openid profile email offline_access",
        state = state,
        nonce = nonce,
        audience = config$auth0$audience,
        code_challenge = challenge
    )
}

exchange_code <- function(config, code, verifier) {
    app_auth0_client(config)$exchange_code(code, paste0(config$app_url, "/callback"), verifier = verifier)
}

refresh_access <- function(config, refresh_token) {
    app_auth0_client(config)$refresh_tokens(refresh_token)
}

# Best-effort at logout; the server-side session is destroyed regardless.
revoke_refresh_token <- function(config, refresh_token) {
    app_auth0_client(config)$revoke_refresh_token(refresh_token)
}

# Full OIDC validation (signature via tenant JWKS, exp/iat + freshness, iss,
# aud/azp, nonce echo); see auth0r::Auth0Verifier. Returns the claims or stops
# with a short reason (never echoing token contents).
validate_id_token <- function(id_token, config, expected_nonce, leeway = 60, max_age = 600) {
    app_verifier(config)$verify_id_token(
        id_token,
        client_id = config$auth0$client_id,
        nonce = expected_nonce,
        leeway = leeway,
        max_token_age = max_age
    )
}

# --- access-token freshness --------------------------------------------------

# Classed condition raised when the session can no longer produce a valid BE
# access token; callers map it to a forced re-login.
auth_expired <- function(message = "session authentication expired") {
    structure(class = c("fe_auth_expired", "error", "condition"), list(message = message, call = NULL))
}

# Return a BE access token that stays valid for at least `refresh_window`
# seconds, refreshing (and persisting the rotated refresh token BEFORE
# returning, via auth0r::ensure_fresh_tokens) when needed. Guest sessions carry
# no token and return NULL (the BE bypass guard covers them). Serialization:
# the R process is single-threaded and the refresh call blocks, so two
# near-expiry requests run strictly sequentially. On any expiry (refresh
# rejected by rotation reuse detection, nothing stored) the session is
# destroyed and the app-level fe_auth_expired condition forces a re-login.
ensure_fresh_access_token <- function(datastore, config, refresh_window = 60) {
    read_tokens <- function() {
        auth <- datastore$session$auth
        if (is.null(auth)) {
            return(NULL)
        }
        list(
            access_token = auth$access_token,
            access_expires_at = auth$access_expires_at,
            refresh_token = if (!is.null(auth$refresh_token_enc)) {
                decrypt_secret(auth$refresh_token_enc, refresh_key(config))
            }
        )
    }
    write_tokens <- function(tokens) {
        auth <- datastore$session$auth
        auth$access_token <- tokens$access_token
        auth$access_expires_at <- tokens$access_expires_at
        if (!is.null(tokens$refresh_token)) {
            auth$refresh_token_enc <- encrypt_secret(tokens$refresh_token, refresh_key(config))
        }
        # Only reached on an actual refresh: the fresh access token can carry a
        # changed roles claim, so drop the cached /v1/me scope grant - the UI
        # re-syncs with a role change within one token TTL (<=15 min).
        auth[["scopes"]] <- NULL
        auth[["scopes_failed_at"]] <- NULL
        datastore$session$auth <- auth
    }
    tryCatch(
        auth0r::ensure_fresh_tokens(app_auth0_client(config), read_tokens, write_tokens, refresh_window),
        auth0r_auth_expired = function(e) {
            destroy_auth_session(datastore)
            stop(auth_expired(conditionMessage(e)))
        }
    )
}
