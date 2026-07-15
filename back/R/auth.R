# Authentication for back: RFC 9068 access-token verification via
# auth0r::Auth0Verifier (manual RS256 - jose cannot decode typ at+jwt), the
# fireproof guards built on it, and the in-handler scope/principal helpers
# (fireproof's auth_scope is broken, spike finding 1).
#
# Guards deliberately DO NOT reuse fireproof's session-cached authentication:
# stock GuardBearer/GuardKey skip re-validation when the firesale session (keyed
# by the fiery client-id cookie) already holds user info, which would let a
# cookie-replaying client outlive token expiry or key revocation. auth0r's
# fireproof_jwt_guard and the RevalidatingKey subclass below clear that cache
# before delegating, forcing full validation on every request (stateless API).
# The per-request write that remains is the principal handoff read by handlers
# via current_principal().

# --- token verification ---------------------------------------------------------

# One process-wide verifier (plus the api-key handoff, see api_key_guard).
auth_state <- new.env(parent = emptyenv())

# Wire the verifier to a tenant. An empty domain (dev without Auth0) creates a
# reject-everything verifier: only the bypass/api_key guards can authenticate.
configure_jwks <- function(domain) {
    auth_state$jwks_domain <- domain %||% ""
    auth_state$verifier <- auth0r::Auth0Verifier$new(domain = auth_state$jwks_domain)
    invisible()
}

# Tests inject a fixture fetcher (returning list(keys, max_age)) against the
# configured test tenant; NULL resets to a reject-everything verifier.
set_jwks_fetcher <- function(fetcher = NULL) {
    auth_state$verifier <- if (is.null(fetcher)) {
        auth0r::Auth0Verifier$new(domain = "")
    } else {
        auth0r::Auth0Verifier$new(domain = auth_state$jwks_domain %||% "", jwks_fetcher = fetcher)
    }
    invisible()
}

app_verifier <- function() {
    auth_state$verifier %||% auth0r::Auth0Verifier$new(domain = "")
}

# --- guards --------------------------------------------------------------------

# GuardKey that clears the firesale-cached user info before checking, so
# authentication is re-validated on every request (see file header). The
# Bearer equivalent ships with auth0r (fireproof_jwt_guard).
RevalidatingKey <- R6::R6Class(
    "RevalidatingKey",
    inherit = fireproof::GuardKey,
    public = list(
        check_request = function(request, response, keys, ..., .datastore) {
            .datastore$session$fireproof[[private$NAME]] <- NULL
            super$check_request(request, response, keys, ..., .datastore = .datastore)
        }
    )
)

# Dev-only guard: authenticates every request as the guest principal with the
# default user scopes. Only registered (and only referenced by auth flows) when
# BYPASS_AUTH is set, which the startup assertion forbids in prod, so it does not
# exist in a production router. reject_response is a no-op because this guard
# never fails; the base class would unconditionally overwrite the real guards'
# 401/403 with a 400.
BypassGuard <- R6::R6Class(
    "BypassGuard",
    inherit = fireproof::Guard,
    public = list(
        initialize = function(scopes, name = NULL) {
            super$initialize(name = name)
            private$SCOPES <- scopes
        },
        check_request = function(request, response, keys, ..., .datastore) {
            .datastore$session$fireproof[[private$NAME]] <- fireproof::new_user_info(
                provider = "bypass",
                id = "guest",
                name_user = "guest",
                scopes = private$SCOPES
            )
            TRUE
        },
        reject_response = function(response, scope, ..., .datastore) {
            invisible(NULL)
        }
    ),
    active = list(
        # plumber2's ignite runs fireproof::prune_openapi over every registered
        # scheme and requires a $type; the base Guard returns an empty list,
        # which crashes doc generation. Bypass needs no credential, so this is
        # documentation-only (and dev-only, like the guard itself).
        open_api = function() {
            list(
                type = "apiKey",
                `in` = "header",
                name = "X-Bypass-Unused",
                description = paste(
                    "Dev-only BYPASS_AUTH mode: requests are authenticated",
                    "as the guest user, no credential needed."
                )
            )
        }
    ),
    private = list(SCOPES = character())
)

# Failure reasons only, never token material; silenced by tests via the option.
log_auth_reject <- function(reason) {
    if (!isTRUE(getOption("back.quiet_auth_log"))) {
        cat(sprintf("[back] jwt rejected: %s\n", reason), file = stderr())
    }
    invisible()
}

# Bearer guard validating Auth0 access tokens (auth0r fireproof guard, fed the
# verifier through an accessor so tests can swap in fixture JWKS after the api
# is built). Requires the email_verified custom claim (the FE gate alone is
# bypassable by calling the BE directly) and grants scopes mapped from the
# roles custom claim.
jwt_guard <- function(config, permissions) {
    namespace <- config$auth0$claim_namespace
    auth0r::fireproof_jwt_guard(
        verifier = app_verifier,
        audience = config$auth0$audience,
        # The Auth0 API is provisioned with token_dialect rfc9068_profile
        # (deploy/auth0/provision.R), so client_id + jti presence is enforced.
        profile = "rfc9068",
        scopes_from_claims = function(claims) {
            if (!isTRUE(claims[[paste0(namespace, "email_verified")]])) {
                log_auth_reject("email not verified")
                return(FALSE)
            }
            scopes_for_roles(claims[[paste0(namespace, "roles")]] %||% character(), permissions)
        },
        user_info_from_claims = function(claims) {
            fireproof::new_user_info(
                provider = "auth0",
                id = claims$sub,
                roles = claims[[paste0(namespace, "roles")]] %||% character()
            )
        },
        on_reject = log_auth_reject
    )
}

# X-API-Key guard: constant-time hash check against Postgres, granting the key's
# stored scopes. validate/user_info run back-to-back inside one check_request on
# the single R thread, so the record handoff through auth_state is race-free.
api_key_guard <- function() {
    RevalidatingKey$new(
        key_name = "X-API-Key",
        cookie = FALSE,
        validate = function(key, request, response) {
            record <- lookup_api_key(app_pool(), key)
            if (is.null(record)) {
                return(FALSE)
            }
            auth_state$key_record <- record
            touch_api_key(app_pool(), record$id)
            record$scopes
        },
        user_info = function(key) {
            record <- auth_state$key_record
            auth_state$key_record <- NULL
            fireproof::new_user_info(
                provider = "api_key",
                id = as.character(record$user_id),
                key_id = record$id,
                key_name = record$name
            )
        }
    )
}

# --- principal + scope helpers -------------------------------------------------

# The authenticated principal of the current request, read from where the guards
# stored it. Guard order = flow order (api_key || jwt || bypass).
current_principal <- function(datastore) {
    for (guard in c("api_key", "jwt", "bypass")) {
        info <- datastore$session$fireproof[[guard]]
        if (inherits(info, "fireproof_user_info")) {
            return(list(guard = guard, info = info, scopes = info$scopes %||% character()))
        }
    }
    # Unreachable behind the /v1/* auth rules; defensive for misconfigured routes.
    reqres::abort_http_problem(401L, detail = "authentication required")
}

# In-handler scope enforcement (fireproof auth_scope is broken, spike finding 1).
# Denials follow RFC 6750: 403 + WWW-Authenticate error="insufficient_scope" +
# problem+json body. The challenge header cannot survive an abort_* unwind (the
# problem renderer clears headers), so the problem is written directly and, as
# with respond_problem, the CALLER MUST RETURN the non-TRUE result, which is
# plumber2::Break, instead of continuing the handler.
require_scope <- function(datastore, response, needed) {
    granted <- current_principal(datastore)$scopes
    missing <- setdiff(needed, granted)
    if (length(missing)) {
        return(auth0r::write_bearer_problem(
            response,
            403L,
            error = "insufficient_scope",
            scope = needed,
            detail = paste0("missing scope(s): ", paste(missing, collapse = ", "))
        ))
    }
    invisible(TRUE)
}

# Principal + backing user row, resolved once per request and cached on the
# response (also read by the request logger). Aborts 500 when no user backs the
# credential (a key whose user vanished mid-request).
request_principal <- function(datastore, response) {
    cached <- response$get_data("principal_ids")
    if (!is.null(cached)) {
        return(cached)
    }
    principal <- current_principal(datastore)
    user <- principal_user(app_pool(), principal)
    if (is.null(user)) {
        reqres::abort_http_problem(500L, detail = "no user record backs this credential")
    }
    resolved <- list(
        guard = principal$guard,
        scopes = principal$scopes,
        user = user,
        user_id = as.integer(user$id),
        api_key_id = if (identical(principal$guard, "api_key")) as.integer(principal$info$key_id)
    )
    response$set_data("principal_ids", resolved)
    resolved
}
