# In-app rate limiting: token buckets in a package-level environment, keyed by
# principal (api-key id / JWT sub / guest). Valid while replicas == 1 (the
# deployment plan); Redis/PG is the documented upgrade path if that changes.
#
# One enforcement route, dispatched AFTER the auth guards (fireproof inserts
# its route FIRST in the stack regardless of attach order, so a pre-auth route
# is impossible - proven, see spike addendum). Accepted consequences:
# unauthenticated/failed-auth floods
# are answered by the guards before any in-app limiter can see them, and
# IP-level flood control belongs to the edge (Cloudflare rate rule + Traefik).
#
# The route applies: the per-principal quota, stricter buckets for the
# expensive endpoints (upload, model fit), and the upload Content-Length
# precheck (rejected BEFORE the multipart parser buffers the body). On limit:
# 429 + Retry-After + RateLimit-* headers; the headers are also emitted on
# success. The 429 response is written manually (NOT abort_http_problem: the
# problem renderer drops previously set headers, so RateLimit-*/Retry-After
# would vanish). Limiter errors fail open.

rate_state <- new.env(parent = emptyenv())
rate_state$buckets <- new.env(parent = emptyenv())

reset_rate_limits <- function() {
    rate_state$buckets <- new.env(parent = emptyenv())
    invisible()
}

# Classic token bucket: capacity == limit_per_min, continuous refill.
take_rate_token <- function(key, limit_per_min, now = Sys.time()) {
    bucket <- rate_state$buckets[[key]]
    if (is.null(bucket)) {
        bucket <- list(tokens = limit_per_min, updated = now)
    }
    elapsed <- as.numeric(difftime(now, bucket$updated, units = "secs"))
    tokens <- min(limit_per_min, bucket$tokens + elapsed * limit_per_min / 60)
    allowed <- tokens >= 1
    if (allowed) {
        tokens <- tokens - 1
    }
    rate_state$buckets[[key]] <- list(tokens = tokens, updated = now)
    list(
        allowed = allowed,
        limit = limit_per_min,
        remaining = max(0L, as.integer(floor(tokens))),
        # Seconds until a token is available (0 when allowed).
        reset = if (allowed) 0L else as.integer(ceiling((1 - tokens) * 60 / limit_per_min))
    )
}

# Apply one bucket: stamp the RateLimit headers; on an empty bucket write the
# 429 problem response directly and return FALSE (stop dispatch). Fail open.
enforce_rate_limit <- function(response, key, limit_per_min) {
    outcome <- tryCatch(take_rate_token(key, limit_per_min), error = function(e) NULL)
    if (is.null(outcome)) {
        return(TRUE)
    }
    response$set_header("RateLimit-Limit", as.character(outcome$limit))
    response$set_header("RateLimit-Remaining", as.character(outcome$remaining))
    response$set_header("RateLimit-Reset", as.character(outcome$reset))
    if (outcome$allowed) {
        return(TRUE)
    }
    response$set_header("Retry-After", as.character(max(1L, outcome$reset)))
    respond_problem(response, 429L, "Too Many Requests", "rate limit exceeded, slow down")
    FALSE
}

# Drop buckets untouched for max_idle_secs (an idle bucket has refilled and
# carries no state worth keeping): bounds memory across many distinct
# principals in a long-lived process. Called from the maintenance tick.
sweep_rate_buckets <- function(max_idle_secs = 900, now = Sys.time()) {
    for (key in ls(rate_state$buckets)) {
        bucket <- rate_state$buckets[[key]]
        if (as.numeric(difftime(now, bucket$updated, units = "secs")) > max_idle_secs) {
            rm(list = key, envir = rate_state$buckets)
        }
    }
    invisible()
}

# The rate-limit key for an authenticated request: the credential identity.
principal_rate_key <- function(datastore, request) {
    for (guard in c("api_key", "jwt", "bypass")) {
        info <- datastore$session$fireproof[[guard]]
        if (inherits(info, "fireproof_user_info")) {
            return(paste0(guard, ":", info$id %||% "unknown"))
        }
    }
    paste0("ip:", request$ip %||% "unknown")
}

request_content_length <- function(request) {
    raw <- request$headers$content_length %||%
        request$origin$CONTENT_LENGTH %||%
        request$origin$HTTP_CONTENT_LENGTH
    if (is.null(raw)) {
        return(NA_real_)
    }
    suppressWarnings(as.numeric(raw))
}

add_principal_limits_route <- function(api, config) {
    limits_route <- routr::Route$new()
    limits_route$add_handler("all", "/v1/*", function(request, response, keys, ..., arg_list) {
        datastore <- arg_list$datastore
        # Authenticated API responses must never be cached by an intermediary.
        response$set_header("Cache-Control", "no-store")
        key <- principal_rate_key(datastore, request)
        method <- tolower(request$method)

        if (method == "post" && request$path == "/v1/datasets") {
            content_length <- request_content_length(request)
            if (!is.na(content_length) && content_length > config$max_upload_bytes) {
                reqres::abort_http_problem(
                    413L,
                    detail = sprintf("upload exceeds the %d byte limit", config$max_upload_bytes)
                )
            }
            return(enforce_rate_limit(response, paste0("upload:", key), config$rate_limit_uploads_per_min))
        }
        if (method == "post" && request$path == "/v1/models") {
            return(enforce_rate_limit(response, paste0("fit:", key), config$rate_limit_fits_per_min))
        }
        enforce_rate_limit(response, key, config$rate_limit_per_min)
    })
    plumber2::api_add_route(api, "limits", route = limits_route)
}
