# CSRF protection: signed double-submit (OWASP-recommended variant). The token
# is nonce.HMAC(key, csrf_id.nonce) where csrf_id is a per-session random value
# stored server-side (deviation from "HMAC over the session id": the fiery
# client id is not exposed to handlers, and a session-bound random id gives the
# same binding). Delivered in a non-HttpOnly cookie AND the shell's meta tag;
# app.js attaches it as X-CSRF-Token on every htmx request. State-changing
# requests additionally require a same-origin Origin/Referer header.

csrf_cookie_name <- function(is_prod) {
    if (is_prod) "__Host-csrf" else "fb_csrf"
}

csrf_key <- function(config) {
    derive_key(config$session_key, "csrf")
}

issue_csrf_token <- function(csrf_id, key) {
    nonce <- sodium::bin2hex(sodium::random(16))
    paste0(nonce, ".", csrf_signature(csrf_id, nonce, key))
}

csrf_signature <- function(csrf_id, nonce, key) {
    as.character(openssl::sha256(charToRaw(paste0(csrf_id, ".", nonce)), key = key))
}

verify_csrf_token <- function(token, csrf_id, key) {
    if (!is.character(token) || length(token) != 1 || !nzchar(token %||% "")) {
        return(FALSE)
    }
    parts <- strsplit(token, ".", fixed = TRUE)[[1]]
    if (length(parts) != 2 || !all(nzchar(parts))) {
        return(FALSE)
    }
    expected <- csrf_signature(csrf_id, parts[1], key)
    constant_time_equal(charToRaw(expected), charToRaw(parts[2]))
}

# Compares every byte regardless of where a mismatch occurs.
constant_time_equal <- function(a, b) {
    if (length(a) != length(b)) {
        return(FALSE)
    }
    sum(bitwXor(as.integer(a), as.integer(b))) == 0L
}

# --- origin check ------------------------------------------------------------

app_origin <- function(app_url) {
    parsed <- httr2::url_parse(app_url)
    paste0(parsed$scheme, "://", parsed$hostname, if (!is.null(parsed$port)) paste0(":", parsed$port))
}

# Browsers send Origin on state-changing requests; when absent, Referer is the
# fallback. Neither present -> reject (fail closed; every legitimate htmx
# request in this app carries at least one).
origin_allowed <- function(request, origin) {
    from <- request$get_header("Origin")
    if (!is.null(from) && nzchar(from)) {
        return(identical(from, origin))
    }
    referer <- request$get_header("Referer")
    if (!is.null(referer) && nzchar(referer)) {
        return(identical(referer, origin) || startsWith(referer, paste0(origin, "/")))
    }
    FALSE
}
