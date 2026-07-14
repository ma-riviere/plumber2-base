# Central authentication gate: one raw routr route dispatched before every page
# route (default-deny, mirroring back's central api_auth rules). Protected
# requests without a valid session are redirected to /login - as a 302 for
# normal navigation, as 200 + HX-Redirect for htmx (htmx does not process
# response headers on 3xx, and 200 is its documented redirect pattern). For
# authenticated state-changing requests it enforces the CSRF token + Origin
# check, then stamps the per-request CSRF token on the response (cookie + data
# read by render_shell into the meta tag).

# /logout is intentionally NOT public: it is a state-changing POST that revokes
# the refresh token and destroys the session, so it must pass the gate's session
# + CSRF + Origin checks (a public GET logout is CSRFable - a cross-site
# navigation could force-logout the user).
GATE_PUBLIC_PATHS <- c("/health", "/login", "/callback", "/unverified")
GATE_PUBLIC_PREFIXES <- c("/lang/", "/static/")
GATE_UNSAFE_METHODS <- c("POST", "PUT", "PATCH", "DELETE")

is_public_path <- function(path) {
    path %in% GATE_PUBLIC_PATHS || any(startsWith(path, GATE_PUBLIC_PREFIXES))
}

# A safe local path to return to after a public GET that redirects "back" (the
# /lang switcher). The Referer is honored ONLY when it is same-origin, reduced
# to its path (+query); anything else falls back to /home. Without this the raw
# Referer becomes an open redirect (a cross-site link to /lang/x bounces the
# victim to the attacker's origin).
safe_referer_path <- function(request, app_url) {
    referer <- request$get_header("Referer") %||% ""
    if (!nzchar(referer)) {
        return("/home")
    }
    parts <- regmatches(referer, regexec("^([a-zA-Z][a-zA-Z0-9+.-]*://[^/?#]+)(/[^#]*)?$", referer))[[1]]
    if (length(parts) != 3 || !identical(parts[2], app_origin(app_url))) {
        return("/home")
    }
    path <- if (nzchar(parts[3])) parts[3] else "/"
    if (!startsWith(path, "/") || startsWith(path, "//")) {
        return("/home")
    }
    path
}

# Only local absolute paths may be used as a post-login target (no scheme, no
# protocol-relative //host), so ?next= cannot become an open redirect.
is_safe_next <- function(path) {
    is.character(path) &&
        length(path) == 1 &&
        !is.na(path) &&
        startsWith(path, "/") &&
        !startsWith(path, "//") &&
        !grepl("://", path, fixed = TRUE)
}

add_auth_gate_route <- function(api, config) {
    is_prod <- identical(config$environment, "prod")
    origin <- app_origin(config$app_url)
    gate_route <- routr::Route$new()
    gate_route$add_handler("all", "/*", function(request, response, keys, ..., arg_list) {
        if (is_public_path(request$path)) {
            return(TRUE)
        }
        datastore <- arg_list$datastore
        auth <- session_auth(datastore)
        if (is.null(auth)) {
            response$set_header("Cache-Control", "private, no-store")
            if (is_htmx_request(request)) {
                response$status <- 200L
                response$set_header("HX-Redirect", "/login")
                response$body <- raw()
            } else {
                response$status <- 302L
                target <- "/login"
                if (toupper(request$method) %in% c("GET", "HEAD") && !identical(request$path, "/")) {
                    target <- paste0("/login?next=", utils::URLencode(request$path, reserved = TRUE))
                }
                response$set_header("Location", target)
            }
            return(FALSE)
        }
        if (toupper(request$method) %in% GATE_UNSAFE_METHODS) {
            token <- request$get_header("X-CSRF-Token") %||% ""
            if (!origin_allowed(request, origin) || !verify_csrf_token(token, auth$csrf_id, csrf_key(config))) {
                respond_problem(response, 403L, "Forbidden", "CSRF validation failed")
                return(FALSE)
            }
        }
        touch_session(datastore, auth)
        csrf_token <- issue_csrf_token(auth$csrf_id, csrf_key(config))
        response$set_data("csrf_token", csrf_token)
        response$set_cookie(
            csrf_cookie_name(is_prod),
            csrf_token,
            http_only = FALSE,
            path = "/",
            secure = is_prod,
            same_site = "Lax"
        )
        TRUE
    })
    plumber2::api_add_route(api, "auth_gate", route = gate_route)
}

# Write a problem+json response directly (same rationale as back: the
# abort_* renderer cannot be used from a raw routr handler that must also stop
# dispatch by returning FALSE).
respond_problem <- function(response, status, title, detail) {
    response$status <- status
    response$type <- "application/problem+json"
    response$body <- charToRaw(yyjsonr::write_json_str(
        list(title = title, status = status, detail = detail),
        auto_unbox = TRUE
    ))
    invisible(response)
}
