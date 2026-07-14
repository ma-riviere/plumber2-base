# RFC 7807 problem responses for back.
#
# reqres/plumber2 already emit application/problem+json for every abort_*() call,
# so no wrapper helpers are needed (each abort site passes its own detail string).
# The one gap is a request that matches no route: plumber2's default is a bare 404
# with an empty text/plain body. add_fallback_route() closes that gap.
#
# It MUST be added LAST, after every other route. The handler is a raw routr
# handler on purpose: it reads response$status directly, which stays 404 until
# some earlier route claims the request. The plumber2 handler wrappers would flip
# that status to 200 before we could read it.
#
# Route order vs the docs route: Plumber2$ignite() adds the "openapi" route to the
# request router BEFORE firing the "start" event, so a fallback added in an
# api_on("start") hook dispatches after it (the entrypoint path). The docs
# handlers return FALSE on a match (dispatch stops), so requests they serve never
# reach the fallback at all; anything that does reach it with a 404 is genuinely
# unmatched. In-process tests never ignite (no docs route) and add the fallback
# directly on the assembled api.

# Write a problem+json response directly (status, type, raw body) instead of
# aborting. Needed when response headers must survive: the abort_* problem
# renderer drops headers set before the abort (proven; spike addendum). The
# handler must return plumber2::Break after calling this.
respond_problem <- function(response, status, title, detail) {
    response$status <- status
    response$type <- "application/problem+json"
    response$body <- charToRaw(yyjsonr::write_json_str(
        list(title = title, status = status, detail = detail),
        auto_unbox = TRUE
    ))
    invisible(response)
}

add_fallback_route <- function(api) {
    route <- routr::Route$new()
    route$add_handler("all", "/*", function(request, response, keys, ...) {
        if (response$status == 404L) {
            reqres::abort_not_found("no endpoint matches this path")
        }
        TRUE
    })
    plumber2::api_add_route(api, "fallback", route = route)
}
