#* Liveness/readiness check: confirms the datastore's DBI connection answers.
#* @get /health
#* @serializer json
function(response, server) {
    state <- server$get_data("state")
    healthy <- tryCatch(
        {
            DBI::dbGetQuery(state$con, "SELECT 1")
            TRUE
        },
        error = function(e) FALSE
    )
    if (!healthy) {
        response$status <- 503L
        return(list(status = "error", checks = list(datastore = "down")))
    }
    list(status = "ok", checks = list(datastore = "up"))
}
