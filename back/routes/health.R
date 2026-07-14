#* Liveness + database check for Docker/Traefik. No auth, no /v1 prefix.
#* Returns 200 {"status":"ok"} when the pool answers SELECT 1, else a 503
#* problem+json. app_pool()/db_healthcheck() resolve from the constructor's
#* environment (see constructor.R).
#* @get /health
#* @serializer json
function() {
    if (db_healthcheck(app_pool())) {
        list(status = jsonlite::unbox("ok"))
    } else {
        reqres::abort_http_problem(
            503,
            detail = "database is not reachable",
            title = "Service Unavailable"
        )
    }
}
