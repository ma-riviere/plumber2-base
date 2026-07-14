# Request logging into the request_log table (admin stats + per-key usage),
# wired as an after-request event handler. Fire-and-forget: a logging failure
# never affects the response. High-frequency paths (health checks, job polling)
# and the docs are excluded per the plan; query strings and headers are never
# logged (they may carry tokens).

REQUEST_LOG_SERVICE <- "back"

should_log_request <- function(path) {
    if (path == "/health") {
        return(FALSE)
    }
    if (startsWith(path, "/__docs__") || path == "/openapi.json") {
        return(FALSE)
    }
    if (startsWith(path, "/v1/jobs/")) {
        return(FALSE)
    }
    TRUE
}

# after-request handler signature: (server, id, request, response).
log_request <- function(server, id, request, response, ...) {
    pool <- app_pool()
    if (is.null(pool) || !should_log_request(request$path)) {
        return(invisible())
    }
    ids <- request$response$get_data("principal_ids")
    try(
        DBI::dbExecute(
            pool,
            "INSERT INTO request_log (service, method, path, status, user_id, api_key_id, duration_ms)
             VALUES ($1, $2, $3, $4, $5, $6, $7)",
            params = list(
                REQUEST_LOG_SERVICE,
                toupper(request$method),
                request$path,
                as.integer(request$response$status),
                ids$user_id %||% NA,
                ids$api_key_id %||% NA,
                as.integer(round((request$duration %||% 0) * 1000))
            )
        ),
        silent = TRUE
    )
    invisible()
}

# Retention pruning, called from the maintenance tick (R/maintenance.R). The ts
# index keeps the delete cheap; returns the number of rows removed.
db_prune_request_log <- function(pool, retention_days = 30L) {
    DBI::dbExecute(
        pool,
        "DELETE FROM request_log WHERE ts < now() - make_interval(days => $1)",
        params = list(as.integer(retention_days))
    )
}

# --- admin queries ---------------------------------------------------------

db_admin_users <- function(pool) {
    DBI::dbGetQuery(
        pool,
        "SELECT u.id, u.auth0_sub, u.email, u.nickname, u.is_guest, u.created_at, u.last_seen_at,
                (SELECT count(*) FROM datasets d WHERE d.user_id = u.id) AS n_datasets,
                (SELECT count(*) FROM models m WHERE m.user_id = u.id) AS n_models,
                (SELECT count(*) FROM api_keys k WHERE k.user_id = u.id AND k.revoked_at IS NULL) AS n_api_keys
         FROM users u ORDER BY u.id"
    )
}

# FE server-side sessions live in the firesale/storr tables written by
# front into the shared schema. Their internal shape is storr's; this
# reports what is knowable without deserializing R blobs. Refined in Phase 7
# when the FE admin page consumes it.
db_admin_sessions <- function(pool) {
    exists <- DBI::dbGetQuery(pool, "SELECT to_regclass('fe_store_keys') AS t")$t
    if (is.na(exists)) {
        return(data.frame(namespace = character(), n = integer()))
    }
    DBI::dbGetQuery(
        pool,
        "SELECT namespace, count(*) AS n FROM fe_store_keys GROUP BY namespace ORDER BY namespace"
    )
}

db_admin_requests <- function(pool, hours = 24L) {
    DBI::dbGetQuery(
        pool,
        "SELECT service, method, path, status, count(*) AS n,
                round(avg(duration_ms)) AS avg_ms, max(duration_ms) AS max_ms
         FROM request_log
         WHERE ts > now() - make_interval(hours => $1)
         GROUP BY service, method, path, status
         ORDER BY n DESC, path",
        params = list(hours)
    )
}
