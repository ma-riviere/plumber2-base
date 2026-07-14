# Async job records + the fit-job lifecycle. The POST /v1/models handler runs
# synchronously (validation, dedupe, cap, job insert), launches the fit in a
# mirai worker and returns 202 immediately; the promise callback back on the
# main process writes the model and finishes the job. plumber2's @async is NOT
# used because the whole handler would run in the worker, where neither the
# pool (job insert) nor the request principal exist (spike finding 5).

db_create_job <- function(pool, user_id, kind, payload) {
    DBI::dbGetQuery(
        pool,
        "INSERT INTO jobs (user_id, kind, status, payload) VALUES ($1, $2, 'running', $3::jsonb)
         RETURNING id",
        params = list(user_id, kind, yyjsonr::write_json_str(payload, auto_unbox = TRUE))
    )$id
}

# The jobs.id column is a uuid: a non-uuid path segment would make Postgres
# raise an "invalid input syntax for type uuid" error (a 500) instead of a clean
# 404. Callers validate the shape first.
UUID_PATTERN <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"

is_uuid <- function(x) {
    is.character(x) && length(x) == 1 && grepl(UUID_PATTERN, x)
}

db_get_job <- function(pool, user_id, job_id) {
    row <- DBI::dbGetQuery(
        pool,
        "SELECT id, kind, status, payload, result, error, created_at, updated_at
         FROM jobs WHERE id = $1 AND user_id = $2",
        params = list(job_id, user_id)
    )
    if (nrow(row) == 0) NULL else row
}

db_count_active_jobs <- function(pool, user_id) {
    as.integer(
        DBI::dbGetQuery(
            pool,
            "SELECT count(*) AS n FROM jobs WHERE user_id = $1 AND status IN ('pending', 'running')",
            params = list(user_id)
        )$n
    )
}

# Double-click protection: an identical live fit request returns the existing job.
db_find_active_fit_job <- function(pool, user_id, dataset_id, formula_str) {
    row <- DBI::dbGetQuery(
        pool,
        "SELECT id FROM jobs
         WHERE user_id = $1 AND kind = 'fit_model' AND status IN ('pending', 'running')
           AND payload->>'dataset_id' = $2 AND payload->>'formula' = $3
         ORDER BY created_at LIMIT 1",
        params = list(user_id, as.character(dataset_id), formula_str)
    )
    if (nrow(row) == 0) NULL else row$id
}

db_finish_job <- function(pool, job_id, result) {
    DBI::dbExecute(
        pool,
        "UPDATE jobs SET status = 'done', result = $2::jsonb, updated_at = now() WHERE id = $1",
        params = list(job_id, yyjsonr::write_json_str(result, auto_unbox = TRUE))
    )
    invisible()
}

db_fail_job <- function(pool, job_id, error) {
    DBI::dbExecute(
        pool,
        "UPDATE jobs SET status = 'error', error = $2, updated_at = now() WHERE id = $1",
        params = list(job_id, error)
    )
    invisible()
}

# Startup recovery: any job still live in the table was orphaned by a restart;
# mark it failed so polling clients terminate cleanly (run before serving).
db_recover_stale_jobs <- function(pool) {
    n <- DBI::dbExecute(
        pool,
        "UPDATE jobs SET status = 'error', error = 'stale: interrupted by a service restart',
         updated_at = now() WHERE status IN ('pending', 'running')"
    )
    if (n > 0) {
        cat(sprintf("[back] recovered %d stale job(s)\n", n), file = stderr())
    }
    invisible(n)
}

# Launch the fit and wire the completion back into the database. Everything sent
# to the worker is plain data (the formula's environment holds only whitelisted
# functions). The callbacks run on the main process via the promise loop; any
# error in them fails the job rather than leaving it 'running' forever.
launch_fit_job <- function(job_id, user_id, dataset_id, formula_str, formula, data, timeout_seconds = NULL) {
    timeout_seconds <- timeout_seconds %||% app_config()$fit_timeout_seconds %||% 60
    handle <- mirai::mirai(
        fit_task(data, formula),
        fit_task = fit_model_task,
        data = data,
        formula = formula,
        # Dispatcher walltime cap: a runaway fit is cancelled and the promise
        # rejects, so the daemon is freed and the job fails cleanly.
        .timeout = as.integer(timeout_seconds * 1000)
    )
    promises::then(
        handle,
        onFulfilled = function(result) {
            tryCatch(
                {
                    if (isTRUE(result$success)) {
                        model_id <- db_upsert_model(
                            app_pool(),
                            user_id,
                            dataset_id,
                            formula_str,
                            result$metrics,
                            result$model_blob
                        )
                        db_finish_job(
                            app_pool(),
                            job_id,
                            list(
                                model_id = as.integer(model_id),
                                metrics = result$metrics[c("r_squared", "rmse", "aic")]
                            )
                        )
                    } else {
                        db_fail_job(app_pool(), job_id, result$error)
                    }
                },
                error = function(e) {
                    db_fail_job(app_pool(), job_id, conditionMessage(e))
                }
            )
        },
        onRejected = function(e) {
            msg <- conditionMessage(e)
            # A dispatcher-cancelled task rejects with "5 | Timed out".
            if (grepl("timed out", msg, ignore.case = TRUE)) {
                msg <- sprintf("fit timed out after %s seconds", format(timeout_seconds))
            }
            db_fail_job(app_pool(), job_id, msg)
        }
    )
    invisible(job_id)
}

job_json <- function(row) {
    out <- list(
        id = jsonlite::unbox(row$id),
        kind = jsonlite::unbox(row$kind),
        status = jsonlite::unbox(row$status),
        created_at = jsonlite::unbox(format(row$created_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
        updated_at = jsonlite::unbox(format(row$updated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    )
    if (!is.na(row$result)) {
        out$result <- unbox_scalars(yyjsonr::read_json_str(row$result[[1]]))
    }
    if (!is.na(row$error)) {
        out$error <- jsonlite::unbox(row$error)
    }
    out
}
