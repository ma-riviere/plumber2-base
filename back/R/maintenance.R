# In-process periodic maintenance (request_log retention, rate-bucket sweep),
# scheduled on fiery's later loop at startup. Deliberately in-app rather than an
# external cron/systemd timer: the deployment is a single replica and both tasks
# are trivial (indexed DELETE, small environment sweep), so a scheduler
# container would be pure overhead. Revisit alongside the rate limiter if
# replicas ever exceed 1.

MAINTENANCE_INTERVAL_SECONDS <- 3600

run_maintenance <- function(config) {
    pool <- app_pool()
    if (!is.null(pool)) {
        pruned <- tryCatch(
            db_prune_request_log(pool, config$request_log_retention_days),
            error = function(e) NA_integer_
        )
        if (!is.na(pruned) && pruned > 0) {
            cat(sprintf("[back] pruned %d request_log row(s)\n", pruned), file = stderr())
        }
    }
    sweep_rate_buckets()
    invisible()
}

# First tick fires immediately (startup prune), then every `interval` seconds.
# run_maintenance contains all errors, so the chain cannot die.
schedule_maintenance <- function(config, interval = MAINTENANCE_INTERVAL_SECONDS) {
    tick <- function() {
        run_maintenance(config)
        later::later(tick, interval)
    }
    later::later(tick, 0)
    invisible()
}
