# Phase 8 hardening: request_log retention pruning, rate-bucket sweeping and
# the fit-job walltime timeout.

test_that("db_prune_request_log removes only rows older than the retention window", {
    pool <- local_migrated_pool()

    DBI::dbExecute(
        pool,
        "INSERT INTO request_log (ts, service, method, path, status, duration_ms) VALUES
         (now() - interval '40 days', 'back', 'GET', '/v1/old', 200, 1),
         (now() - interval '1 day', 'back', 'GET', '/v1/new', 200, 1)"
    )

    pruned <- db_prune_request_log(pool, retention_days = 30L)

    expect_equal(pruned, 1L)
    expect_equal(DBI::dbGetQuery(pool, "SELECT path FROM request_log")$path, "/v1/new")
})

test_that("sweep_rate_buckets drops idle buckets and keeps active ones", {
    reset_rate_limits()
    withr::defer(reset_rate_limits())
    now <- Sys.time()
    take_rate_token("active", 10, now = now)
    take_rate_token("idle", 10, now = now - 3600)

    sweep_rate_buckets(max_idle_secs = 900, now = now)

    expect_equal(ls(rate_state$buckets), "active")
})

test_that("run_maintenance is a no-op without a pool and never errors", {
    reset_rate_limits()
    withr::defer(reset_rate_limits())
    set_app_pool(NULL)

    expect_no_error(run_maintenance(list(request_log_retention_days = 30L)))
})

test_that("a runaway fit is cancelled by the dispatcher walltime and the job fails", {
    pool <- local_migrated_pool()
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL))
    mirai::daemons(1L)
    withr::defer(mirai::daemons(0L))

    user <- get_or_create_guest(pool)
    job_id <- db_create_job(pool, user$id, "fit_model", list(dataset_id = 1L, formula = "mpg ~ wt"))
    # 1ms walltime: daemon dispatch alone exceeds it, so the fit always times out.
    launch_fit_job(job_id, user$id, 1L, "mpg ~ wt", mpg ~ wt, mtcars, timeout_seconds = 0.001)

    deadline <- Sys.time() + 10
    job <- NULL
    while (Sys.time() < deadline) {
        later::run_now(0.05)
        job <- db_get_job(pool, user$id, job_id)
        if (!job$status %in% c("pending", "running")) {
            break
        }
    }

    expect_equal(job$status, "error")
    expect_match(job$error, "fit timed out after")
})
