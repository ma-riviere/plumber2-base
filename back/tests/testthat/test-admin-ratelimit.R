# Admin endpoints (view:admin scope) + the rate limiter (bucket unit tests and
# the enforcement routes).

test_that("admin endpoints require the view:admin scope", {
    ctx <- auth_api(bypass = TRUE) # guest has user scopes only
    for (path in c("/v1/admin/users", "/v1/admin/sessions", "/v1/admin/requests")) {
        expect_equal(do_request(ctx$pa, paste0("http://t", path))$status, 403L)
    }
})

test_that("admin users/sessions/requests return their shapes for an admin", {
    ctx <- auth_api()
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))

    # Generate some logged traffic first (me is not excluded from the log).
    do_request(ctx$pa, "http://t/v1/me", headers = admin)

    users <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/admin/users", headers = admin)$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_gte(length(users$items), 1L)
    expect_equal(users$items[[1]]$auth0_sub, "auth0|root")
    expect_equal(users$items[[1]]$n_datasets, 0L)

    sessions <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/admin/sessions", headers = admin)$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_length(sessions$items, 0L) # no FE store tables in the scratch schema

    requests <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/admin/requests", headers = admin)$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_equal(requests$window_hours, 24L)
    paths <- vapply(requests$items, function(x) x$path, character(1))
    expect_true("/v1/me" %in% paths)
})

test_that("requests are logged with the resolved principal; noisy paths are excluded", {
    ctx <- auth_api(bypass = TRUE)
    do_request(ctx$pa, "http://t/v1/me")
    do_request(ctx$pa, "http://t/health")
    do_request(ctx$pa, "http://t/v1/jobs/00000000-0000-0000-0000-000000000000")

    logged <- DBI::dbGetQuery(
        ctx$pool,
        "SELECT method, path, status, user_id FROM request_log ORDER BY id"
    )
    expect_true("/v1/me" %in% logged$path)
    expect_false("/health" %in% logged$path)
    expect_false(any(startsWith(logged$path, "/v1/jobs/")))
    me_row <- logged[logged$path == "/v1/me", ]
    expect_equal(me_row$status[1], 200L)
    expect_false(is.na(me_row$user_id[1])) # guest resolved and recorded
})

test_that("take_rate_token implements a refilling bucket", {
    reset_rate_limits()
    t0 <- Sys.time()
    for (i in 1:3) {
        expect_true(take_rate_token("k", 3L, now = t0)$allowed)
    }
    denied <- take_rate_token("k", 3L, now = t0)
    expect_false(denied$allowed)
    expect_gte(denied$reset, 1L)
    # 20 seconds later one token has refilled (3/min = 1 per 20s).
    refilled <- take_rate_token("k", 3L, now = t0 + 21)
    expect_true(refilled$allowed)
    # Buckets are independent per key.
    expect_true(take_rate_token("other", 3L, now = t0)$allowed)
})

test_that("the principal limiter yields 429 with RateLimit and Retry-After headers", {
    withr::local_envvar(RATE_LIMIT_PER_MIN = "3")
    ctx <- auth_api(bypass = TRUE)

    for (i in 1:3) {
        ok <- do_request(ctx$pa, "http://t/v1/me")
        expect_equal(ok$status, 200L)
        expect_equal(ok$headers[["ratelimit-limit"]], "3")
    }
    limited <- do_request(ctx$pa, "http://t/v1/me")
    expect_equal(limited$status, 429L)
    expect_match(limited$headers[["content-type"]], "application/problem\\+json")
    expect_equal(limited$headers[["ratelimit-remaining"]], "0")
    expect_false(is.null(limited$headers[["retry-after"]]))
    # Health is outside the limited surface.
    expect_false(do_request(ctx$pa, "http://t/health")$status == 429L)
})

test_that("the expensive-endpoint bucket is stricter than the general one", {
    withr::local_envvar(RATE_LIMIT_FITS_PER_MIN = "1", RATE_LIMIT_PER_MIN = "50")
    ctx <- auth_api(bypass = TRUE)
    payload <- multipart_csv("mpg,wt\n1,2\n2,3\n")
    up <- do_request(
        ctx$pa,
        "http://t/v1/datasets",
        method = "post",
        headers = payload$headers,
        content = payload$content
    )
    id <- yyjsonr::read_json_str(up$body)$id
    guest_id <- DBI::dbGetQuery(ctx$pool, "SELECT id FROM users WHERE is_guest LIMIT 1")$id
    # Park a live job so the fit POST cannot actually launch work.
    db_create_job(ctx$pool, guest_id, "fit_model", list(dataset_id = as.integer(id), formula = "mpg ~ wt"))

    first <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt")
    )
    expect_equal(first$status, 202L) # dedupe short-circuit, still consumes the fit bucket
    second <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt")
    )
    expect_equal(second$status, 429L)
    # General reads are unaffected by the fit bucket.
    expect_equal(do_request(ctx$pa, "http://t/v1/me")$status, 200L)
})

test_that("unauthenticated requests are answered by the guards, not the limiter", {
    # fireproof's auth route always dispatches first (spike addendum): a failed
    # auth aborts before the limiter route runs. IP flood control is the edge's
    # job (Cloudflare); this pins the in-app behavior.
    ctx <- auth_api()
    res <- do_request(ctx$pa, "http://t/v1/me")
    expect_equal(res$status, 401L)
    expect_null(res$headers[["ratelimit-limit"]])
})
