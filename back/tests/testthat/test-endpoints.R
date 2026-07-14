# In-process endpoint behaviour via fiery::fake_request + pa$test_request. The
# start/end lifecycle hooks do not fire here, so tests set the pool explicitly.
# Docs UI (openapi route) is only registered at api_run(); it is covered live in
# test-live.R.

test_that("GET /health returns 200 {status: ok} when the database answers", {
    pool <- dev_pool_or_skip()
    withr::defer(pool::poolClose(pool))
    pa <- build_test_api()
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL))

    res <- do_request(pa, "http://t/health")

    expect_equal(res$status, 200L)
    expect_match(res$headers[["content-type"]], "application/json")
    expect_equal(yyjsonr::read_json_str(res$body)$status, "ok")
})

test_that("GET /health returns a 503 problem+json when the database is unreachable", {
    # A closed pool fails SELECT 1 immediately, exercising the same failure path as
    # a truly unreachable database without the multi-second TCP timeout of a dead
    # port.
    pool <- dev_pool_or_skip()
    pool::poolClose(pool)
    pa <- build_test_api()
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL))

    res <- do_request(pa, "http://t/health")

    expect_equal(res$status, 503L)
    expect_match(res$headers[["content-type"]], "application/problem\\+json")
    expect_equal(yyjsonr::read_json_str(res$body)$status, 503L)
})

test_that("GET /v1/ping works under the /v1 root and returns an ISO timestamp", {
    # Since Phase 3 the whole /v1 surface is behind auth; bypass mode keeps this
    # a pure routing/serializer test (the auth matrix lives in
    # test-auth-endpoints.R).
    with_auth_env(bypass = TRUE)
    pa <- build_test_api()

    res <- do_request(pa, "http://t/v1/ping")

    expect_equal(res$status, 200L)
    expect_match(res$headers[["content-type"]], "application/json")
    pong <- yyjsonr::read_json_str(res$body)$pong
    expect_match(pong, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
})

test_that("ping is NOT served at the unprefixed /ping", {
    pa <- build_test_api()

    res <- do_request(pa, "http://t/ping")

    expect_equal(res$status, 404L)
    expect_match(res$headers[["content-type"]], "application/problem\\+json")
})

test_that("an unknown route returns a 404 problem+json (not a bare 404)", {
    pa <- build_test_api()

    res <- do_request(pa, "http://t/nope")

    expect_equal(res$status, 404L)
    expect_match(res$headers[["content-type"]], "application/problem\\+json")
    problem <- yyjsonr::read_json_str(res$body)
    expect_equal(problem$status, 404L)
    expect_true(nzchar(problem$detail))
})

test_that("security headers are present on responses, without HSTS or a redirect", {
    pool <- dev_pool_or_skip()
    withr::defer(pool::poolClose(pool))
    pa <- build_test_api()
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL))

    res <- do_request(pa, "http://t/health")

    expect_equal(res$status, 200L)
    expect_true(all(
        c(
            "content-security-policy",
            "x-content-type-options",
            "x-frame-options",
            "referrer-policy"
        ) %in%
            names(res$headers)
    ))
    expect_equal(res$headers[["x-frame-options"]], "DENY")
    expect_null(res$headers[["strict-transport-security"]])

    # Strict API policy: JSON responses are never rendered as documents.
    csp <- res$headers[["content-security-policy"]]
    expect_match(csp, "default-src 'none'", fixed = TRUE)
    expect_match(csp, "frame-ancestors 'none'", fixed = TRUE)
})

test_that("the docs paths relax the CSP for the rapidoc page, other paths keep the strict one", {
    with_auth_env(bypass = TRUE)
    pa <- build_test_api()

    # The docs page itself is only served at api_run() (covered in test-live.R);
    # in-process the override route is dispatched against the docs path directly.
    # Abort-path responses (404 etc.) cannot carry ANY response header: the
    # problem renderer drops fiery's global defaults too (spike addendum).
    docs_route <- pa$request_router$get_route("docs_csp")
    req <- reqres::Request$new(fiery::fake_request("http://t/__docs__/"))
    expect_true(docs_route$dispatch(req))
    expect_match(
        req$respond()$get_header("Content-Security-Policy"),
        "script-src 'self' 'unsafe-inline'",
        fixed = TRUE
    )

    # A non-docs 200 keeps the strict API policy.
    ping <- do_request(pa, "http://t/v1/ping")
    expect_match(ping$headers[["content-security-policy"]], "default-src 'none'", fixed = TRUE)
})

test_that("/v1 responses carry Cache-Control: no-store", {
    with_auth_env(bypass = TRUE)
    pa <- build_test_api()

    res <- do_request(pa, "http://t/v1/ping")

    expect_equal(res$status, 200L)
    expect_equal(res$headers[["cache-control"]], "no-store")
})
