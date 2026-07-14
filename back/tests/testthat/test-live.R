# Live instance over real sockets (callr + httr2), the spike's pattern for things
# in-process test_request cannot exercise. The docs UI (openapi route) is only
# registered when the server ignites, so 200-on-docs must be checked live.

test_that("a live instance serves docs UI and endpoints over real sockets", {
    testthat::skip_if_not_installed("callr")
    testthat::skip_if_not_installed("httr2")
    # The start hook opens a pool, so a live run needs the dev database.
    pool <- dev_pool_or_skip()
    pool::poolClose(pool)

    port <- 8091L
    proc <- callr::r_bg(
        function(dir, port) {
            setwd(dir)
            plumber2::api("_server.yml") |>
                plumber2::api_run(
                    host = "127.0.0.1",
                    port = port,
                    block = TRUE,
                    showcase = FALSE,
                    silent = TRUE
                )
        },
        args = list(dir = BACK_DIR, port = port)
    )
    withr::defer({
        if (proc$is_alive()) {
            proc$interrupt()
            Sys.sleep(0.3)
            if (proc$is_alive()) proc$kill()
        }
    })

    base_url <- sprintf("http://127.0.0.1:%d", port)
    fetch <- function(path) {
        httr2::request(paste0(base_url, path)) |>
            httr2::req_timeout(3) |>
            httr2::req_error(is_error = function(resp) FALSE) |>
            httr2::req_perform()
    }

    ready <- FALSE
    for (i in seq_len(60)) {
        if (!proc$is_alive()) {
            break
        }
        if (tryCatch(!is.null(fetch("/health")), error = function(e) FALSE)) {
            ready <- TRUE
            break
        }
        Sys.sleep(0.25)
    }
    if (!ready) {
        testthat::skip("live server did not come up (could not bind or connect)")
    }

    health <- fetch("/health")
    expect_equal(httr2::resp_status(health), 200L)
    expect_equal(httr2::resp_body_json(health)$status, "ok")
    expect_match(
        httr2::resp_header(health, "content-security-policy"),
        "default-src 'none'",
        fixed = TRUE
    )

    # /v1 is behind auth since Phase 3; the child process runs without bypass and
    # without a JWKS fixture, so the live check asserts the 401 challenge.
    ping <- fetch("/v1/ping")
    expect_equal(httr2::resp_status(ping), 401L)
    expect_match(httr2::resp_header(ping, "www-authenticate"), "Bearer")

    nope <- fetch("/nope")
    expect_equal(httr2::resp_status(nope), 404L)
    expect_match(httr2::resp_content_type(nope), "application/problem\\+json")

    docs <- fetch("/__docs__/")
    expect_equal(httr2::resp_status(docs), 200L)
    expect_match(httr2::resp_content_type(docs), "text/html")
    # The rapidoc page boots through inline script; the docs paths carry the
    # relaxed CSP while everything else keeps the strict API policy.
    expect_match(
        httr2::resp_header(docs, "content-security-policy"),
        "script-src 'self' 'unsafe-inline'",
        fixed = TRUE
    )

    # rapidoc loads the spec from /openapi.json (a sibling of /__docs__/); the
    # fallback must not shadow it.
    spec <- fetch("/openapi.json")
    expect_equal(httr2::resp_status(spec), 200L)
    expect_equal(httr2::resp_body_json(spec)$info$title, "back")
})
