# Explore and Model pages through the real assembled api (guest mode, fake
# backend), including the fit -> poll -> terminal-fragment flow.

test_that("/explore?dataset=1 renders description, column summary and preview", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/explore?dataset=1", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$body, "Speed and stopping distances", fixed = TRUE)
    expect_match(res$body, ">speed<")
    expect_match(res$body, 'id="preview"', fixed = TRUE)
    # 50 rows, page size 10 -> Next enabled, Previous disabled.
    expect_match(res$body, "1-10 / 50", fixed = TRUE)
    # A stale/foreign id degrades to the empty state instead of erroring.
    gone <- do_request(pa, "http://t/explore?dataset=999", headers = list(Cookie = cookie))
    expect_equal(gone$status, 200L)
    expect_match(gone$body, "No dataset selected", fixed = TRUE)
})

test_that("fit rejections surface as alert fragments with the backend status", {
    pa <- local_front_api()
    session <- guest_session(pa)
    post_fit <- function(body) {
        do_request(
            pa,
            "http://t/models/fit",
            method = "post",
            headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
            content = body
        )
    }

    unsafe <- post_fit("dataset=1&formula=boom")
    expect_equal(unsafe$status, 422L)
    expect_match(unsafe$body, "disallowed function", fixed = TRUE)

    capped <- post_fit("dataset=1&formula=cap%20~%20x")
    expect_equal(capped$status, 429L)
    expect_match(capped$body, "too many jobs", fixed = TRUE)

    missing <- post_fit("dataset=1&formula=")
    expect_equal(missing$status, 422L)
    expect_match(missing$body, "alert-danger", fixed = TRUE)
})

test_that("the job partial keeps polling while running and terminates on done/error", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    poll <- function(job) {
        do_request(
            pa,
            sprintf("http://t/partials/model/job/%s?dataset=1", job),
            headers = list(Cookie = cookie, HX_Request = "true")
        )
    }

    running <- poll("job-running")
    expect_equal(running$status, 200L)
    expect_match(running$body, 'hx-trigger="load delay:2s"', fixed = TRUE)

    done <- poll("job-done")
    expect_equal(done$status, 200L)
    expect_false(grepl("hx-trigger=\"load", done$body))
    expect_match(done$body, "Model Summary", fixed = TRUE)
    expect_match(done$body, "0.6511", fixed = TRUE)
    expect_match(done$body, 'id="saved-models" hx-swap-oob="true"', fixed = TRUE)
    expect_match(done$body, "Model fitted successfully", fixed = TRUE)

    failed <- poll("job-error")
    expect_equal(failed$status, 200L)
    expect_false(grepl("hx-trigger=\"load", failed$body))
    expect_match(failed$body, "singular matrix", fixed = TRUE)
    expect_match(failed$body, "Model fitting failed", fixed = TRUE)
})

test_that("a saved model loads with the formula mirrored OOB and deletes cleanly", {
    pa <- local_front_api()
    session <- guest_session(pa)

    loaded <- do_request(
        pa,
        "http://t/partials/model/saved/7",
        headers = list(Cookie = session$cookie, HX_Request = "true")
    )
    expect_equal(loaded$status, 200L)
    expect_match(loaded$body, "Model Summary", fixed = TRUE)
    expect_match(loaded$body, 'id="formula-input"', fixed = TRUE)
    expect_match(loaded$body, 'value="dist ~ speed"', fixed = TRUE)
    expect_match(loaded$body, 'hx-swap-oob="true"', fixed = TRUE)

    deleted <- do_request(
        pa,
        "http://t/models/7?dataset=1",
        method = "delete",
        headers = action_headers(session)
    )
    expect_equal(deleted$status, 200L)
    expect_match(deleted$body, "Model deleted", fixed = TRUE)
    expect_match(deleted$body, 'id="saved-models" hx-swap-oob="true"', fixed = TRUE)
    expect_match(deleted$body, 'id="fit-status"', fixed = TRUE)
})
