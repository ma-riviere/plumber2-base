# Explore and Model pages through the real assembled api (guest mode, fake
# backend), including the fit -> poll -> terminal-fragment flow.

test_that("/explore without a selection renders the empty state and the picker", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/explore", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$body, 'id="dataset-select"', fixed = TRUE)
    expect_match(res$body, "No dataset selected", fixed = TRUE)
    expect_match(res$body, 'hx-get="/partials/explore/content"', fixed = TRUE)
})

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

test_that("/partials/explore/content pushes the canonical URL", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/explore/content?dataset=1",
        headers = list(Cookie = cookie, HX_Request = "true")
    )
    expect_equal(res$headers[["hx-push-url"]], "/explore?dataset=1")
    expect_match(res$body, 'id="page-body"', fixed = TRUE)
    expect_false(grepl("<html", res$body, fixed = TRUE))
})

test_that("/partials/explore/preview pages through the rows", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/explore/preview?dataset=1&offset=10",
        headers = list(Cookie = cookie, HX_Request = "true")
    )
    expect_equal(res$status, 200L)
    expect_match(res$body, "11-20 / 50", fixed = TRUE)
    # Previous points back to offset 0 (hx-vals JSON, quotes escaped in attrs).
    expect_match(res$body, "offset&quot;: 0", fixed = TRUE)
})

test_that("/model?dataset=1 renders the fit form, variables and saved models", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/model?dataset=1", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$body, 'id="formula-input"', fixed = TRUE)
    expect_match(res$body, 'hx-post="/models/fit"', fixed = TRUE)
    expect_match(res$body, "Available variables:", fixed = TRUE)
    expect_match(res$body, 'id="saved-models"', fixed = TRUE)
    expect_match(res$body, "dist ~ speed", fixed = TRUE)
})

test_that("submitting a fit returns the self-polling fragment", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/models/fit",
        method = "post",
        headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
        content = "dataset=1&formula=slow%20~%20x"
    )

    expect_equal(res$status, 200L)
    expect_match(res$body, 'hx-get="/partials/model/job/job-running?dataset=1&amp;model="', fixed = TRUE)
    expect_match(res$body, 'hx-trigger="load delay:1s"', fixed = TRUE)
    expect_match(res$body, "Fitting model...", fixed = TRUE)
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

test_that("loading a saved model fills the results area and mirrors the formula", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/model/saved/7",
        headers = list(Cookie = cookie, HX_Request = "true")
    )

    expect_equal(res$status, 200L)
    expect_match(res$body, "Model Summary", fixed = TRUE)
    expect_match(res$body, 'id="formula-input"', fixed = TRUE)
    expect_match(res$body, 'value="dist ~ speed"', fixed = TRUE)
    expect_match(res$body, 'hx-swap-oob="true"', fixed = TRUE)
})

test_that("deleting a model refreshes the sidebar and clears the results", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/models/7?dataset=1",
        method = "delete",
        headers = action_headers(session)
    )

    expect_equal(res$status, 200L)
    expect_match(res$body, "Model deleted", fixed = TRUE)
    expect_match(res$body, 'id="saved-models" hx-swap-oob="true"', fixed = TRUE)
    expect_match(res$body, 'id="fit-status"', fixed = TRUE)
})

test_that("/partials/model/content pushes the canonical URL", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/model/content?dataset=1",
        headers = list(Cookie = cookie, HX_Request = "true")
    )
    expect_equal(res$headers[["hx-push-url"]], "/model?dataset=1")
    expect_match(res$body, 'id="page-body"', fixed = TRUE)
})
