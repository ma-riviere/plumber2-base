# Home page + dataset actions through the real assembled api (guest mode, fake
# backend). State-changing requests carry the CSRF token + Origin (the gate
# enforces both).

test_that("empty filter params (htmx includes empty inputs) do not 400", {
    # Regression: typed @query params rejected `min_rows=` with a 400 that
    # htmx swapped over the panel (found in the live walkthrough).
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/home/datasets?min_rows=&max_rows=&created_from=&created_to=",
        headers = list(Cookie = cookie, HX_Request = "true")
    )
    expect_equal(res$status, 200L)
    expect_match(res$body, 'id="dataset-count">2<')
    expect_equal(res$headers[["hx-push-url"]], "/home")

    page <- do_request(pa, "http://t/explore?dataset=", headers = list(Cookie = cookie))
    expect_equal(page$status, 200L)
    expect_match(page$body, "No dataset selected", fixed = TRUE)
})

test_that("the upload proxy closes the modal, refreshes the panel and toasts", {
    pa <- local_front_api()
    session <- guest_session(pa)

    boundary <- "----feTestBoundary"
    body <- paste0(
        "--",
        boundary,
        "\r\n",
        'Content-Disposition: form-data; name="file"; filename="new.csv"',
        "\r\n",
        "Content-Type: text/csv\r\n\r\n",
        "a,b\n1,2\n",
        "\r\n",
        "--",
        boundary,
        "\r\n",
        'Content-Disposition: form-data; name="name"',
        "\r\n\r\n",
        "my dataset",
        "\r\n",
        "--",
        boundary,
        "--\r\n"
    )
    res <- do_request(
        pa,
        "http://t/datasets/upload",
        method = "post",
        headers = action_headers(
            session,
            Content_Type = paste0("multipart/form-data; boundary=", boundary)
        ),
        content = body
    )

    expect_equal(res$status, 200L)
    expect_match(res$headers[["hx-trigger"]], "fb:close-modal")
    expect_match(res$headers[["hx-trigger"]], "fb:refresh-datasets")
    expect_match(res$body, "hx-swap-oob", fixed = TRUE)
    expect_match(res$body, "Dataset uploaded successfully", fixed = TRUE)
})

test_that("an upload without a file part answers 422 with an alert fragment", {
    pa <- local_front_api()
    session <- guest_session(pa)

    boundary <- "----feTestBoundary"
    body <- paste0(
        "--",
        boundary,
        "\r\n",
        'Content-Disposition: form-data; name="name"',
        "\r\n\r\n",
        "no file",
        "\r\n",
        "--",
        boundary,
        "--\r\n"
    )
    res <- do_request(
        pa,
        "http://t/datasets/upload",
        method = "post",
        headers = action_headers(
            session,
            Content_Type = paste0("multipart/form-data; boundary=", boundary)
        ),
        content = body
    )

    expect_equal(res$status, 422L)
    expect_match(res$body, "alert-danger", fixed = TRUE)
})

test_that("backend 404s surface as alert fragments with the real status", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/datasets/999",
        method = "delete",
        headers = c(action_headers(session), list(HX_Request = "true"))
    )
    expect_equal(res$status, 404L)
    expect_match(res$body, "alert-danger", fixed = TRUE)
    expect_match(res$body, "no such dataset", fixed = TRUE)
})

test_that("the download proxy streams the CSV with the backend's headers", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/datasets/1/download", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$headers[["content-type"]], "text/csv")
    expect_match(res$headers[["content-disposition"]], 'attachment; filename="cars.csv"', fixed = TRUE)
    expect_match(res$body, "speed,dist", fixed = TRUE)

    missing <- do_request(pa, "http://t/datasets/999/download", headers = list(Cookie = cookie))
    expect_equal(missing$status, 404L)

    # Unauthenticated: the gate redirects before the proxy runs.
    anon <- do_request(pa, "http://t/datasets/1/download")
    expect_equal(anon$status, 302L)
})
