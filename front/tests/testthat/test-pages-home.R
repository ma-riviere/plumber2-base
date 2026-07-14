# Home page + dataset actions through the real assembled api (guest mode, fake
# backend). State-changing requests carry the CSRF token + Origin (the gate
# enforces both).

test_that("/home lists the backend datasets with stat card, filters and upload modal", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/home", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$body, ">cars<")
    expect_match(res$body, ">trees<")
    expect_match(res$body, 'id="dataset-count">2<')
    expect_match(res$body, 'id="home-filters"', fixed = TRUE)
    expect_match(res$body, 'id="upload-modal"', fixed = TRUE)
    expect_match(res$body, 'hx-encoding="multipart/form-data"', fixed = TRUE)
    # Row actions: explore link, rename, download, delete.
    expect_match(res$body, 'href="/explore?dataset=1"', fixed = TRUE)
    expect_match(res$body, "/partials/dataset/1/edit?context=home", fixed = TRUE)
    expect_match(res$body, 'href="/datasets/1/download"', fixed = TRUE)
    expect_match(res$body, 'hx-delete="/datasets/1"', fixed = TRUE)
    # User menu: Account link present; Profile disabled for the guest.
    expect_match(res$body, 'href="/account"', fixed = TRUE)
    expect_match(res$body, "dropdown-item disabled", fixed = TRUE)
    # No admin scope -> no Admin nav entry.
    expect_false(grepl('href="/admin"', res$body, fixed = TRUE))
})

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

test_that("/partials/home/datasets filters and pushes the canonical /home URL", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/home/datasets?max_rows=40",
        headers = list(Cookie = cookie, HX_Request = "true")
    )

    expect_equal(res$status, 200L)
    expect_equal(res$headers[["hx-push-url"]], "/home?max_rows=40")
    expect_match(res$body, ">trees<")
    expect_false(grepl(">cars<", res$body))
    expect_match(res$body, 'id="dataset-count">1<')

    # min_rows has no UI control anymore: a stale URL param is ignored (and
    # a slider at the ceiling is treated as no filter).
    stale <- do_request(
        pa,
        "http://t/partials/home/datasets?min_rows=40&max_rows=50000",
        headers = list(Cookie = cookie, HX_Request = "true")
    )
    expect_equal(stale$headers[["hx-push-url"]], "/home")
    expect_match(stale$body, 'id="dataset-count">2<')
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

test_that("the rename flow serves the edit form, validates, and returns the row", {
    pa <- local_front_api()
    session <- guest_session(pa)

    edit <- do_request(
        pa,
        "http://t/partials/dataset/1/edit",
        headers = list(Cookie = session$cookie, HX_Request = "true")
    )
    expect_equal(edit$status, 200L)
    expect_match(edit$body, 'hx-patch="/datasets/1"', fixed = TRUE)
    expect_match(edit$body, 'value="cars"', fixed = TRUE)

    empty <- do_request(
        pa,
        "http://t/datasets/1",
        method = "patch",
        headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
        content = "name="
    )
    expect_equal(empty$status, 200L)
    expect_match(empty$body, "is-invalid", fixed = TRUE)
    expect_match(empty$body, "Dataset name cannot be empty", fixed = TRUE)

    renamed <- do_request(
        pa,
        "http://t/datasets/1",
        method = "patch",
        headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
        content = "name=wheels"
    )
    expect_equal(renamed$status, 200L)
    expect_match(renamed$body, ">wheels<")
    expect_match(renamed$body, "Dataset renamed successfully", fixed = TRUE)

    cancel <- do_request(
        pa,
        "http://t/partials/dataset/1/row",
        headers = list(Cookie = session$cookie, HX_Request = "true")
    )
    expect_equal(cancel$status, 200L)
    expect_match(cancel$body, 'id="dataset-row-1"', fixed = TRUE)
})

test_that("deleting a dataset toasts and triggers the panel refresh", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(pa, "http://t/datasets/1", method = "delete", headers = action_headers(session))

    expect_equal(res$status, 200L)
    expect_equal(res$headers[["hx-trigger"]], "fb:refresh-datasets")
    expect_match(res$body, "Dataset deleted successfully", fixed = TRUE)
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
