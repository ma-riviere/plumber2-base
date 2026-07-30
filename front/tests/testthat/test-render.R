# Unit tests for the rendering core. These are pure (no DB, no sockets): they use
# the real committed manifest/translations/shell so the assertions match what the
# assembled app serves.

front_state <- function() {
    base_dir <- dirname(r_dir)
    list(
        manifest = yyjsonr::read_json_file(file.path(base_dir, "dist", "manifest.json")),
        translations = load_translations(file.path(base_dir, "assets", "translations.json")),
        template = paste(
            readLines(file.path(base_dir, "assets", "templates", "shell.html"), warn = FALSE),
            collapse = "\n"
        )
    )
}

make_request <- function(headers = list()) {
    reqres::Request$new(fiery::fake_request("http://t/home", headers = headers))
}

test_that("render_shell wraps content in the shell with fingerprinted /static/ assets", {
    state <- front_state()
    html <- render_shell(content = "<p>hi</p>", title = "T", lang = "en", state = state)
    doc <- xml2::read_html(html)

    expect_equal(xml2::xml_attr(xml2::xml_find_first(doc, "//html"), "lang"), "en")
    expect_equal(xml2::xml_text(xml2::xml_find_first(doc, "//title")), "T")

    expected_css <- paste0("/static/", state$manifest[["css/app.css"]])
    links <- xml2::xml_attr(xml2::xml_find_all(doc, "//link[@rel='stylesheet']"), "href")
    expect_true(expected_css %in% links)

    expected_htmx <- paste0("/static/", state$manifest[["vendor/htmx.min.js"]])
    scripts <- xml2::xml_attr(xml2::xml_find_all(doc, "//script"), "src")
    expect_true(expected_htmx %in% scripts)

    expect_gt(length(xml2::xml_find_all(doc, "//div[@id='toasts']")), 0)
    expect_match(xml2::xml_text(xml2::xml_find_first(doc, "//main/p")), "hi")
})

test_that("the shell's htmx-config meta pins the security and history settings", {
    state <- front_state()
    html <- render_shell(content = "<p>hi</p>", title = "T", lang = "en", state = state)
    doc <- xml2::read_html(html)

    htmx_config <- yyjsonr::read_json_str(xml2::xml_attr(
        xml2::xml_find_first(doc, "//meta[@name='htmx-config']"),
        "content"
    ))
    expect_false(htmx_config$allowEval)
    # History restores must NOT send HX-Request (they need the full page back).
    expect_false(htmx_config$historyRestoreAsHxRequest)
    expect_true(htmx_config$refreshOnHistoryMiss)
    # Reused fragments may carry dormant hx-swap-oob attrs: nested OOB must not
    # extract them out of the primary content.
    expect_false(htmx_config$allowNestedOobSwaps)
    # 4xx swaps into the target (error fragments); 5xx does not (toast path).
    handling <- htmx_config$responseHandling
    expect_true(handling$swap[handling$code == "4.."])
    expect_false(handling$swap[handling$code == "..."])
})

test_that("render_page returns the full shell and sets the HTML headers", {
    state <- front_state()
    request <- make_request()
    response <- request$respond()
    body <- render_page(request, response, content = "<p>x</p>", title = "T", lang = "en", state = state)

    expect_true(grepl("<html", body, fixed = TRUE))
    expect_equal(response$get_header("Vary"), "HX-Request")
    expect_equal(response$get_header("Cache-Control"), "private, no-store")
})

test_that("render_page returns a bare fragment under HX-Request with the HTML headers", {
    state <- front_state()
    request <- make_request(list(HX_Request = "true"))
    response <- request$respond()
    body <- render_page(request, response, content = "<p>x</p>", title = "T", lang = "en", state = state)

    expect_false(grepl("<html", body, fixed = TRUE))
    expect_equal(body, "<p>x</p>")
    expect_equal(response$get_header("Vary"), "HX-Request")
    expect_equal(response$get_header("Cache-Control"), "private, no-store")
})

test_that("redirect stamps Location and no-store", {
    response <- make_request()$respond()

    out <- redirect(response, "/login")

    expect_identical(out, plumber2::Break)
    expect_equal(response$status, 302L)
    expect_equal(response$get_header("Location"), "/login")
    expect_equal(response$get_header("Cache-Control"), "private, no-store")
})

test_that("with_fe_errors stamps the HTML headers on htmx error fragments", {
    state <- front_state()
    request <- make_request(list(HX_Request = "true"))
    response <- request$respond()

    body <- with_fe_errors(
        request,
        response,
        state,
        list(session = list(auth = NULL)),
        stop(backend_error(502L, "Bad Gateway", "boom"))
    )

    expect_equal(response$status, 502L)
    expect_match(body, "alert-danger", fixed = TRUE)
    expect_match(body, "boom", fixed = TRUE)
    expect_equal(response$get_header("Vary"), "HX-Request")
    expect_equal(response$get_header("Cache-Control"), "private, no-store")
})

test_that("with_fe_errors answers htmx auth expiry with HX-Redirect and no-store", {
    state <- front_state()
    request <- make_request(list(HX_Request = "true"))
    response <- request$respond()

    body <- with_fe_errors(request, response, state, list(), stop(auth_expired()))

    expect_equal(response$status, 200L)
    expect_equal(body, "")
    expect_equal(response$get_header("HX-Redirect"), "/login")
    expect_equal(response$get_header("Cache-Control"), "private, no-store")
})
