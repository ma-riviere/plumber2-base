render_shell <- function() {
    shell_path <- file.path(dirname(r_dir), "assets", "templates", "shell.html")
    template <- paste(readLines(shell_path, warn = FALSE), collapse = "\n")
    data <- list(
        lang = "en",
        title = "Test Page",
        csrf_token = "tok123",
        msg_server_error = "Server error, please try again",
        asset_bootstrap_css = "vendor/bootstrap.aaaaaaaa.css",
        asset_icons_css = "vendor/bootstrap-icons.min.bbbbbbbb.css",
        asset_app_css = "css/app.cccccccc.css",
        asset_htmx_js = "vendor/htmx.dddddddd.js",
        asset_bootstrap_js = "vendor/bootstrap.bundle.eeeeeeee.js",
        asset_app_js = "js/app.ffffffff.js",
        brand = "Base Front",
        nav_links = '<li class="nav-item"><a class="nav-link" href="/">Home</a></li>',
        user_menu = '<li class="nav-item">User</li>',
        content = "<p>Hello world</p>"
    )
    whisker::whisker.render(template, data)
}

test_that("shell template renders with no leftover placeholders", {
    rendered <- render_shell()
    expect_false(grepl("{{", rendered, fixed = TRUE))
})

test_that("rendered shell is parseable HTML5 with the expected structure", {
    rendered <- render_shell()
    doc <- xml2::read_html(rendered)

    expect_equal(
        xml2::xml_attr(xml2::xml_find_first(doc, "//html"), "lang"),
        "en"
    )
    expect_equal(
        xml2::xml_text(xml2::xml_find_first(doc, "//title")),
        "Test Page"
    )
    expect_equal(
        xml2::xml_attr(
            xml2::xml_find_first(doc, "//meta[@name='csrf-token']"),
            "content"
        ),
        "tok123"
    )
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
    expect_gt(length(xml2::xml_find_all(doc, "//nav")), 0)
    expect_equal(
        xml2::xml_text(xml2::xml_find_first(doc, "//main/p")),
        "Hello world"
    )
    expect_gt(length(xml2::xml_find_all(doc, "//div[@id='toasts']")), 0)
})

test_that("rendered shell wires assets from the manifest placeholders", {
    rendered <- render_shell()
    doc <- xml2::read_html(rendered)
    scripts <- xml2::xml_attr(xml2::xml_find_all(doc, "//script"), "src")
    expect_true("vendor/htmx.dddddddd.js" %in% scripts)
    expect_true("js/app.ffffffff.js" %in% scripts)
    links <- xml2::xml_attr(
        xml2::xml_find_all(doc, "//link[@rel='stylesheet']"),
        "href"
    )
    expect_true("css/app.cccccccc.css" %in% links)
})
