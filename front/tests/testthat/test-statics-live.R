# Static file serving is done at the httpuv (C++) level and bypasses R, so it
# cannot be exercised through pa$test_request(). This drives a live instance via
# callr + httr2 (spike finding 11) to prove fingerprinted assets are served.

skip_if_not_installed("callr")
skip_if_not_installed("httr2")

start_statics_api <- function(port, dist_dir) {
    srv <- callr::r_bg(
        function(port, dist_dir) {
            suppressMessages(library(plumber2))
            pa <- api() |> api_statics("/static", dist_dir)
            api_run(pa, host = "127.0.0.1", port = port, block = TRUE, showcase = FALSE)
        },
        args = list(port = port, dist_dir = dist_dir)
    )
    base_url <- sprintf("http://127.0.0.1:%d", port)
    ready <- FALSE
    for (i in 1:50) {
        Sys.sleep(0.2)
        if (!srv$is_alive()) {
            break
        }
        r <- tryCatch(
            httr2::request(paste0(base_url, "/static/manifest.json")) |>
                httr2::req_error(is_error = function(x) FALSE) |>
                httr2::req_perform(),
            error = function(e) NULL
        )
        if (!is.null(r)) {
            ready <- TRUE
            break
        }
    }
    if (!ready) {
        srv$kill()
        testthat::skip("could not start live statics api")
    }
    list(srv = srv, base_url = base_url)
}

test_that("fingerprinted static assets are served with the correct content-type", {
    base_dir <- dirname(r_dir)
    dist_dir <- normalizePath(file.path(base_dir, "dist"))
    manifest <- yyjsonr::read_json_file(file.path(dist_dir, "manifest.json"))

    port <- 18700L + sample(0:250, 1)
    live <- start_statics_api(port, dist_dir)
    withr::defer(live$srv$kill())

    css <- httr2::request(paste0(live$base_url, "/static/", manifest[["css/app.css"]])) |>
        httr2::req_perform()
    expect_equal(httr2::resp_status(css), 200L)
    expect_match(httr2::resp_content_type(css), "text/css")

    js <- httr2::request(paste0(live$base_url, "/static/", manifest[["vendor/htmx.min.js"]])) |>
        httr2::req_perform()
    expect_equal(httr2::resp_status(js), 200L)
    expect_match(httr2::resp_content_type(js), "javascript")
})
