# In-process integration tests: the real route files parsed into an api assembled
# against an isolated scratch schema (skipped if the dev Postgres is unreachable).
# Pages sit behind the auth gate, so protected requests replay the guest-session
# cookie obtained from /login (bypass mode).

test_that("/health returns 200 while the datastore connection is live", {
    pa <- local_front_api()
    res <- do_request(pa, "http://t/health")
    expect_equal(res$status, 200L)
    expect_match(res$body, '"ok"', fixed = TRUE)
})

test_that("dev api does not trust X-Forwarded-* headers (prod-only, behind Traefik)", {
    pa <- local_front_api()
    expect_false(pa$trust)
})

test_that("GET / redirects to /home for a logged-in session", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/", headers = list(Cookie = cookie))
    expect_equal(res$status, 302L)
    expect_equal(res$headers[["location"]], "/home")
})

test_that("/home renders the full shell with fingerprinted assets and toast container", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/home", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$headers[["content-type"]], "text/html")
    expect_equal(res$headers[["vary"]], "HX-Request")
    expect_equal(res$headers[["cache-control"]], "private, no-store")

    doc <- xml2::read_html(res$body)
    expect_equal(xml2::xml_attr(xml2::xml_find_first(doc, "//html"), "lang"), "en")

    manifest <- yyjsonr::read_json_file(file.path(dirname(r_dir), "dist", "manifest.json"))
    expected_css <- paste0("/static/", manifest[["css/app.css"]])
    links <- xml2::xml_attr(xml2::xml_find_all(doc, "//link[@rel='stylesheet']"), "href")
    expect_true(expected_css %in% links)

    expect_gt(length(xml2::xml_find_all(doc, "//div[@id='toasts']")), 0)
    expect_match(res$body, "Home", fixed = TRUE)
    expect_match(res$body, "dataset-count", fixed = TRUE)
    # The signed-in guest is visible in the user menu, with the logout link.
    expect_match(res$body, "guest", fixed = TRUE)
    expect_match(res$body, "/logout", fixed = TRUE)
})

test_that("/home localizes when the lang cookie is set to fr", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/home", headers = list(Cookie = paste(cookie, "lang=fr", sep = "; ")))

    doc <- xml2::read_html(res$body)
    expect_equal(xml2::xml_attr(xml2::xml_find_first(doc, "//html"), "lang"), "fr")
    expect_match(res$body, "Accueil", fixed = TRUE)
    expect_match(res$body, "Jeux de donn", fixed = TRUE)
})

test_that("/home under HX-Request returns a bare fragment with the HTML headers", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/home", headers = list(HX_Request = "true", Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_false(grepl("<html", res$body, fixed = TRUE))
    expect_false(grepl("<!DOCTYPE", res$body, fixed = TRUE))
    expect_match(res$body, "dataset-count", fixed = TRUE)
    expect_equal(res$headers[["vary"]], "HX-Request")
    expect_equal(res$headers[["cache-control"]], "private, no-store")
})

test_that("GET /lang/{code} sets a Lax lang cookie and redirects back without caching", {
    pa <- local_front_api()
    # A same-origin Referer is honored (reduced to its path + query).
    res <- do_request(pa, "http://t/lang/fr", headers = list(Referer = "http://t/explore?dataset=2"))

    expect_equal(res$status, 302L)
    expect_equal(res$headers[["location"]], "/explore?dataset=2")
    expect_equal(res$headers[["cache-control"]], "private, no-store")
    expect_true(any(grepl("^lang=fr", res$set_cookies)))
    expect_true(any(grepl("SameSite=Lax", res$set_cookies)))

    # A cross-origin Referer is never honored (open-redirect guard).
    res2 <- do_request(pa, "http://t/lang/en", headers = list(Referer = "http://evil.test/phish"))
    expect_equal(res2$status, 302L)
    expect_equal(res2$headers[["location"]], "/home")
})

test_that("responses carry the strict CSP and DENY frame options", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/home", headers = list(Cookie = cookie))

    csp <- res$headers[["content-security-policy"]]
    expect_match(csp, "script-src 'self'", fixed = TRUE)
    expect_match(csp, "script-src-attr 'none'", fixed = TRUE)
    expect_false(grepl("unsafe-inline", csp, fixed = TRUE))
    expect_false(grepl("unsafe-eval", csp, fixed = TRUE))
    expect_match(csp, "img-src 'self' data: https:", fixed = TRUE)
    expect_match(csp, "frame-ancestors 'none'", fixed = TRUE)
    expect_equal(res$headers[["x-frame-options"]], "DENY")
})

test_that("GET /lang/{code} rejects an unsupported code and redirects to /home", {
    pa <- local_front_api()
    res <- do_request(pa, "http://t/lang/de")

    expect_equal(res$status, 302L)
    expect_equal(res$headers[["location"]], "/home")
    expect_false(any(grepl("^lang=", res$set_cookies)))
})

test_that("the client-id cookie uses the configured dev name and SameSite=Lax", {
    pa <- local_front_api()
    res <- do_request(pa, "http://t/home")
    session_cookie <- grep("^fb_session=", res$set_cookies, value = TRUE)
    expect_length(session_cookie, 1L)
    expect_match(session_cookie, "HttpOnly")
    # Lax (not fiery's Strict) so the cookie survives the OIDC redirect back.
    expect_match(session_cookie, "SameSite=Lax")
})
