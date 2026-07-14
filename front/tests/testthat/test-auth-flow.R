# OIDC flow, CSRF enforcement and guest mode through the real assembled api,
# with a webfakes stand-in for Auth0's token endpoint (the /authorize browser
# leg is simulated by parsing the redirect URL, as a browser would follow it).
# Skipped when the dev Postgres is unreachable (scratch-schema datastore).

login_redirect <- function(pa, cookie) {
    res <- do_request(pa, "http://t/login?next=%2Fhome", headers = list(Cookie = cookie))
    testthat::expect_equal(res$status, 302L)
    httr2::url_parse(res$headers[["location"]])$query
}

test_that("the full OIDC login round-trips: gate, authorize, callback, session", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    pa <- local_front_api(bypass = FALSE, auth0 = list(domain = tenant$base_url))
    use_fixture_jwks(fixture)

    # Protected page -> login redirect carrying the target; session cookie set.
    r1 <- do_request(pa, "http://t/home")
    expect_equal(r1$status, 302L)
    expect_equal(r1$headers[["location"]], "/login?next=%2Fhome")
    cookie <- extract_cookie(r1, "fb_session")
    expect_false(is.null(cookie))

    # /login -> tenant /authorize with the full OIDC parameter set.
    r2 <- do_request(pa, "http://t/login?next=%2Fhome", headers = list(Cookie = cookie))
    expect_equal(r2$status, 302L)
    auth_url <- r2$headers[["location"]]
    expect_match(auth_url, paste0("^", tenant$base_url, "/authorize\\?"))
    q <- httr2::url_parse(auth_url)$query
    expect_equal(q$code_challenge_method, "S256")
    expect_equal(q$audience, "https://base-api.test")

    # Callback with the state echo; the code carries the nonce (test protocol).
    r3 <- do_request(
        pa,
        paste0("http://t/callback?state=", q$state, "&code=", utils::URLencode(q$nonce, reserved = TRUE)),
        headers = list(Cookie = cookie)
    )
    expect_equal(r3$status, 302L)
    expect_equal(r3$headers[["location"]], "/home")

    # The session is live: the page renders the user and a CSRF meta token.
    r4 <- do_request(pa, "http://t/home", headers = list(Cookie = cookie))
    expect_equal(r4$status, 200L)
    expect_match(r4$body, "tester", fixed = TRUE)
    expect_match(r4$body, "/logout", fixed = TRUE)
    token <- regmatches(r4$body, regexec('name="csrf-token" content="([^"]*)"', r4$body))[[1]][2]
    expect_match(token, "^[0-9a-f]+\\.[0-9a-f]+$")
    expect_false(is.null(extract_cookie(r4, "fb_csrf")))

    # Logout revokes the refresh token, clears the session, hits Auth0's logout.
    # It is a CSRF-gated POST now (a public GET logout would be CSRFable).
    r5 <- do_request(
        pa,
        "http://t/logout",
        method = "post",
        headers = list(Cookie = cookie, Origin = "http://t", X_CSRF_Token = token)
    )
    expect_equal(r5$status, 302L)
    expect_match(r5$headers[["location"]], paste0("^", tenant$base_url, "/v2/logout\\?client_id=fe-client"))
    expect_match(r5$headers[["clear-site-data"]], "cookies")
    expect_equal(tenant$stats()$n_revoke, 1L)
    expect_equal(do_request(pa, "http://t/home", headers = list(Cookie = cookie))$status, 302L)
})

test_that("a state mismatch fails the login and burns the stored OIDC state", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    pa <- local_front_api(bypass = FALSE, auth0 = list(domain = tenant$base_url))
    use_fixture_jwks(fixture)

    r1 <- do_request(pa, "http://t/home")
    cookie <- extract_cookie(r1, "fb_session")
    q <- login_redirect(pa, cookie)

    bad <- do_request(
        pa,
        paste0("http://t/callback?state=WRONG&code=", utils::URLencode(q$nonce, reserved = TRUE)),
        headers = list(Cookie = cookie)
    )
    expect_equal(bad$status, 403L)
    expect_match(bad$body, "Login failed", fixed = TRUE)
    # One-shot: replaying the correct state after the failure is also rejected.
    replay <- do_request(
        pa,
        paste0("http://t/callback?state=", q$state, "&code=", utils::URLencode(q$nonce, reserved = TRUE)),
        headers = list(Cookie = cookie)
    )
    expect_equal(replay$status, 403L)
    expect_equal(tenant$stats()$n_token, 0L)
})

test_that("an ID token with the wrong nonce is rejected", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    pa <- local_front_api(bypass = FALSE, auth0 = list(domain = tenant$base_url))
    use_fixture_jwks(fixture)

    cookie <- extract_cookie(do_request(pa, "http://t/home"), "fb_session")
    q <- login_redirect(pa, cookie)
    res <- do_request(
        pa,
        paste0("http://t/callback?state=", q$state, "&code=some-other-nonce"),
        headers = list(Cookie = cookie)
    )
    expect_equal(res$status, 403L)
    expect_match(res$body, "Login failed", fixed = TRUE)
})

test_that("an unverified email is gated to /unverified without a session", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    pa <- local_front_api(bypass = FALSE, auth0 = list(domain = tenant$base_url))
    use_fixture_jwks(fixture)

    cookie <- extract_cookie(do_request(pa, "http://t/home"), "fb_session")
    q <- login_redirect(pa, cookie)
    res <- do_request(
        pa,
        paste0(
            "http://t/callback?state=",
            q$state,
            "&code=",
            utils::URLencode(paste0("unverified:", q$nonce), reserved = TRUE)
        ),
        headers = list(Cookie = cookie)
    )
    expect_equal(res$status, 302L)
    expect_equal(res$headers[["location"]], "/unverified")
    # No session was created; the gate page itself is public.
    expect_equal(do_request(pa, "http://t/home", headers = list(Cookie = cookie))$status, 302L)
    page <- do_request(pa, "http://t/unverified", headers = list(Cookie = cookie))
    expect_equal(page$status, 200L)
    expect_match(page$body, "Email verification required", fixed = TRUE)
})

test_that("an unauthenticated htmx request gets 200 + HX-Redirect, not a 302", {
    pa <- local_front_api()
    res <- do_request(pa, "http://t/home", headers = list(HX_Request = "true"))
    expect_equal(res$status, 200L)
    expect_equal(res$headers[["hx-redirect"]], "/login")
})

test_that("the gate enforces the CSRF token and Origin on state-changing requests", {
    pa <- local_front_api()
    add_csrf_probe(pa)
    cookie <- guest_cookie(pa)
    token <- csrf_token_for(pa, cookie)

    post_probe <- function(headers) {
        do_request(pa, "http://t/csrf-probe", method = "post", headers = headers)
    }

    ok <- post_probe(list(Cookie = cookie, Origin = "http://t", X_CSRF_Token = token))
    expect_equal(ok$status, 200L)
    expect_equal(ok$body, "probe-ok")
    # Referer works as the Origin fallback.
    expect_equal(post_probe(list(Cookie = cookie, Referer = "http://t/home", X_CSRF_Token = token))$status, 200L)

    expect_equal(post_probe(list(Cookie = cookie, Origin = "http://t"))$status, 403L)
    expect_equal(post_probe(list(Cookie = cookie, Origin = "http://t", X_CSRF_Token = "aa.bb"))$status, 403L)
    expect_equal(post_probe(list(Cookie = cookie, Origin = "http://evil.test", X_CSRF_Token = token))$status, 403L)
    # Neither Origin nor Referer: fail closed.
    expect_equal(post_probe(list(Cookie = cookie, X_CSRF_Token = token))$status, 403L)

    # A token stolen from another session does not transfer.
    other_cookie <- guest_cookie(pa)
    other_token <- csrf_token_for(pa, other_cookie)
    expect_equal(post_probe(list(Cookie = cookie, Origin = "http://t", X_CSRF_Token = other_token))$status, 403L)

    # Unauthenticated POSTs never reach the CSRF check (no session -> redirect).
    expect_equal(do_request(pa, "http://t/csrf-probe", method = "post")$status, 302L)
})

test_that("guest mode logs in end-to-end without Auth0 and logout kills the session", {
    pa <- local_front_api()

    r1 <- do_request(pa, "http://t/home")
    expect_equal(r1$status, 302L)
    expect_equal(r1$headers[["location"]], "/login?next=%2Fhome")
    cookie <- extract_cookie(r1, "fb_session")

    r2 <- do_request(pa, "http://t/login?next=%2Fhome", headers = list(Cookie = cookie))
    expect_equal(r2$status, 302L)
    expect_equal(r2$headers[["location"]], "/home")

    r3 <- do_request(pa, "http://t/home", headers = list(Cookie = cookie))
    expect_equal(r3$status, 200L)
    expect_match(r3$body, "guest", fixed = TRUE)
    token <- regmatches(r3$body, regexec('name="csrf-token" content="([^"]*)"', r3$body))[[1]][2]

    r4 <- do_request(
        pa,
        "http://t/logout",
        method = "post",
        headers = list(Cookie = cookie, Origin = "http://t", X_CSRF_Token = token)
    )
    expect_equal(r4$status, 302L)
    expect_equal(r4$headers[["location"]], "/login")
    expect_match(r4$headers[["clear-site-data"]], "cookies")

    expect_equal(do_request(pa, "http://t/home", headers = list(Cookie = cookie))$status, 302L)
})

test_that("a malicious ?next= target never leaves the app", {
    pa <- local_front_api()
    res <- do_request(pa, "http://t/login?next=https%3A%2F%2Fevil.test")
    expect_equal(res$status, 302L)
    expect_equal(res$headers[["location"]], "/home")
    res2 <- do_request(pa, "http://t/login?next=%2F%2Fevil.test")
    expect_equal(res2$status, 302L)
    expect_equal(res2$headers[["location"]], "/home")
})
