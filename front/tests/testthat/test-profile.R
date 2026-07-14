# Profile modal: guests are refused; an OIDC-authenticated user can edit the
# nickname (Auth0 Management API PATCH, sub enforced to the session's own) and
# the interface language (lang cookie + HX-Refresh).

test_that("guests cannot open or submit the profile modal", {
    pa <- local_front_api()
    session <- guest_session(pa)

    modal <- do_request(
        pa,
        "http://t/partials/profile",
        headers = list(Cookie = session$cookie, HX_Request = "true")
    )
    expect_equal(modal$status, 403L)

    save <- do_request(
        pa,
        "http://t/profile",
        method = "post",
        headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
        content = "nickname=hacker&language=en"
    )
    expect_equal(save$status, 403L)
})

test_that("an authenticated user edits nickname via the Management API and language via cookie", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    pa <- local_front_api(
        bypass = FALSE,
        auth0 = list(
            domain = tenant$base_url,
            mgmt_client_id = "m2m-client",
            mgmt_client_secret = "m2m-secret"
        )
    )
    use_fixture_jwks(fixture)
    reset_mgmt_cache()
    withr::defer(reset_mgmt_cache())

    # OIDC login (mirrors test-auth-flow): gate cookie -> /login -> /callback.
    r1 <- do_request(pa, "http://t/home")
    cookie <- extract_cookie(r1, "fb_session")
    r2 <- do_request(pa, "http://t/login?next=%2Fhome", headers = list(Cookie = cookie))
    q <- httr2::url_parse(r2$headers[["location"]])$query
    r3 <- do_request(
        pa,
        paste0("http://t/callback?state=", q$state, "&code=", utils::URLencode(q$nonce, reserved = TRUE)),
        headers = list(Cookie = cookie)
    )
    expect_equal(r3$status, 302L)
    token <- csrf_token_for(pa, cookie)
    headers <- list(
        Cookie = cookie,
        Origin = "http://t",
        X_CSRF_Token = token,
        Content_Type = "application/x-www-form-urlencoded"
    )

    # The modal renders the profile fields.
    modal <- do_request(pa, "http://t/partials/profile", headers = list(Cookie = cookie, HX_Request = "true"))
    expect_equal(modal$status, 200L)
    expect_match(modal$body, 'id="profile-modal"', fixed = TRUE)
    expect_match(modal$body, "user@example.test", fixed = TRUE)
    expect_match(modal$body, 'value="tester"', fixed = TRUE)

    # Nickname change: Management API PATCHed with the session's own sub,
    # navbar refreshed out-of-band, modal closed.
    saved <- do_request(
        pa,
        "http://t/profile",
        method = "post",
        headers = headers,
        content = "nickname=neo&language=en"
    )
    expect_equal(saved$status, 200L)
    expect_equal(saved$headers[["hx-trigger"]], "fb:close-modal")
    expect_match(saved$body, "Profile updated successfully", fixed = TRUE)
    expect_match(saved$body, 'id="navbar-user-name" hx-swap-oob="true"', fixed = TRUE)
    expect_match(saved$body, ">neo<")
    stats <- tenant$stats()
    expect_equal(stats$n_mgmt_patch, 1L)
    expect_equal(stats$last_patch$nickname, "neo")

    # An empty nickname is rejected.
    invalid <- do_request(pa, "http://t/profile", method = "post", headers = headers, content = "nickname=&language=en")
    expect_equal(invalid$status, 422L)

    # A language change sets the cookie and asks htmx for a full refresh.
    lang <- do_request(pa, "http://t/profile", method = "post", headers = headers, content = "nickname=neo&language=fr")
    expect_equal(lang$status, 200L)
    expect_equal(lang$headers[["hx-refresh"]], "true")
    expect_true(any(grepl("^lang=fr", lang$set_cookies)))
})
