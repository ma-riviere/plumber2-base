# Admin and Account pages through the real assembled api (guest mode, fake
# backend). `admin = TRUE` makes the fake /v1/me grant view:admin, which
# session_scopes caches into the session.

test_that("/admin without view:admin answers 403 with the access-denied page", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/admin", headers = list(Cookie = cookie))

    expect_equal(res$status, 403L)
    expect_match(res$body, "Access denied", fixed = TRUE)
})

test_that("a role change refreshes the user card out-of-band and closes the modal", {
    pa <- local_front_api(admin = TRUE)
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/admin/users/2/role",
        method = "put",
        headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
        content = "role_id=rol_admin"
    )

    expect_equal(res$status, 200L)
    expect_match(res$headers[["hx-trigger"]], "fb:close-modal")
    expect_match(res$body, 'id="admin-user-2" hx-swap-oob="true"', fixed = TRUE)
    expect_match(res$body, "Role updated", fixed = TRUE)
})

test_that("backend role-change rejections surface inside the modal", {
    pa <- local_front_api(admin = TRUE)
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/admin/users/1/role",
        method = "put",
        headers = c(
            action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
            list(HX_Request = "true")
        ),
        content = "role_id=rol_admin"
    )

    expect_equal(res$status, 422L)
    expect_match(res$body, "alert-danger", fixed = TRUE)
    expect_match(res$body, "guest users have no Auth0 identity", fixed = TRUE)
})

test_that("creating a key shows the secret once; a duplicate name surfaces the 409", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/keys",
        method = "post",
        headers = action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
        content = "name=ci-key&scopes=write%3Adatasets"
    )

    expect_equal(res$status, 200L)
    expect_match(res$body, paste0("pbk_", strrep("f0", 32)), fixed = TRUE)
    expect_match(res$body, "shown only once", fixed = TRUE)
    expect_match(res$body, "data-clipboard-text", fixed = TRUE)
    expect_match(res$body, 'id="keys-table" hx-swap-oob="true"', fixed = TRUE)

    dup <- do_request(
        pa,
        "http://t/keys",
        method = "post",
        headers = c(
            action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
            list(HX_Request = "true")
        ),
        content = "name=dup"
    )
    expect_equal(dup$status, 409L)
    expect_match(dup$body, "already exists", fixed = TRUE)
})

test_that("revoking a key toasts and refreshes the table oob", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(pa, "http://t/keys/9", method = "delete", headers = action_headers(session))

    expect_equal(res$status, 200L)
    expect_match(res$body, "API key revoked", fixed = TRUE)
    expect_match(res$body, 'id="keys-table" hx-swap-oob="true"', fixed = TRUE)
})
