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

test_that("/admin renders the users card grid by default for an admin session", {
    pa <- local_front_api(admin = TRUE)
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/admin", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$body, 'href="/admin?tab=users"', fixed = TRUE)
    expect_match(res$body, 'class="nav-link active" href="/admin?tab=users"', fixed = TRUE)
    # One card per user, guest badge on the guest, role badge on the dev.
    expect_match(res$body, 'id="admin-user-1"', fixed = TRUE)
    expect_match(res$body, 'id="admin-user-2"', fixed = TRUE)
    expect_match(res$body, ">Guest<")
    expect_match(res$body, ">dev<")
    # Role edit control only on the non-guest user.
    expect_match(res$body, 'hx-get="/partials/admin/users/2/role"', fixed = TRUE)
    expect_false(grepl('hx-get="/partials/admin/users/1/role"', res$body, fixed = TRUE))
    # All/recently-active filter links.
    expect_match(res$body, 'href="/admin?tab=users&amp;seen=recent"', fixed = TRUE)
    # Admin nav entry appears for the granted scope.
    expect_match(res$body, 'href="/admin"', fixed = TRUE)
})

test_that("/admin serves the requests tab; the removed sessions tab falls back to users", {
    pa <- local_front_api(admin = TRUE)
    cookie <- guest_cookie(pa)

    fallback <- do_request(pa, "http://t/admin?tab=sessions", headers = list(Cookie = cookie))
    expect_equal(fallback$status, 200L)
    expect_match(fallback$body, 'class="nav-link active" href="/admin?tab=users"', fixed = TRUE)
    expect_false(grepl("fe_sessions", fallback$body, fixed = TRUE))

    requests <- do_request(pa, "http://t/admin?tab=requests&hours=168", headers = list(Cookie = cookie))
    expect_equal(requests$status, 200L)
    expect_match(requests$body, "/v1/datasets", fixed = TRUE)
    # The active window button reflects ?hours= (htmltools escapes & in attrs).
    expect_match(requests$body, 'btn btn-sm btn-secondary" href="/admin?tab=requests&amp;hours=168"', fixed = TRUE)
})

test_that("the role modal offers the tenant roles with the current one selected", {
    pa <- local_front_api(admin = TRUE)
    cookie <- guest_cookie(pa)
    res <- do_request(
        pa,
        "http://t/partials/admin/users/2/role",
        headers = list(Cookie = cookie, HX_Request = "true")
    )

    expect_equal(res$status, 200L)
    expect_match(res$body, 'id="role-modal"', fixed = TRUE)
    expect_match(res$body, 'hx-put="/admin/users/2/role"', fixed = TRUE)
    expect_match(res$body, 'value="rol_dev" selected', fixed = TRUE)
    # Empty value = clear back to the default role; unmapped roles are flagged.
    expect_match(res$body, 'value=""', fixed = TRUE)
    expect_match(res$body, "user (default)", fixed = TRUE)
    expect_match(res$body, "beta (no scopes mapped)", fixed = TRUE)
    expect_match(res$body, "next token refresh", fixed = TRUE)
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

test_that("/account lists keys with prefix, revoked badge and revoke button", {
    pa <- local_front_api()
    cookie <- guest_cookie(pa)
    res <- do_request(pa, "http://t/account", headers = list(Cookie = cookie))

    expect_equal(res$status, 200L)
    expect_match(res$body, "pbk_abcd", fixed = TRUE)
    expect_match(res$body, "Revoked", fixed = TRUE)
    expect_match(res$body, 'hx-delete="/keys/9"', fixed = TRUE)
    expect_match(res$body, 'id="create-key-form"', fixed = TRUE)
    # Key-safe scopes offered as checkboxes.
    expect_match(res$body, 'value="write:datasets"', fixed = TRUE)
    expect_match(res$body, 'value="write:models"', fixed = TRUE)
})

test_that("creating a key shows the secret once and refreshes the table oob", {
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
})

test_that("a duplicate key name surfaces the backend 409 as an alert", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(
        pa,
        "http://t/keys",
        method = "post",
        headers = c(
            action_headers(session, Content_Type = "application/x-www-form-urlencoded"),
            list(HX_Request = "true")
        ),
        content = "name=dup"
    )

    expect_equal(res$status, 409L)
    expect_match(res$body, "already exists", fixed = TRUE)
})

test_that("revoking a key toasts and refreshes the table oob", {
    pa <- local_front_api()
    session <- guest_session(pa)
    res <- do_request(pa, "http://t/keys/9", method = "delete", headers = action_headers(session))

    expect_equal(res$status, 200L)
    expect_match(res$body, "API key revoked", fixed = TRUE)
    expect_match(res$body, 'id="keys-table" hx-swap-oob="true"', fixed = TRUE)
})
