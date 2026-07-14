# Admin Auth0 role endpoints, exercised with an injected fake management
# client (back/R/mgmt.R test seam): listing gates on view:admin, changes on
# manage:admin:roles (admin-only), guests and self-demotion are refused.

fake_mgmt_client <- function(user_roles = list()) {
    calls <- new.env(parent = emptyenv())
    roles <- list(
        list(id = "rol_admin", name = "admin", description = "Administrator"),
        list(id = "rol_dev", name = "dev", description = "Developer"),
        list(id = "rol_beta", name = "beta", description = "Beta tester")
    )
    list(
        calls = calls,
        list_roles = function(...) roles,
        get_role = function(role_id) {
            hit <- Filter(function(role) identical(role$id, role_id), roles)
            if (length(hit) == 0) {
                stop("no such role")
            }
            hit[[1]]
        },
        get_user_roles = function(sub, full = FALSE) {
            current <- user_roles[[sub]] %||% list()
            if (full) current else vapply(current, function(role) role$name, character(1))
        },
        remove_user_roles = function(sub, ids) {
            calls$removed <- c(calls$removed, ids)
            invisible()
        },
        assign_user_roles = function(sub, ids) {
            calls$assigned <- c(calls$assigned, ids)
            invisible()
        }
    )
}

put_role <- function(ctx, headers, user_id, body) {
    do_request(
        ctx$pa,
        sprintf("http://t/v1/admin/users/%d/role", as.integer(user_id)),
        method = "put",
        headers = c(headers, list(Content_Type = "application/json")),
        content = body
    )
}

test_that("role endpoints gate on scopes: view:admin to list, manage:admin:roles to change", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_mgmt_client())

    user <- bearer_header(sign_access_token(ctx$fixture, roles = "user", sub = "auth0|plain"))
    dev <- bearer_header(sign_access_token(ctx$fixture, roles = "dev", sub = "auth0|dev"))

    expect_equal(do_request(ctx$pa, "http://t/v1/admin/roles", headers = user)$status, 403L)
    expect_equal(do_request(ctx$pa, "http://t/v1/admin/roles", headers = dev)$status, 200L)
    # dev may view the admin panel but not manage roles (shiny-base parity).
    expect_equal(put_role(ctx, dev, 1L, '{"role_id": "rol_dev"}')$status, 403L)
})

test_that("GET /v1/admin/roles flags yaml-mapped roles and reports the default role", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_mgmt_client())
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))

    body <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/admin/roles", headers = admin)$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_equal(body$default_role, "user")
    expect_equal(vapply(body$items, function(role) role$name, character(1)), c("admin", "dev", "beta"))
    expect_equal(vapply(body$items, function(role) role$in_yaml, logical(1)), c(TRUE, TRUE, FALSE))
})

test_that("role endpoints answer 503 when no mgmt client is configured; user listing degrades", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)

    expect_equal(do_request(ctx$pa, "http://t/v1/admin/roles", headers = admin)$status, 503L)
    users <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/admin/users", headers = admin)$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_null(users$items[[1]]$role)
})

test_that("the admin user listing carries each user's Auth0 role", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_mgmt_client(
        user_roles = list("auth0|root" = list(list(id = "rol_admin", name = "admin")))
    ))
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)

    users <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/admin/users", headers = admin)$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_equal(users$items[[1]]$auth0_sub, "auth0|root")
    expect_equal(users$items[[1]]$role, "admin")
})

test_that("PUT role replaces the target's roles; guests, bad ids and self-demotion are refused", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    client <- fake_mgmt_client(
        user_roles = list(
            "auth0|root" = list(list(id = "rol_admin", name = "admin")),
            "auth0|bob" = list(list(id = "rol_dev", name = "dev"))
        )
    )
    set_mgmt_client(client)
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    bob <- bearer_header(sign_access_token(ctx$fixture, roles = "dev", sub = "auth0|bob"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)
    do_request(ctx$pa, "http://t/v1/me", headers = bob)
    ids <- DBI::dbGetQuery(ctx$pool, "SELECT id, auth0_sub FROM users ORDER BY id")
    root_id <- ids$id[ids$auth0_sub == "auth0|root"]
    bob_id <- ids$id[ids$auth0_sub == "auth0|bob"]

    promoted <- put_role(ctx, admin, bob_id, '{"role_id": "rol_admin"}')
    expect_equal(promoted$status, 200L)
    promoted_body <- yyjsonr::read_json_str(promoted$body, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE)
    expect_equal(promoted_body$role, "admin")
    expect_equal(client$calls$removed, "rol_dev")
    expect_equal(client$calls$assigned, "rol_admin")

    cleared <- put_role(ctx, admin, bob_id, '{"role_id": ""}')
    expect_equal(cleared$status, 200L)
    expect_null(yyjsonr::read_json_str(cleared$body, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE)$role)

    expect_equal(put_role(ctx, admin, bob_id, '{"role_id": "rol_nope"}')$status, 422L)
    expect_equal(put_role(ctx, admin, 99999L, '{"role_id": "rol_dev"}')$status, 404L)

    guest_id <- DBI::dbGetQuery(
        ctx$pool,
        "INSERT INTO users (nickname, is_guest, last_seen_at) VALUES ('guest', true, now()) RETURNING id"
    )$id
    expect_equal(put_role(ctx, admin, guest_id, '{"role_id": "rol_dev"}')$status, 422L)

    # Self-demotion lockout guard: root cannot drop its own admin role.
    self <- put_role(ctx, admin, root_id, '{"role_id": "rol_dev"}')
    expect_equal(self$status, 409L)
    expect_false("rol_admin" %in% client$calls$removed)
})
