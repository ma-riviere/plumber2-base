# API-key management endpoints (JWT-only surface; the flow rules are covered in
# test-auth-endpoints.R). Focus here: issuance with scope bounding, the
# show-once secret, listing without secrets, revocation taking effect.

test_that("key issuance bounds scopes to caller scopes AND the key-safe allowlist", {
    ctx <- auth_api()
    admin_token <- sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|admin")
    user_token <- sign_access_token(ctx$fixture, roles = "user", sub = "auth0|plain")

    # An admin asking for everything still only gets the key-safe scopes.
    res <- do_json_request(
        ctx$pa,
        "http://t/v1/keys",
        "post",
        list(
            name = "greedy",
            scopes = list("write:datasets", "write:models", "manage:keys", "view:admin")
        ),
        headers = bearer_header(admin_token)
    )
    expect_equal(res$status, 201L)
    body <- yyjsonr::read_json_str(res$body)
    expect_setequal(body$scopes, c("write:datasets", "write:models"))
    expect_match(body$secret, API_KEY_PATTERN)

    # A plain user asking for admin-ish scopes gets the intersection with their own.
    res2 <- do_json_request(
        ctx$pa,
        "http://t/v1/keys",
        "post",
        list(name = "mine", scopes = list("write:datasets", "view:admin")),
        headers = bearer_header(user_token)
    )
    expect_equal(res2$status, 201L)
    expect_equal(yyjsonr::read_json_str(res2$body)$scopes, "write:datasets")

    # No scopes requested -> a valid read-only key.
    res3 <- do_json_request(
        ctx$pa,
        "http://t/v1/keys",
        "post",
        list(name = "readonly"),
        headers = bearer_header(user_token)
    )
    expect_equal(res3$status, 201L)
    expect_length(yyjsonr::read_json_str(res3$body)$scopes, 0L)
})

test_that("an issued key authenticates with exactly its bounded scopes", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, roles = "user", sub = "auth0|worker")
    created <- yyjsonr::read_json_str(
        do_json_request(
            ctx$pa,
            "http://t/v1/keys",
            "post",
            list(name = "workhorse", scopes = list("write:datasets")),
            headers = bearer_header(token)
        )$body
    )

    me <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = created$secret))$body
    )
    expect_equal(me$auth, "api_key")
    expect_equal(me$scopes, "write:datasets")
    # The key's user is the JWT user that minted it.
    expect_equal(me$user$auth0_sub, "auth0|worker")
})

test_that("listing keys returns metadata but never secrets; duplicates are 409", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, sub = "auth0|lister")
    do_json_request(
        ctx$pa,
        "http://t/v1/keys",
        "post",
        list(name = "k1"),
        headers = bearer_header(token)
    )
    dup <- do_json_request(
        ctx$pa,
        "http://t/v1/keys",
        "post",
        list(name = "k1"),
        headers = bearer_header(token)
    )
    expect_equal(dup$status, 409L)

    listed <- do_request(ctx$pa, "http://t/v1/keys", headers = bearer_header(token))
    expect_equal(listed$status, 200L)
    body <- yyjsonr::read_json_str(listed$body, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE)
    expect_length(body$items, 1L)
    expect_equal(body$items[[1]]$name, "k1")
    expect_match(body$items[[1]]$key_prefix, "^pbk_")
    expect_null(body$items[[1]]$secret)
    expect_false(grepl("pbk_[0-9a-f]{64}", listed$body))
})

test_that("revoking a key immediately stops it from authenticating", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, sub = "auth0|revoker")
    created <- yyjsonr::read_json_str(
        do_json_request(
            ctx$pa,
            "http://t/v1/keys",
            "post",
            list(name = "doomed", scopes = list("write:datasets")),
            headers = bearer_header(token)
        )$body
    )

    expect_equal(
        do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = created$secret))$status,
        200L
    )
    revoked <- do_request(
        ctx$pa,
        sprintf("http://t/v1/keys/%d", created$id),
        method = "delete",
        headers = bearer_header(token)
    )
    expect_equal(revoked$status, 204L)
    expect_equal(
        do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = created$secret))$status,
        403L
    )
    # Second revoke of the same key: no longer active -> 404.
    expect_equal(
        do_request(
            ctx$pa,
            sprintf("http://t/v1/keys/%d", created$id),
            method = "delete",
            headers = bearer_header(token)
        )$status,
        404L
    )
    # Another user cannot revoke someone else's key.
    other <- sign_access_token(ctx$fixture, sub = "auth0|other")
    created2 <- yyjsonr::read_json_str(
        do_json_request(
            ctx$pa,
            "http://t/v1/keys",
            "post",
            list(name = "safe"),
            headers = bearer_header(token)
        )$body
    )
    expect_equal(
        do_request(
            ctx$pa,
            sprintf("http://t/v1/keys/%d", created2$id),
            method = "delete",
            headers = bearer_header(other)
        )$status,
        404L
    )
})
