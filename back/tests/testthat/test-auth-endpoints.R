# End-to-end auth through the real assembled api (constructor + route files +
# central /v1/* rules), in-process. Fixture JWKS, scratch-schema pool. The status
# contract (RFC 6750 via auth0r 0.4.0's revalidating bearer guard): no
# credential -> 401 with a bare Bearer challenge (bearer reject runs last in the
# api_key || jwt flow), an attempted-but-invalid bearer token -> 401 with
# error="invalid_token", an invalid API key -> 403 (fireproof's key contract,
# untouched by the bearer override), valid -> 200.

test_that("dev api does not trust X-Forwarded-* headers (prod-only, behind Traefik)", {
    ctx <- auth_api()
    expect_false(ctx$pa$trust)
})

test_that("/v1/me without credentials is 401 with a Bearer challenge", {
    ctx <- auth_api()
    res <- do_request(ctx$pa, "http://t/v1/me")
    expect_equal(res$status, 401L)
    expect_match(res$headers[["www-authenticate"]], "Bearer")
})

test_that("/v1/me with an invalid or forged API key is 403", {
    ctx <- auth_api()
    unknown <- generate_api_key() # valid format, not in the database
    expect_equal(do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = unknown))$status, 403L)
    expect_equal(do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = "garbage"))$status, 403L)
})

test_that("/v1/me with a valid API key returns the owner, key scopes and auth kind", {
    ctx <- auth_api()
    user_id <- DBI::dbGetQuery(
        ctx$pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('owner', false) RETURNING id"
    )$id
    key <- create_api_key(ctx$pool, user_id, "test-key", scopes = "write:datasets")

    res <- do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = key$secret))

    expect_equal(res$status, 200L)
    body <- yyjsonr::read_json_str(res$body)
    expect_equal(body$auth, "api_key")
    expect_equal(body$user$id, as.integer(user_id)) # bigint comes back as integer64
    expect_equal(body$scopes, "write:datasets")
})

test_that("/v1/me with a valid JWT provisions the user and maps roles to scopes", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|jwt-user")

    res <- do_request(ctx$pa, "http://t/v1/me", headers = bearer_header(token))

    expect_equal(res$status, 200L)
    body <- yyjsonr::read_json_str(res$body)
    expect_equal(body$auth, "jwt")
    expect_equal(body$user$auth0_sub, "auth0|jwt-user")
    expect_false(body$user$is_guest)
    expect_setequal(
        body$scopes,
        c("write:datasets", "write:models", "manage:keys", "view:admin", "manage:admin:roles")
    )

    # The user row was created; a second call reuses it.
    res2 <- do_request(ctx$pa, "http://t/v1/me", headers = bearer_header(token))
    expect_equal(yyjsonr::read_json_str(res2$body)$user$id, body$user$id)
    n <- DBI::dbGetQuery(
        ctx$pool,
        "SELECT count(*) AS n FROM users WHERE auth0_sub = 'auth0|jwt-user'"
    )$n
    expect_equal(as.integer(n), 1L)
})

test_that("a role-less JWT gets the default user scopes (parity choice)", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, roles = character(), sub = "auth0|norole")
    res <- do_request(ctx$pa, "http://t/v1/me", headers = bearer_header(token))
    expect_equal(res$status, 200L)
    expect_setequal(
        yyjsonr::read_json_str(res$body)$scopes,
        c("write:datasets", "write:models", "manage:keys")
    )
})

test_that("expired, unverified-email and wrong-audience JWTs are 401 invalid_token", {
    ctx <- auth_api()
    expired <- sign_access_token(ctx$fixture, exp = as.numeric(Sys.time()) - 120)
    unverified <- sign_access_token(ctx$fixture, email_verified = FALSE)
    no_verified_claim <- sign_access_token(ctx$fixture, email_verified = NULL)
    wrong_aud <- sign_access_token(ctx$fixture, aud = "https://other.api")

    for (bad in list(expired, unverified, no_verified_claim, wrong_aud)) {
        res <- do_request(ctx$pa, "http://t/v1/me", headers = bearer_header(bad))
        expect_equal(res$status, 401L)
        expect_match(res$headers[["www-authenticate"]], 'error="invalid_token"', fixed = TRUE)
    }
})

test_that("JWTs missing client_id or jti are 401 invalid_token (rfc9068 profile)", {
    ctx <- auth_api()
    no_client_id <- sign_access_token(ctx$fixture, client_id = NULL)
    no_jti <- sign_access_token(ctx$fixture, jti = NULL)

    for (bad in list(no_client_id, no_jti)) {
        res <- do_request(ctx$pa, "http://t/v1/me", headers = bearer_header(bad))
        expect_equal(res$status, 401L)
        expect_match(res$headers[["www-authenticate"]], 'error="invalid_token"', fixed = TRUE)
    }
})

test_that("authentication is re-validated per request (no session-cookie replay)", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, sub = "auth0|replay")

    first <- do_request(ctx$pa, "http://t/v1/me", headers = bearer_header(token))
    expect_equal(first$status, 200L)
    session_cookie <- sub(";.*$", "", first$headers[["set-cookie"]])
    expect_match(session_cookie, "=")

    # Same fiery session cookie, no Authorization header: stock fireproof would
    # serve the cached authentication; the revalidating guards must 401.
    replay <- do_request(ctx$pa, "http://t/v1/me", headers = list(Cookie = session_cookie))
    expect_equal(replay$status, 401L)
})

test_that("the /v1/keys rules are JWT-only and /v1 is default-deny", {
    ctx <- auth_api()
    user_id <- DBI::dbGetQuery(
        ctx$pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('ko', false) RETURNING id"
    )$id
    key <- create_api_key(ctx$pool, user_id, "k", scopes = "write:datasets")
    token <- sign_access_token(ctx$fixture)

    # A valid API key does NOT satisfy the keys endpoints' jwt-only flow.
    expect_equal(do_request(ctx$pa, "http://t/v1/keys", headers = list(X_API_Key = key$secret))$status, 401L)
    expect_equal(do_request(ctx$pa, "http://t/v1/keys/1", headers = list(X_API_Key = key$secret))$status, 401L)
    # A valid JWT passes the rule and reaches the handler.
    expect_equal(do_request(ctx$pa, "http://t/v1/keys", headers = bearer_header(token))$status, 200L)

    # Default-deny: an unknown /v1 path requires auth before it can 404.
    expect_equal(do_request(ctx$pa, "http://t/v1/definitely-not-here")$status, 401L)
    expect_equal(
        do_request(ctx$pa, "http://t/v1/definitely-not-here", headers = bearer_header(token))$status,
        404L
    )
})

test_that("/v1/ping is now behind auth like everything under /v1", {
    ctx <- auth_api()
    expect_equal(do_request(ctx$pa, "http://t/v1/ping")$status, 401L)
    token <- sign_access_token(ctx$fixture)
    res <- do_request(ctx$pa, "http://t/v1/ping", headers = bearer_header(token))
    expect_equal(res$status, 200L)
    expect_match(yyjsonr::read_json_str(res$body)$pong, "^\\d{4}-\\d{2}-\\d{2}T")
})

test_that("bypass mode authenticates everything as the guest with user scopes", {
    ctx <- auth_api(bypass = TRUE)

    res <- do_request(ctx$pa, "http://t/v1/me")
    expect_equal(res$status, 200L)
    body <- yyjsonr::read_json_str(res$body)
    expect_equal(body$auth, "bypass")
    expect_true(body$user$is_guest)
    expect_setequal(body$scopes, c("write:datasets", "write:models", "manage:keys"))

    # Real credentials still win over the bypass principal when presented.
    user_id <- DBI::dbGetQuery(
        ctx$pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('real', false) RETURNING id"
    )$id
    key <- create_api_key(ctx$pool, user_id, "real-key", scopes = "write:models")
    with_key <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = key$secret))$body
    )
    expect_equal(with_key$auth, "api_key")
    expect_equal(with_key$user$id, as.integer(user_id))
})

test_that("without bypass mode the bypass guard is absent from the router", {
    ctx <- auth_api(bypass = FALSE)
    # If the guard were registered and always-pass, this would be 200.
    expect_equal(do_request(ctx$pa, "http://t/v1/me")$status, 401L)
})

test_that("every guard's OpenAPI scheme has a type (prune_openapi crashes otherwise)", {
    # Regression: plumber2's ignite runs prune_openapi over all registered
    # security schemes; a scheme without $type (base Guard default) aborts the
    # server at startup. Exercise the exact vapply that failed.
    schemes <- list(
        jwt = jwt_guard(
            list(auth0 = list(domain = "d", audience = "a", claim_namespace = "ns/")),
            load_permissions(file.path(BACK_DIR, "permissions.yaml"))
        )$open_api,
        api_key = api_key_guard()$open_api,
        bypass = BypassGuard$new(character())$open_api
    )
    types <- vapply(schemes, function(x) x$type %in% c("oauth2", "openIdConnect"), logical(1))
    expect_length(types, 3L)
})
