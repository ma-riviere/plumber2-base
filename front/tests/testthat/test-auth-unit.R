# Unit tests for the auth building blocks: CSRF tokens, secret encryption, the
# session cookie converter, next-path validation, the authorize URL and the ID
# token validation matrix (local RSA fixtures, no network).

test_that("csrf tokens verify only for their session and key", {
    key <- derive_key(TEST_SESSION_KEY, "csrf")
    token <- issue_csrf_token("session-a", key)
    expect_true(verify_csrf_token(token, "session-a", key))
    # Cross-session, forged and malformed tokens all fail.
    expect_false(verify_csrf_token(token, "session-b", key))
    expect_false(verify_csrf_token(token, "session-a", derive_key("other-key", "csrf")))
    parts <- strsplit(token, ".", fixed = TRUE)[[1]]
    forged <- paste0(parts[1], ".", paste(rev(strsplit(parts[2], "")[[1]]), collapse = ""))
    expect_false(verify_csrf_token(forged, "session-a", key))
    expect_false(verify_csrf_token("", "session-a", key))
    expect_false(verify_csrf_token("no-dot", "session-a", key))
    expect_false(verify_csrf_token(NULL, "session-a", key))
})

test_that("origin_allowed accepts same-origin Origin/Referer and rejects the rest", {
    origin <- app_origin("http://t")
    req_with <- function(headers) reqres::Request$new(fiery::fake_request("http://t/x", headers = headers))
    expect_true(origin_allowed(req_with(list(Origin = "http://t")), origin))
    expect_false(origin_allowed(req_with(list(Origin = "http://evil.test")), origin))
    expect_true(origin_allowed(req_with(list(Referer = "http://t/home")), origin))
    expect_false(origin_allowed(req_with(list(Referer = "http://evil.test/home")), origin))
    # Neither header present: fail closed.
    expect_false(origin_allowed(req_with(list()), origin))
})

test_that("secrets round-trip through secretbox and fail with the wrong key", {
    key <- refresh_key(test_config())
    enc <- encrypt_secret("rt-secret-1", key)
    expect_equal(decrypt_secret(enc, key), "rt-secret-1")
    expect_false(identical(enc$cipher, encrypt_secret("rt-secret-1", key)$cipher))
    expect_error(decrypt_secret(enc, derive_key("other", "refresh-token")))
})

test_that("the session cookie converter emits Lax cookies and reuses the id", {
    converter <- session_cookie_converter("fb_session", secure = FALSE)
    req <- reqres::Request$new(fiery::fake_request("http://t/home"))
    id <- converter(req)
    expect_true(nzchar(id))
    cookie <- req$respond()$as_list()$headers[["set-cookie"]]
    expect_match(cookie, "^fb_session=")
    expect_match(cookie, "SameSite=Lax")
    expect_match(cookie, "HttpOnly")
    # A request that already carries the cookie keeps its id.
    req2 <- reqres::Request$new(fiery::fake_request(
        "http://t/home",
        headers = list(Cookie = paste0("other=1; fb_session=", id))
    ))
    expect_equal(converter(req2), id)
})

test_that("the prod converter emits a Secure __Host- cookie", {
    converter <- session_cookie_converter("__Host-session", secure = TRUE)
    req <- reqres::Request$new(fiery::fake_request("http://t/home"))
    converter(req)
    cookie <- req$respond()$as_list()$headers[["set-cookie"]]
    expect_match(cookie, "^__Host-session=")
    expect_match(cookie, "Secure")
    expect_match(cookie, "Path=/")
    expect_match(cookie, "SameSite=Lax")
})

test_that("is_safe_next only accepts local absolute paths", {
    expect_true(is_safe_next("/home"))
    expect_true(is_safe_next("/explore?dataset=1"))
    expect_false(is_safe_next("//evil.test/phish"))
    expect_false(is_safe_next("https://evil.test"))
    expect_false(is_safe_next("home"))
    expect_false(is_safe_next(""))
    expect_false(is_safe_next(NULL))
    expect_false(is_safe_next(NA_character_))
})

test_that("the authorize URL carries the full OIDC + PKCE parameter set", {
    config <- test_config()
    url <- build_authorize_url(config, state = "st-1", nonce = "no-1", challenge = "ch-1")
    expect_match(url, "^https://tenant.test/authorize\\?")
    query <- httr2::url_parse(url)$query
    expect_equal(query$response_type, "code")
    expect_equal(query$client_id, "fe-client")
    expect_equal(query$redirect_uri, "http://t/callback")
    expect_equal(query$scope, "openid profile email offline_access")
    expect_equal(query$audience, "https://base-api.test")
    expect_equal(query$state, "st-1")
    expect_equal(query$nonce, "no-1")
    expect_equal(query$code_challenge, "ch-1")
    expect_equal(query$code_challenge_method, "S256")
})

test_that("pkce_challenge is the base64url SHA-256 of the verifier", {
    verifier <- "test-verifier"
    expect_equal(
        pkce_challenge(verifier),
        jose::base64url_encode(openssl::sha256(charToRaw(verifier)))
    )
})

test_that("a valid ID token yields its claims", {
    fixture <- new_jwt_fixture()
    use_fixture_jwks(fixture)
    config <- test_config()
    iss <- auth0_issuer(config$auth0$domain)

    token <- sign_id_token(fixture, iss = iss, nonce = "no-1", roles = "admin")
    claims <- validate_id_token(token, config, expected_nonce = "no-1")
    expect_equal(claims$sub, "auth0|fe-user")
    expect_equal(claims$nickname, "tester")
    expect_true(claims$email_verified)
    expect_equal(claims[[paste0(TEST_CLAIM_NS, "roles")]], "admin")
})

test_that("each forged or invalid ID token variant is rejected with its reason", {
    fixture <- new_jwt_fixture()
    use_fixture_jwks(fixture)
    config <- test_config()
    iss <- auth0_issuer(config$auth0$domain)
    now <- as.numeric(Sys.time())
    sign <- function(...) sign_id_token(fixture, iss = iss, nonce = "no-1", ...)
    reject <- function(token, reason) expect_error(validate_id_token(token, config, "no-1"), reason)

    expect_error(validate_id_token("not-a-jwt", config, "no-1"), "malformed")
    reject(sign_id_token(fixture, iss = "https://evil.test/", nonce = "no-1"), "bad iss")
    reject(sign(aud = "other-client"), "bad aud")
    reject(sign(extra_claims = list(azp = "other-client")), "bad azp")
    reject(sign(exp = now - 120), "expired")
    reject(sign(iat = now + 3600), "iat in the future")
    reject(sign(iat = now - 3600), "stale iat")
    reject(sign(kid = "unknown-kid"), "unknown kid")
    expect_error(
        validate_id_token(sign_id_token(fixture, iss = iss, nonce = "other-nonce"), config, "no-1"),
        "nonce mismatch"
    )
    other <- new_jwt_fixture()
    reject(sign(key = other$key), "bad signature")
    # alg none (hand-built, unsigned)
    enc <- function(x) jose::base64url_encode(charToRaw(yyjsonr::write_json_str(x, auto_unbox = TRUE)))
    payload <- enc(list(iss = iss, aud = "fe-client", sub = "u", exp = now + 600, iat = now, nonce = "no-1"))
    none_token <- paste0(enc(list(alg = "none", typ = "JWT", kid = fixture$kid)), ".", payload, ".x")
    expect_error(validate_id_token(none_token, config, "no-1"), "none")
})

test_that("session_auth destroys idle- and absolutely-expired sessions", {
    now <- as.numeric(Sys.time())
    base_auth <- list(user_id = 1L, roles = character(), csrf_id = "x", created_at = now, last_seen = now)

    ds <- fake_datastore()
    ds$session$auth <- base_auth
    expect_equal(session_auth(ds)$user_id, 1L)

    ds$session$auth <- utils::modifyList(base_auth, list(last_seen = now - SESSION_IDLE_SECONDS - 10))
    expect_null(session_auth(ds))
    expect_null(ds$session$auth)

    ds$session$auth <- utils::modifyList(base_auth, list(created_at = now - SESSION_ABSOLUTE_SECONDS - 10))
    expect_null(session_auth(ds))
    expect_null(ds$session$auth)
})
