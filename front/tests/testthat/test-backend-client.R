# Backend client unit tests against the canned fake backend (guest sessions:
# no Authorization header, the dev bypass guard covers them in production).

guest_store <- function() {
    store <- fake_datastore()
    store$session$auth <- list(
        user_id = 1L,
        is_guest = TRUE,
        roles = character(),
        csrf_id = "csrf",
        created_at = as.numeric(Sys.time()),
        last_seen = as.numeric(Sys.time())
    )
    store
}

client_state <- function(backend_url) {
    list(config = utils::modifyList(test_config(), list(backend_url = backend_url)))
}

test_that("be_get parses JSON success bodies", {
    state <- client_state(local_backend_fake())
    me <- be_get(state, guest_store(), "/v1/me")
    expect_equal(me$user$nickname, "guest")
    expect_true("write:datasets" %in% unlist(me$scopes))
})

test_that("backend problem+json maps to fe_backend_error with status/title/detail", {
    state <- client_state(local_backend_fake())
    err <- tryCatch(be_get(state, guest_store(), "/v1/datasets/404"), fe_backend_error = identity)
    expect_s3_class(err, "fe_backend_error")
    expect_equal(err$status, 404L)
    expect_equal(err$title, "Not Found")
    expect_equal(err$detail, "no such dataset")
})

test_that("a non-JSON error body maps to fe_backend_error with the standard reason phrase", {
    # Regression: resp_status_desc() was fed the numeric status and 500'd the FE
    # whenever the BE answered without a problem+json body (e.g. guard 403s).
    state <- client_state(local_backend_fake())
    err <- tryCatch(be_get(state, guest_store(), "/v1/bare-403"), fe_backend_error = identity)
    expect_s3_class(err, "fe_backend_error")
    expect_equal(err$status, 403L)
    expect_equal(err$title, "Forbidden")
})

test_that("an unreachable backend maps to a 503 fe_backend_error", {
    state <- client_state("http://127.0.0.1:1")
    err <- tryCatch(be_get(state, guest_store(), "/v1/me"), fe_backend_error = identity)
    expect_s3_class(err, "fe_backend_error")
    expect_equal(err$status, 503L)
    expect_equal(err$title, "Backend unreachable")
})

test_that("be_maybe converts 404 to NULL and passes other errors through", {
    state <- client_state(local_backend_fake())
    expect_null(be_maybe(be_get(state, guest_store(), "/v1/datasets/404")))
    expect_error(
        be_maybe(stop(backend_error(429L, "Too Many Requests"))),
        class = "fe_backend_error"
    )
})

test_that("session_scopes fetches /v1/me once and caches the grant in the session", {
    state <- client_state(local_backend_fake())
    store <- guest_store()
    scopes <- session_scopes(state, store)
    expect_true(all(c("write:datasets", "write:models", "manage:keys") %in% scopes))
    expect_equal(unlist(store$session$auth$scopes), scopes)
    # Cached: a now-dead backend does not matter.
    state$config$backend_url <- "http://127.0.0.1:1"
    expect_equal(session_scopes(state, store), scopes)
    expect_true(session_can(state, store, "write:models"))
    expect_false(session_can(state, store, "view:admin"))
})

test_that("session_scopes fails soft (empty grant + backoff) when the backend is down", {
    state <- client_state("http://127.0.0.1:1")
    store <- guest_store()
    expect_equal(session_scopes(state, store), character())
    expect_false(is.null(store$session$auth$scopes_failed_at))
    # Within the backoff window the failure is not retried (cached empty grant).
    expect_equal(session_scopes(state, store), character())
})

test_that("ensure_fresh_access_token propagates fe_auth_expired for missing sessions", {
    state <- client_state("http://127.0.0.1:1")
    err <- tryCatch(be_get(state, fake_datastore(), "/v1/me"), fe_auth_expired = identity)
    expect_s3_class(err, "fe_auth_expired")
})

test_that("part_as_csv_bytes normalizes raw, data.frame and character parts", {
    raw_part <- charToRaw("a,b\n1,2\n")
    expect_identical(part_as_csv_bytes(raw_part), raw_part)

    df_bytes <- part_as_csv_bytes(data.frame(a = 1L, b = 2L))
    expect_true(is.raw(df_bytes))
    parsed <- utils::read.csv(text = rawToChar(df_bytes))
    expect_equal(parsed$a, 1L)

    expect_identical(part_as_csv_bytes("a,b\n1,2\n"), charToRaw("a,b\n1,2\n"))
    expect_null(part_as_csv_bytes(NULL))
    expect_null(part_as_csv_bytes(list()))
})

test_that("scalar_field trims and drops empties", {
    expect_equal(scalar_field(" x "), "x")
    expect_null(scalar_field(""))
    expect_null(scalar_field("   "))
    expect_null(scalar_field(NULL))
    expect_equal(scalar_field(c("a", "b")), "a")
})

test_that("the mgmt token is fetched once and mgmt_update_nickname PATCHes the sub", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    reset_mgmt_cache()
    withr::defer(reset_mgmt_cache())
    config <- test_config(domain = tenant$base_url)
    config$auth0$mgmt_client_id <- "m2m-client"
    config$auth0$mgmt_client_secret <- "m2m-secret"

    mgmt_update_nickname(config, "auth0|fe-user", "neo")
    mgmt_update_nickname(config, "auth0|fe-user", "trinity")
    stats <- tenant$stats()
    # Auth0Management caches the client-credentials token across calls.
    expect_equal(stats$n_mgmt_token, 1L)
    expect_equal(stats$n_mgmt_patch, 2L)
    expect_equal(stats$last_patch$nickname, "trinity")
    expect_equal(stats$last_patch$id, "auth0|fe-user")
})
