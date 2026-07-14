# Token refresh against the fake tenant. The FE is a single non-async R
# process, so "two parallel near-expiry requests" execute strictly sequentially:
# the first blocks on the refresh and persists the rotated refresh token before
# the second re-reads the session - proven here by the call counter.

seeded_session <- function(config, refresh_token = "rt-1", expires_in = 30) {
    ds <- fake_datastore()
    now <- as.numeric(Sys.time())
    ds$session$auth <- list(
        user_id = 1L,
        roles = character(),
        csrf_id = "c",
        created_at = now,
        last_seen = now,
        access_token = "at-0",
        access_expires_at = now + expires_in,
        refresh_token_enc = encrypt_secret(refresh_token, refresh_key(config))
    )
    ds
}

test_that("two near-expiry requests trigger exactly one refresh and persist the rotation", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    config <- test_config(domain = tenant$base_url)
    ds <- seeded_session(config)

    first <- ensure_fresh_access_token(ds, config)
    second <- ensure_fresh_access_token(ds, config)

    expect_match(first, "^at-refreshed-")
    expect_identical(second, first)
    stats <- tenant$stats()
    expect_equal(stats$n_token, 1L)
    # The rotated refresh token was stored before the second call ran.
    expect_equal(decrypt_secret(ds$session$auth$refresh_token_enc, refresh_key(config)), stats$current_rt)
})

test_that("a fresh access token is served without touching the tenant", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    config <- test_config(domain = tenant$base_url)
    ds <- seeded_session(config, expires_in = 3600)

    expect_equal(ensure_fresh_access_token(ds, config), "at-0")
    expect_equal(tenant$stats()$n_token, 0L)
})

test_that("a rejected refresh (rotation reuse) destroys the session and raises", {
    fixture <- new_jwt_fixture()
    tenant <- local_auth0_fake(fixture)
    config <- test_config(domain = tenant$base_url)
    ds <- seeded_session(config, refresh_token = "stolen-rt")

    expect_error(ensure_fresh_access_token(ds, config), class = "fe_auth_expired")
    expect_null(ds$session$auth)
})

test_that("guest sessions carry no token and never refresh", {
    config <- test_config()
    ds <- fake_datastore()
    now <- as.numeric(Sys.time())
    ds$session$auth <- list(user_id = 1L, is_guest = TRUE, csrf_id = "c", created_at = now, last_seen = now)
    expect_null(ensure_fresh_access_token(ds, config))
})

test_that("a missing session raises fe_auth_expired", {
    expect_error(ensure_fresh_access_token(fake_datastore(), test_config()), class = "fe_auth_expired")
})
