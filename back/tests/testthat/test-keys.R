# API-key machinery: format, hashing, constant-time compare (pure), and
# create/lookup/revoke against a migrated scratch schema.

test_that("generated keys have the documented format and unique prefixes", {
    secret <- generate_api_key()
    expect_match(secret, API_KEY_PATTERN)
    expect_equal(nchar(secret), 4L + 64L)
    expect_equal(api_key_prefix(secret), substr(secret, 1, 8))
    expect_false(generate_api_key() == generate_api_key())
})

test_that("constant_time_equal compares raw vectors correctly", {
    a <- hash_api_key("pbk_a")
    b <- hash_api_key("pbk_b")
    expect_true(constant_time_equal(a, hash_api_key("pbk_a")))
    expect_false(constant_time_equal(a, b))
    expect_false(constant_time_equal(a, a[1:16]))
})

test_that("create -> lookup -> revoke round-trips against the database", {
    pool <- local_migrated_pool()
    user_id <- DBI::dbGetQuery(
        pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('key-owner', false) RETURNING id"
    )$id

    created <- create_api_key(pool, user_id, "ci-key", scopes = c("write:datasets", "write:models"))
    expect_match(created$secret, API_KEY_PATTERN)

    record <- lookup_api_key(pool, created$secret)
    expect_equal(record$id, created$id)
    expect_equal(record$user_id, user_id)
    expect_equal(record$name, "ci-key")
    expect_setequal(record$scopes, c("write:datasets", "write:models"))

    # Wrong secret with a valid format and the same prefix is rejected
    forged <- paste0(substr(created$secret, 1, 8), sodium::bin2hex(openssl::rand_bytes(30)))
    expect_equal(nchar(forged), 68L)
    expect_null(lookup_api_key(pool, forged))
    # Garbage formats never reach the database
    expect_null(lookup_api_key(pool, "pbk_short"))
    expect_null(lookup_api_key(pool, NULL))

    # touch updates last_used_at
    touch_api_key(pool, created$id)
    last_used <- DBI::dbGetQuery(
        pool,
        "SELECT last_used_at FROM api_keys WHERE id = $1",
        params = list(created$id)
    )$last_used_at
    expect_false(is.na(last_used))

    # revoke: scoped to the owner, idempotent, kills lookup
    expect_false(revoke_api_key(pool, user_id + 1L, created$id))
    expect_true(revoke_api_key(pool, user_id, created$id))
    expect_false(revoke_api_key(pool, user_id, created$id))
    expect_null(lookup_api_key(pool, created$secret))
})

test_that("an expired key is not looked up", {
    pool <- local_migrated_pool()
    user_id <- DBI::dbGetQuery(
        pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('exp-owner', false) RETURNING id"
    )$id

    live <- create_api_key(pool, user_id, "live", expires_at = Sys.time() + 3600)
    expired <- create_api_key(pool, user_id, "expired", expires_at = Sys.time() - 3600)

    expect_false(is.null(lookup_api_key(pool, live$secret)))
    expect_null(lookup_api_key(pool, expired$secret))
})

test_that("empty scopes round-trip as an empty character vector", {
    pool <- local_migrated_pool()
    user_id <- DBI::dbGetQuery(
        pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('noscope', false) RETURNING id"
    )$id
    created <- create_api_key(pool, user_id, "noscope-key")
    expect_equal(lookup_api_key(pool, created$secret)$scopes, character(0))
})
