testthat::local_edition(3)

test_that("users round-trips guest and authenticated rows", {
    con <- local_migrated_con()
    DBI::dbExecute(
        con,
        "INSERT INTO users (auth0_sub, email, nickname, is_guest) VALUES ($1, $2, $3, $4)",
        params = list("auth0|abc", "alice@example.com", "alice", FALSE)
    )
    DBI::dbExecute(
        con,
        "INSERT INTO users (auth0_sub, nickname, is_guest) VALUES ($1, $2, $3)",
        params = list(NA_character_, "guest", TRUE)
    )

    users <- DBI::dbGetQuery(
        con,
        "SELECT auth0_sub, email, nickname, is_guest FROM users ORDER BY id"
    )
    expect_equal(users$nickname, c("alice", "guest"))
    expect_equal(users$is_guest, c(FALSE, TRUE))
    expect_equal(users$auth0_sub, c("auth0|abc", NA_character_))
})

test_that("datasets round-trips jsonb via yyjsonr in both directions", {
    con <- local_migrated_con()
    user_id <- insert_user(con)
    df <- mtcars
    data_json <- yyjsonr::write_json_str(df)

    DBI::dbExecute(
        con,
        "INSERT INTO datasets (user_id, name, data, n_rows, n_cols) VALUES ($1, $2, $3::jsonb, $4, $5)",
        params = list(user_id, "mtcars", data_json, nrow(df), ncol(df))
    )

    row <- DBI::dbGetQuery(con, "SELECT data, n_rows, n_cols FROM datasets")
    parsed <- yyjsonr::read_json_str(row$data)
    expect_equal(nrow(parsed), nrow(df))
    expect_setequal(names(parsed), names(df))
    for (nm in names(df)) {
        expect_equal(parsed[[nm]], df[[nm]], info = nm)
    } # jsonb does not preserve column order
    expect_equal(as.integer(row$n_rows), nrow(df))
    expect_equal(as.integer(row$n_cols), ncol(df))
})

test_that("models round-trips a serialized lm blob and cascades on dataset delete", {
    con <- local_migrated_con()
    user_id <- insert_user(con)
    dataset_id <- DBI::dbGetQuery(
        con,
        "INSERT INTO datasets (user_id, name, data, n_rows, n_cols) VALUES ($1, 'd', '[]'::jsonb, 0, 0) RETURNING id",
        params = list(user_id)
    )$id

    fit <- lm(mpg ~ wt, data = mtcars)
    metrics_json <- yyjsonr::write_json_str(list(r2 = summary(fit)$r.squared))
    DBI::dbExecute(
        con,
        "INSERT INTO models (user_id, dataset_id, formula, metrics, model_blob) VALUES ($1, $2, $3, $4::jsonb, $5)",
        params = list(
            user_id,
            dataset_id,
            "mpg ~ wt",
            metrics_json,
            list(serialize(fit, NULL))
        )
    )

    got <- DBI::dbGetQuery(con, "SELECT metrics, model_blob FROM models")
    restored <- unserialize(got$model_blob[[1]])
    expect_equal(coef(restored), coef(fit))
    newdata <- data.frame(wt = c(2.5, 3.5))
    expect_equal(predict(restored, newdata), predict(fit, newdata))
    expect_equal(yyjsonr::read_json_str(got$metrics)$r2, summary(fit)$r.squared)

    DBI::dbExecute(
        con,
        "DELETE FROM datasets WHERE id = $1",
        params = list(dataset_id)
    )
    n_models <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM models")$n
    expect_equal(as.integer(n_models), 0L) # FK ON DELETE CASCADE
})

test_that("jobs generates a uuid, defaults status, and enforces the status CHECK", {
    con <- local_migrated_con()
    user_id <- insert_user(con)
    payload_json <- yyjsonr::write_json_str(list(
        dataset_id = 1L,
        formula = "mpg ~ wt"
    ))

    inserted <- DBI::dbGetQuery(
        con,
        "INSERT INTO jobs (user_id, kind, payload) VALUES ($1, 'fit_model', $2::jsonb) RETURNING id, status",
        params = list(user_id, payload_json)
    )
    expect_match(
        inserted$id,
        "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
    )
    expect_equal(inserted$status, "pending")
    expect_equal(
        yyjsonr::read_json_str(
            DBI::dbGetQuery(con, "SELECT payload FROM jobs")$payload
        )$formula,
        "mpg ~ wt"
    )

    expect_error(
        DBI::dbExecute(
            con,
            "UPDATE jobs SET status = 'bogus' WHERE id = $1",
            params = list(inserted$id)
        ),
        "check constraint"
    )
    clear_pending_result(con)
})

test_that("api_keys round-trips text[] scopes and a bytea hash", {
    con <- local_migrated_con()
    user_id <- insert_user(con)
    scopes <- c("read:datasets", "write:models")
    key_hash <- as.raw(openssl::sha256(charToRaw("the-secret")))

    DBI::dbExecute(
        con,
        "INSERT INTO api_keys (user_id, name, key_prefix, key_hash, scopes) VALUES ($1, $2, $3, $4, $5::text[])",
        params = list(
            user_id,
            "ci-key",
            "pbk_1234",
            list(key_hash),
            pg_text_array_literal(scopes)
        )
    )

    got <- DBI::dbGetQuery(
        con,
        "SELECT key_prefix, key_hash, scopes FROM api_keys"
    )
    expect_equal(got$key_prefix, "pbk_1234")
    expect_identical(got$key_hash[[1]], key_hash)
    expect_equal(parse_pg_text_array(got$scopes), scopes)

    DBI::dbExecute(
        con,
        "INSERT INTO api_keys (user_id, name, key_prefix, key_hash) VALUES ($1, 'empty', 'pbk_0', $2)",
        params = list(user_id, list(key_hash))
    )
    empty <- DBI::dbGetQuery(
        con,
        "SELECT scopes FROM api_keys WHERE name = 'empty'"
    )$scopes
    expect_equal(parse_pg_text_array(empty), character(0)) # DEFAULT '{}'
})

test_that("request_log accepts rows without foreign keys", {
    con <- local_migrated_con()
    DBI::dbExecute(
        con,
        "INSERT INTO request_log (service, method, path, status, user_id, duration_ms) VALUES ($1, $2, $3, $4, $5, $6)",
        params = list("back", "GET", "/v1/me", 200L, 999999L, 12L)
    )

    got <- DBI::dbGetQuery(
        con,
        "SELECT status, user_id, api_key_id FROM request_log"
    )
    expect_equal(got$status, 200L)
    expect_equal(as.integer(got$user_id), 999999L) # no FK: a non-existent user_id is allowed
    expect_true(is.na(got$api_key_id))
})
