# Datasets + models + jobs through the real assembled api (bypass mode for the
# happy paths - the auth matrix lives in test-auth-endpoints.R; JWTs are used
# where distinct users or scopes matter). Scratch-schema DB throughout.

csv_fixture <- "mpg,wt,hp\n21,2.6,110\n22.8,2.3,93\n21.4,3.2,110\n18.7,3.4,175\n"

upload_dataset <- function(ctx, headers = list(), csv = csv_fixture, fields = list()) {
    payload <- multipart_csv(csv, fields = fields)
    do_request(
        ctx$pa,
        "http://t/v1/datasets",
        method = "post",
        headers = c(headers, payload$headers),
        content = payload$content
    )
}

test_that("dataset upload -> get -> data -> patch -> delete round-trips", {
    ctx <- auth_api(bypass = TRUE)

    created <- upload_dataset(ctx, fields = list(name = "cars", description = "a few rows"))
    expect_equal(created$status, 201L)
    body <- yyjsonr::read_json_str(created$body)
    expect_equal(body$name, "cars")
    expect_equal(body$n_rows, 4L)
    expect_equal(body$n_cols, 3L)
    id <- body$id
    expect_equal(created$headers[["location"]], sprintf("/v1/datasets/%d", id))

    meta <- yyjsonr::read_json_str(do_request(ctx$pa, sprintf("http://t/v1/datasets/%d", id))$body)
    expect_equal(meta$description, "a few rows")
    expect_equal(meta$summary$mpg$type, "numeric")
    expect_equal(meta$summary$mpg$n_missing, 0L)

    rows <- yyjsonr::read_json_str(do_request(ctx$pa, sprintf("http://t/v1/datasets/%d/data?limit=2", id))$body)
    expect_equal(rows$n_rows, 4L)
    expect_equal(nrow(rows$rows), 2L)
    expect_equal(rows$columns, c("mpg", "wt", "hp"))
    page2 <- yyjsonr::read_json_str(
        do_request(ctx$pa, sprintf("http://t/v1/datasets/%d/data?offset=2&limit=2", id))$body
    )
    expect_equal(nrow(page2$rows), 2L)
    expect_false(identical(rows$rows$mpg, page2$rows$mpg))

    csv <- do_request(ctx$pa, sprintf("http://t/v1/datasets/%d/data.csv", id))
    expect_equal(csv$status, 200L)
    expect_match(csv$headers[["content-type"]], "text/csv")
    expect_match(csv$headers[["content-disposition"]], "attachment.*cars\\.csv")
    expect_match(csv$body, "^mpg,wt,hp")

    patched <- do_json_request(
        ctx$pa,
        sprintf("http://t/v1/datasets/%d", id),
        "patch",
        list(name = "cars-renamed")
    )
    expect_equal(patched$status, 200L)
    expect_equal(yyjsonr::read_json_str(patched$body)$name, "cars-renamed")

    deleted <- do_request(ctx$pa, sprintf("http://t/v1/datasets/%d", id), method = "delete")
    expect_equal(deleted$status, 204L)
    expect_equal(do_request(ctx$pa, sprintf("http://t/v1/datasets/%d", id))$status, 404L)
})

test_that("character rownames survive the jsonb round-trip via the _row field", {
    ctx <- auth_api(bypass = TRUE)
    user_id <- DBI::dbGetQuery(
        ctx$pool,
        "INSERT INTO users (nickname, is_guest) VALUES ('rowns', false) RETURNING id"
    )$id

    df <- head(mtcars, 3)
    created <- db_insert_dataset(ctx$pool, user_id, "mt", NA, df)
    expect_equal(as.integer(created$n_cols), ncol(df)) # _row is not a column

    fetched <- db_get_dataset_data(ctx$pool, user_id, created$id)
    expect_equal(rownames(fetched), rownames(df))
    expect_false("_row" %in% names(fetched))
    expect_equal(fetched$mpg, df$mpg)

    # The preview page never exposes _row either.
    page <- db_get_dataset_page(ctx$pool, user_id, created$id, offset = 0L, limit = 2L)
    expect_equal(page$columns, names(df))
    expect_false("_row" %in% names(page$rows))

    # Numeric-looking rownames (subset leftovers) are noise, not persisted.
    noise <- data.frame(x = 1:5)[c(3, 5), , drop = FALSE]
    created2 <- db_insert_dataset(ctx$pool, user_id, "noise", NA, noise)
    stored <- DBI::dbGetQuery(
        ctx$pool,
        "SELECT data::text AS t FROM datasets WHERE id = $1",
        params = list(created2$id)
    )$t
    expect_false(grepl("_row", stored, fixed = TRUE))
    expect_equal(db_get_dataset_data(ctx$pool, user_id, created2$id)$x, c(3L, 5L))
})

test_that("dataset list paginates with a cursor and honours filters", {
    ctx <- auth_api(bypass = TRUE)
    for (i in 1:5) {
        n <- i + 1 # 2..6 data rows
        csv <- paste0("x,y\n", paste(sprintf("%d,%d", seq_len(n), seq_len(n)), collapse = "\n"), "\n")
        expect_equal(upload_dataset(ctx, csv = csv, fields = list(name = paste0("ds", i)))$status, 201L)
    }

    page1 <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/datasets?limit=2")$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_length(page1$items, 2L)
    expect_false(is.null(page1$next_after))
    page2 <- yyjsonr::read_json_str(
        do_request(ctx$pa, sprintf("http://t/v1/datasets?limit=2&after=%d", page1$next_after))$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_length(page2$items, 2L)
    ids1 <- vapply(page1$items, function(x) x$id, numeric(1))
    ids2 <- vapply(page2$items, function(x) x$id, numeric(1))
    expect_true(all(ids2 < min(ids1))) # id-descending cursor

    filtered <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/datasets?min_rows=4&max_rows=5")$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_setequal(
        vapply(filtered$items, function(x) x$n_rows, numeric(1)),
        c(4, 5)
    )

    none <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/datasets?created_to=2000-01-01")$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_length(none$items, 0L)
    expect_equal(do_request(ctx$pa, "http://t/v1/datasets?created_from=garbage")$status, 400L)
})

test_that("upload guardrails: missing part, non-CSV part, row/col caps, size cap", {
    withr::local_envvar(MAX_DATASET_ROWS = "3", MAX_DATASET_COLS = "2", MAX_UPLOAD_BYTES = "400")
    ctx <- auth_api(bypass = TRUE)

    missing_part <- multipart_csv(fields = list(name = "x"), include_file = FALSE)
    no_file <- do_request(
        ctx$pa,
        "http://t/v1/datasets",
        method = "post",
        headers = missing_part$headers,
        content = missing_part$content
    )
    expect_equal(no_file$status, 400L)
    # A JSON body on the multipart-only endpoint is 415 (unsupported media type).
    expect_equal(do_json_request(ctx$pa, "http://t/v1/datasets", "post", list(name = "x"))$status, 415L)

    too_many_rows <- upload_dataset(ctx, csv = "x,y\n1,1\n2,2\n3,3\n4,4\n")
    expect_equal(too_many_rows$status, 413L)
    too_many_cols <- upload_dataset(ctx, csv = "x,y,z\n1,2,3\n")
    expect_equal(too_many_cols$status, 413L)

    # The Content-Length precheck fires before the parser (the header must be
    # present; live clients always send it).
    big <- paste0("x,y\n", paste(rep("1,2", 200), collapse = "\n"), "\n")
    payload <- multipart_csv(big)
    oversized <- do_request(
        ctx$pa,
        "http://t/v1/datasets",
        method = "post",
        headers = c(payload$headers, list(Content_Length = as.character(nchar(payload$content)))),
        content = payload$content
    )
    expect_equal(oversized$status, 413L)
    expect_match(oversized$body, "byte limit")

    ok <- upload_dataset(ctx, csv = "x,y\n1,2\n")
    expect_equal(ok$status, 201L)
})

test_that("a dataset name cannot inject headers into the CSV Content-Disposition", {
    ctx <- auth_api(bypass = TRUE)

    created <- upload_dataset(ctx, fields = list(name = "innocent"))
    id <- yyjsonr::read_json_str(created$body)$id
    # PATCH gets the CRLF into the name (JSON strings carry it cleanly).
    patched <- do_json_request(
        ctx$pa,
        sprintf("http://t/v1/datasets/%d", id),
        "patch",
        list(name = "evil\r\nX-Injected: 1")
    )
    expect_equal(patched$status, 200L)

    csv <- do_request(ctx$pa, sprintf("http://t/v1/datasets/%d/data.csv", id))
    expect_equal(csv$status, 200L)
    disposition <- csv$headers[["content-disposition"]]
    expect_false(grepl("[\r\n]", disposition))
    expect_match(disposition, "evil__X-Injected", fixed = TRUE)
    expect_null(csv$headers[["x-injected"]])
})

test_that("datasets are isolated per user (404, existence not leaked)", {
    ctx <- auth_api()
    token_a <- sign_access_token(ctx$fixture, sub = "auth0|alice")
    token_b <- sign_access_token(ctx$fixture, sub = "auth0|bob")

    created <- upload_dataset(ctx, headers = bearer_header(token_a), fields = list(name = "alices"))
    expect_equal(created$status, 201L)
    id <- yyjsonr::read_json_str(created$body)$id

    expect_equal(
        do_request(ctx$pa, sprintf("http://t/v1/datasets/%d", id), headers = bearer_header(token_a))$status,
        200L
    )
    for (path in c("", "/data")) {
        expect_equal(
            do_request(
                ctx$pa,
                sprintf("http://t/v1/datasets/%d%s", id, path),
                headers = bearer_header(token_b)
            )$status,
            404L
        )
    }
    delete_by_b <- do_request(
        ctx$pa,
        sprintf("http://t/v1/datasets/%d", id),
        method = "delete",
        headers = bearer_header(token_b)
    )
    expect_equal(delete_by_b$status, 404L)
    listed_by_b <- yyjsonr::read_json_str(
        do_request(ctx$pa, "http://t/v1/datasets", headers = bearer_header(token_b))$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_length(listed_by_b$items, 0L)
})

test_that("model fit end-to-end: 202, poll to done, fetch model, refit dedupes at rest", {
    ctx <- auth_api(bypass = TRUE)
    mirai::daemons(1L)
    withr::defer(mirai::daemons(0L))

    id <- yyjsonr::read_json_str(upload_dataset(ctx, fields = list(name = "fitme"))$body)$id

    fit <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt + hp")
    )
    expect_equal(fit$status, 202L)
    job_id <- yyjsonr::read_json_str(fit$body)$job_id
    expect_equal(fit$headers[["location"]], paste0("/v1/jobs/", job_id))
    expect_equal(fit$headers[["retry-after"]], "2")

    job <- wait_for_job(ctx$pa, job_id, headers = list())
    expect_equal(job$status, "done")
    expect_true(is.numeric(job$result$metrics$r_squared))
    model_id <- job$result$model_id

    model <- yyjsonr::read_json_str(
        do_request(ctx$pa, sprintf("http://t/v1/models/%d", model_id))$body
    )
    expect_equal(model$dataset_id, id)
    expect_equal(model$formula, "mpg ~ wt + hp")
    expect_match(model$metrics$summary_text, "Coefficients")

    listed <- yyjsonr::read_json_str(
        do_request(ctx$pa, sprintf("http://t/v1/models?dataset_id=%d", id))$body,
        arr_of_objs_to_df = FALSE,
        obj_of_arrs_to_df = FALSE
    )
    expect_length(listed$items, 1L)

    # Refit of the same formula after completion: new job, same stored model row.
    refit <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt + hp")
    )
    expect_equal(refit$status, 202L)
    job2 <- wait_for_job(ctx$pa, yyjsonr::read_json_str(refit$body)$job_id, headers = list())
    expect_equal(job2$result$model_id, model_id)

    deleted <- do_request(ctx$pa, sprintf("http://t/v1/models/%d", model_id), method = "delete")
    expect_equal(deleted$status, 204L)
    expect_equal(do_request(ctx$pa, sprintf("http://t/v1/models/%d", model_id))$status, 404L)
})

test_that("a failing fit ends the job in error", {
    ctx <- auth_api(bypass = TRUE)
    mirai::daemons(1L)
    withr::defer(mirai::daemons(0L))

    # One data row: predictors collapse and summary/AIC still work, so force a
    # hard failure with a zero-row dataset instead.
    id <- yyjsonr::read_json_str(upload_dataset(ctx, csv = "mpg,wt\nNA,NA\n")$body)$id
    fit <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt")
    )
    expect_equal(fit$status, 202L)
    job <- wait_for_job(ctx$pa, yyjsonr::read_json_str(fit$body)$job_id, headers = list())
    expect_equal(job$status, "error")
    expect_true(nzchar(job$error))
})

test_that("malicious formulas are rejected with 422 before any job is created", {
    ctx <- auth_api(bypass = TRUE)
    id <- yyjsonr::read_json_str(upload_dataset(ctx)$body)$id

    for (bad in c("mpg ~ system('id')", "mpg ~ eval(parse(text='1'))", "mpg ~ nope")) {
        res <- do_json_request(
            ctx$pa,
            "http://t/v1/models",
            "post",
            list(dataset_id = id, formula = bad)
        )
        expect_equal(res$status, 422L)
    }
    jobs <- DBI::dbGetQuery(ctx$pool, "SELECT count(*) AS n FROM jobs")$n
    expect_equal(as.integer(jobs), 0L)
})

test_that("an identical live fit request returns the existing job (dedupe)", {
    ctx <- auth_api(bypass = TRUE)
    id <- yyjsonr::read_json_str(upload_dataset(ctx)$body)$id
    guest_id <- DBI::dbGetQuery(ctx$pool, "SELECT id FROM users WHERE is_guest LIMIT 1")$id

    live_job <- db_create_job(
        ctx$pool,
        guest_id,
        "fit_model",
        list(dataset_id = as.integer(id), formula = "mpg ~ wt")
    )
    res <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt")
    )
    expect_equal(res$status, 202L)
    expect_equal(yyjsonr::read_json_str(res$body)$job_id, live_job)
})

test_that("the per-user job cap yields 429 with Retry-After", {
    ctx <- auth_api(bypass = TRUE)
    id <- yyjsonr::read_json_str(upload_dataset(ctx)$body)$id
    guest_id <- DBI::dbGetQuery(ctx$pool, "SELECT id FROM users WHERE is_guest LIMIT 1")$id

    db_create_job(ctx$pool, guest_id, "fit_model", list(dataset_id = 0L, formula = "a ~ b"))
    db_create_job(ctx$pool, guest_id, "fit_model", list(dataset_id = 0L, formula = "a ~ c"))

    res <- do_json_request(
        ctx$pa,
        "http://t/v1/models",
        "post",
        list(dataset_id = id, formula = "mpg ~ wt")
    )
    expect_equal(res$status, 429L)
    expect_false(is.null(res$headers[["retry-after"]]))
})

test_that("polling an unknown or foreign job is 404", {
    ctx <- auth_api()
    token <- sign_access_token(ctx$fixture, sub = "auth0|carol")
    expect_equal(
        do_request(
            ctx$pa,
            "http://t/v1/jobs/00000000-0000-0000-0000-000000000000",
            headers = bearer_header(token)
        )$status,
        404L
    )
})
