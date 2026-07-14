testthat::local_edition(3)

migration_filenames <- function() {
    list.files(MIGRATIONS_DIR, pattern = "\\.sql$")
}

# The app migrations reference users (created by the shared DDL), so every
# scratch schema needs run_shared_ddl() before run_migrations().
apply_shared <- function(con) {
    run_shared_ddl(con, SHARED_DDL_PATH, role = NULL, schema = NULL)
}

test_that("shared DDL is idempotent and passes its column contract check", {
    con <- local_scratch_con()$con
    apply_shared(con)
    apply_shared(con) # second run: all IF NOT EXISTS, must be a no-op

    for (tbl in c("users", "datasets", "models")) {
        expect_true(table_exists(con, tbl), info = tbl)
    }
})

test_that("a fresh run applies every migration file and creates every table", {
    con <- local_scratch_con()$con
    apply_shared(con)
    applied <- run_migrations(con, MIGRATIONS_DIR)

    expect_setequal(applied, migration_filenames())
    recorded <- DBI::dbGetQuery(
        con,
        "SELECT filename, checksum FROM schema_migrations ORDER BY filename"
    )
    expect_setequal(recorded$filename, migration_filenames())
    expect_true(all(nchar(recorded$checksum) == 64L))
    for (tbl in c(
        "users",
        "datasets",
        "models",
        "jobs",
        "api_keys",
        "request_log"
    )) {
        expect_true(table_exists(con, tbl), info = tbl)
    }
})

test_that("re-running applies nothing (idempotent)", {
    con <- local_scratch_con()$con
    apply_shared(con)
    run_migrations(con, MIGRATIONS_DIR)

    second <- run_migrations(con, MIGRATIONS_DIR)
    expect_length(second, 0L)
    n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM schema_migrations")$n
    expect_equal(as.integer(n), length(migration_filenames()))
})

test_that("a checksum mismatch on an applied migration aborts (tamper detection)", {
    con <- local_scratch_con()$con
    apply_shared(con)
    run_migrations(con, MIGRATIONS_DIR)

    DBI::dbExecute(
        con,
        "UPDATE schema_migrations SET checksum = 'tampered' WHERE filename = $1",
        params = list("001_jobs.sql")
    )
    expect_error(run_migrations(con, MIGRATIONS_DIR), "checksum mismatch")
})

test_that("a gap in the applied sequence aborts", {
    con <- local_scratch_con()$con
    apply_shared(con)
    applied <- run_migrations(con, MIGRATIONS_DIR)

    middle <- sort(applied)[[2]]
    DBI::dbExecute(
        con,
        "DELETE FROM schema_migrations WHERE filename = $1",
        params = list(middle)
    )
    expect_error(run_migrations(con, MIGRATIONS_DIR), "gap")
})

test_that("a failing migration is rolled back and not recorded", {
    con <- local_scratch_con()$con
    dir <- withr::local_tempdir()
    writeLines("CREATE TABLE ok_table (id int);", file.path(dir, "001_ok.sql"))
    writeLines(
        c("CREATE TABLE bad_table (id int);", "SELECT this_is_not_valid_sql;"),
        file.path(dir, "002_bad.sql")
    )

    expect_error(run_migrations(con, dir), "002_bad\\.sql")
    expect_true(table_exists(con, "ok_table"))
    expect_false(table_exists(con, "bad_table"))
    recorded <- DBI::dbGetQuery(
        con,
        "SELECT filename FROM schema_migrations ORDER BY filename"
    )$filename
    expect_equal(recorded, "001_ok.sql")
})
