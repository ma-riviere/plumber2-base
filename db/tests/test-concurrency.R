testthat::local_edition(3)

test_that("two concurrent runners serialize on the advisory locks and apply everything once", {
    scratch <- local_scratch_con()
    schema <- scratch$schema

    lib <- MIGRATE_LIB_PATH
    migrations <- MIGRATIONS_DIR
    shared_ddl <- SHARED_DDL_PATH
    conn <- list(
        host = Sys.getenv("PGHOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("PGPORT", "5433")),
        dbname = Sys.getenv("PGDATABASE", "apps")
    )

    # Each worker applies the shared DDL then the migrations, mirroring a real
    # container start: the shared_ddl advisory lock serializes the former (in
    # prod, across BOTH apps), the migration lock the latter.
    worker <- function(lib, migrations, shared_ddl, schema, conn) {
        source(lib)
        con <- DBI::dbConnect(
            RPostgres::Postgres(),
            host = conn$host,
            port = conn$port,
            dbname = conn$dbname,
            user = "admin",
            password = "admin",
            options = "-c client_min_messages=warning"
        )
        on.exit(DBI::dbDisconnect(con))
        DBI::dbExecute(
            con,
            sprintf("SET search_path TO %s", DBI::dbQuoteIdentifier(con, schema))
        )
        run_shared_ddl(con, shared_ddl, role = NULL, schema = NULL)
        run_migrations(con, migrations)
        "ok"
    }

    p1 <- callr::r_bg(worker, args = list(lib, migrations, shared_ddl, schema, conn))
    p2 <- callr::r_bg(worker, args = list(lib, migrations, shared_ddl, schema, conn))
    p1$wait(30000)
    p2$wait(30000)

    expect_equal(p1$get_exit_status(), 0L)
    expect_equal(p2$get_exit_status(), 0L)
    expect_equal(p1$get_result(), "ok")
    expect_equal(p2$get_result(), "ok")

    con <- scratch$con
    n <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM schema_migrations")$n
    expect_equal(
        as.integer(n),
        length(list.files(MIGRATIONS_DIR, pattern = "\\.sql$"))
    )
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
