# Shared helpers for the db/ migration tests. testthat::test_dir sources this
# before the test files. Tests connect as the `admin` superuser and run inside a
# throwaway schema (migrate_test_<pid>_<n>) dropped on exit, so they never touch
# the app schema or the spike schema. Run from the repo root:
#   Rscript -e 'testthat::test_dir("db/tests")'

# testthat runs helpers/tests with the working directory set to the test dir, but
# `Rscript db/migrate.R` is launched from the repo root; resolve paths either way.
db_dir <- Find(
    function(d) file.exists(file.path(d, "migrate-lib.R")),
    c("..", "db", ".")
)
if (is.null(db_dir)) {
    stop("cannot locate db/migrate-lib.R from working directory ", getwd())
}

MIGRATE_LIB_PATH <- normalizePath(file.path(db_dir, "migrate-lib.R"))
MIGRATIONS_DIR <- normalizePath(file.path(db_dir, "migrations"))
SHARED_DDL_PATH <- normalizePath(file.path(db_dir, "schema-shared.sql"))
source(MIGRATE_LIB_PATH)

admin_connect <- function() {
    DBI::dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("PGHOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("PGPORT", "5433")),
        dbname = Sys.getenv("PGDATABASE", "apps"),
        user = "admin",
        password = "admin",
        options = "-c client_min_messages=warning" # suppress routine NOTICEs for pristine test output
    )
}

# A statement that errors mid-execution (e.g. a CHECK violation) leaves RPostgres
# with an uncleared result; the next query would warn "Closing open result set".
# One throwaway query clears it quietly so teardown stays pristine.
clear_pending_result <- function(con) {
    suppressWarnings(DBI::dbGetQuery(con, "SELECT 1"))
    invisible()
}

# Create an isolated schema, pin the connection's search_path to it, and register
# teardown (drop schema + disconnect) on the caller's frame. Returns con + schema.
local_scratch_con <- function(env = parent.frame()) {
    con <- admin_connect()
    schema <- sprintf("migrate_test_%d_%d", Sys.getpid(), sample.int(1e6L, 1L))
    quoted <- DBI::dbQuoteIdentifier(con, schema)
    DBI::dbExecute(con, sprintf("CREATE SCHEMA %s", quoted))
    DBI::dbExecute(con, sprintf("SET search_path TO %s", quoted))
    withr::defer(
        {
            DBI::dbExecute(con, sprintf("DROP SCHEMA IF EXISTS %s CASCADE", quoted))
            DBI::dbDisconnect(con)
        },
        envir = env
    )
    list(con = con, schema = schema)
}

# Fresh scratch schema with the shared DDL and all migrations already applied.
# role/schema = NULL: the shared tables land in the scratch schema (single
# namespace) instead of the real "shared" schema, keeping tests isolated.
local_migrated_con <- function(env = parent.frame()) {
    scratch <- local_scratch_con(env)
    run_shared_ddl(scratch$con, SHARED_DDL_PATH, role = NULL, schema = NULL)
    run_migrations(scratch$con, MIGRATIONS_DIR)
    scratch$con
}

table_exists <- function(con, name) {
    !is.na(
        DBI::dbGetQuery(con, "SELECT to_regclass($1) AS r", params = list(name))$r
    )
}

insert_user <- function(
    con,
    nickname = "u",
    auth0_sub = NA_character_,
    is_guest = FALSE
) {
    DBI::dbGetQuery(
        con,
        "INSERT INTO users (auth0_sub, nickname, is_guest) VALUES ($1, $2, $3) RETURNING id",
        params = list(auth0_sub, nickname, is_guest)
    )$id
}

# Postgres text[] round-trips as a raw array literal string (RPostgres does not
# parse it back into an R vector), so tests build and parse the literal explicitly.
pg_text_array_literal <- function(x) {
    if (length(x) == 0L) {
        return("{}")
    }
    escaped <- gsub('(["\\\\])', "\\\\\\1", x)
    sprintf("{%s}", paste0('"', escaped, '"', collapse = ","))
}

parse_pg_text_array <- function(literal) {
    inner <- sub("\\}$", "", sub("^\\{", "", as.character(literal)))
    if (!nzchar(inner)) {
        return(character(0))
    }
    parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
    gsub('^"|"$', "", parts)
}
