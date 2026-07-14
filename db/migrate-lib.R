# Forward-only SQL migration runner for the plumber-base app schema.
#
# Sourceable library: run_migrations(con, dir) applies pending NNN_*.sql files in
# filename order, each inside its own transaction, under a Postgres advisory lock
# so concurrent runners (parallel container starts) cannot race. It is idempotent
# and aborts on tampering (checksum mismatch), a gap in the applied sequence, or a
# failing migration (that file is rolled back). db/migrate.R is the thin Rscript
# wrapper; the back entrypoint can call run_migrations() in-process.
#
# Schema objects are created unqualified: they land in the connecting role's
# search_path schema (dev/prod pin it per app; tests set it on their scratch schema).
#
# The cross-app tables (users/datasets/models) are NOT migrations: they live in
# the "shared" schema (shared with shiny-base) and are applied separately by
# run_shared_ddl() from db/schema-shared.sql (byte-identical copy in both
# repos), BEFORE the app migrations (which reference users).

MIGRATION_LOCK_NAME <- "plumber_base_migrate"

# Post-apply contract check for the shared DDL: CREATE IF NOT EXISTS never
# evolves an existing table, so a column added in the sibling repo but not yet
# here must fail startup loudly instead of surfacing as runtime SQL errors.
SHARED_EXPECTED_COLUMNS <- list(
    users = c("id", "auth0_sub", "email", "nickname", "is_guest", "created_at", "last_seen_at"),
    datasets = c("id", "user_id", "name", "description", "data", "n_rows", "n_cols", "created_at", "updated_at"),
    models = c("id", "user_id", "dataset_id", "formula", "metrics", "model_blob", "created_at", "updated_at")
)

# Apply the cross-app shared DDL. In prod/dev, role and schema are "shared":
# the advisory lock key is COMMON to every app applying this file (shiny-base
# uses the same expression), SET LOCAL ROLE makes the shared role own every
# object regardless of which app runs first (membership granted by the
# platform), and SET LOCAL search_path lands the unqualified DDL in the shared
# schema. Tests pass role = NULL, schema = NULL to apply into their scratch
# schema instead.
run_shared_ddl <- function(con, path, role = "shared", schema = "shared") {
    stopifnot(inherits(con, "DBIConnection"), DBI::dbIsValid(con))
    sql <- read_sql(path)
    DBI::dbBegin(con)
    tryCatch(
        {
            DBI::dbGetQuery(con, "SELECT pg_advisory_xact_lock(hashtext('shared_ddl')::bigint)")
            if (!is.null(role)) {
                DBI::dbExecute(con, sprintf("SET LOCAL ROLE %s", DBI::dbQuoteIdentifier(con, role)))
            }
            if (!is.null(schema)) {
                DBI::dbExecute(con, sprintf("SET LOCAL search_path TO %s", DBI::dbQuoteIdentifier(con, schema)))
            }
            DBI::dbExecute(con, sql, immediate = TRUE)
            DBI::dbCommit(con)
        },
        error = function(e) {
            DBI::dbRollback(con)
            stop(
                sprintf("shared DDL failed and was rolled back: %s", conditionMessage(e)),
                call. = FALSE
            )
        }
    )
    verify_shared_columns(con, schema)
    invisible()
}

verify_shared_columns <- function(con, schema = NULL) {
    columns <- DBI::dbGetQuery(
        con,
        "SELECT table_name, column_name FROM information_schema.columns
         WHERE table_schema = COALESCE($1, current_schema())",
        params = list(if (is.null(schema)) NA_character_ else schema)
    )
    for (table in names(SHARED_EXPECTED_COLUMNS)) {
        have <- columns$column_name[columns$table_name == table]
        missing <- setdiff(SHARED_EXPECTED_COLUMNS[[table]], have)
        if (length(missing)) {
            stop(
                sprintf(
                    "shared table '%s' is missing column(s): %s (schema-shared.sql drifted from the app code?)",
                    table,
                    paste(missing, collapse = ", ")
                ),
                call. = FALSE
            )
        }
    }
    invisible()
}

# Startup sanity for prod/dev (NOT for scratch-schema tests). With search_path
# = "<app>", shared, a missing app schema makes current_schema() silently fall
# through to "shared" (role name == schema name on both the platform and the
# dev compose). App-local copies of the shared tables would shadow shared.*
# for every unqualified query (including the unqualified REFERENCES users in
# the migrations): refuse to start until they are dropped (cutover step).
assert_db_sanity <- function(con) {
    sanity <- DBI::dbGetQuery(con, "SELECT current_schema() AS schema, current_user AS role")
    if (is.na(sanity$schema) || !identical(sanity$schema, sanity$role)) {
        stop(
            sprintf(
                "current_schema() is '%s' but the role is '%s': app schema missing from search_path or not owned",
                sanity$schema,
                sanity$role
            ),
            call. = FALSE
        )
    }
    shadow <- DBI::dbGetQuery(
        con,
        "SELECT table_name FROM information_schema.tables
         WHERE table_schema = current_schema() AND table_name IN ('users', 'datasets', 'models')"
    )$table_name
    if (length(shadow)) {
        stop(
            sprintf(
                "shared-lineage table(s) still exist in app schema '%s' and would shadow the shared ones: %s",
                sanity$schema,
                paste(shadow, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    invisible()
}

run_migrations <- function(con, dir) {
    stopifnot(inherits(con, "DBIConnection"), DBI::dbIsValid(con))
    files <- migration_files(dir)

    acquire_migration_lock(con)
    on.exit(release_migration_lock(con), add = TRUE)

    ensure_schema_migrations(con)
    applied <- applied_migrations(con)
    check_migration_consistency(files, applied)

    pending <- setdiff(names(files), applied$filename)
    pending <- names(files)[names(files) %in% pending]
    for (filename in pending) {
        apply_migration(con, files[[filename]], filename)
    }
    invisible(pending)
}

# Connect from PG* env vars, defaulting to the dev compose app role.
db_connect_env <- function() {
    DBI::dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("PGHOST", "127.0.0.1"),
        port = as.integer(Sys.getenv("PGPORT", "5433")),
        dbname = Sys.getenv("PGDATABASE", "apps"),
        user = Sys.getenv("PGUSER", "plumber_base"),
        password = Sys.getenv("PGPASSWORD", "plumber_base"),
        # keep migration output clean: idempotent DDL emits routine NOTICEs otherwise
        options = "-c client_min_messages=warning"
    )
}

migration_files <- function(dir) {
    if (!dir.exists(dir)) {
        stop(sprintf("migrations directory not found: %s", dir), call. = FALSE)
    }
    paths <- list.files(dir, pattern = "\\.sql$", full.names = TRUE)
    paths <- paths[order(basename(paths))]
    setNames(paths, basename(paths))
}

ensure_schema_migrations <- function(con) {
    DBI::dbExecute(
        con,
        paste(
            "CREATE TABLE IF NOT EXISTS schema_migrations (",
            "    filename   text PRIMARY KEY,",
            "    checksum   text NOT NULL,",
            "    applied_at timestamptz NOT NULL DEFAULT now()",
            ")",
            sep = "\n"
        )
    )
    invisible()
}

applied_migrations <- function(con) {
    DBI::dbGetQuery(
        con,
        "SELECT filename, checksum FROM schema_migrations ORDER BY filename"
    )
}

# Guard against tampering and out-of-order state before applying anything.
check_migration_consistency <- function(files, applied) {
    on_disk <- names(files)
    recorded <- applied$filename

    missing <- setdiff(recorded, on_disk)
    if (length(missing)) {
        stop(
            sprintf(
                "applied migration(s) missing from disk: %s",
                paste(missing, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    for (i in seq_len(nrow(applied))) {
        current <- file_checksum(files[[applied$filename[i]]])
        if (!identical(current, applied$checksum[i])) {
            stop(
                sprintf(
                    "checksum mismatch for %s: file changed after it was applied",
                    applied$filename[i]
                ),
                call. = FALSE
            )
        }
    }
    # applied migrations must be a contiguous prefix of the on-disk sequence
    expected_prefix <- on_disk[seq_along(recorded)]
    if (!identical(sort(recorded), expected_prefix)) {
        stop(
            sprintf(
                "gap in applied migration sequence: applied {%s} is not a prefix of {%s}",
                paste(sort(recorded), collapse = ", "),
                paste(on_disk, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    invisible()
}

apply_migration <- function(con, path, filename) {
    sql <- read_sql(path)
    checksum <- file_checksum(path)
    DBI::dbBegin(con)
    tryCatch(
        {
            # immediate = TRUE uses the simple query protocol, so a file may hold
            # several statements (CREATE TABLE + CREATE INDEX ...); prepared statements cannot.
            DBI::dbExecute(con, sql, immediate = TRUE)
            DBI::dbExecute(
                con,
                "INSERT INTO schema_migrations (filename, checksum) VALUES ($1, $2)",
                params = list(filename, checksum)
            )
            DBI::dbCommit(con)
        },
        error = function(e) {
            DBI::dbRollback(con)
            stop(
                sprintf(
                    "migration %s failed and was rolled back: %s",
                    filename,
                    conditionMessage(e)
                ),
                call. = FALSE
            )
        }
    )
    invisible()
}

acquire_migration_lock <- function(con) {
    keys <- migration_lock_keys(MIGRATION_LOCK_NAME)
    invisible(DBI::dbGetQuery(
        con,
        "SELECT pg_advisory_lock($1, $2)",
        params = keys
    ))
}

release_migration_lock <- function(con) {
    if (!DBI::dbIsValid(con)) {
        return(invisible())
    }
    keys <- migration_lock_keys(MIGRATION_LOCK_NAME)
    invisible(DBI::dbGetQuery(
        con,
        "SELECT pg_advisory_unlock($1, $2)",
        params = keys
    ))
}

# Two signed 32-bit keys for pg_advisory_lock(int, int), derived deterministically
# from the lock name (R integers are int4, matching Postgres exactly).
migration_lock_keys <- function(name) {
    digest <- openssl::sha256(charToRaw(name))
    list(
        readBin(digest[1:4], "integer", size = 4L, endian = "big"),
        readBin(digest[5:8], "integer", size = 4L, endian = "big")
    )
}

read_sql <- function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
}

file_checksum <- function(path) {
    as.character(openssl::sha256(read_sql(path)))
}
