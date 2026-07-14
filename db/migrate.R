#!/usr/bin/env Rscript
# Applies the shared cross-app DDL, then pending SQL migrations to the app
# schema, then exits 0 (non-zero on failure). Connection from PG* env vars
# (defaults match the dev compose).
# Usage: Rscript db/migrate.R

script_dir <- function() {
    args <- commandArgs(FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg)) {
        return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
    }
    normalizePath(getwd())
}

main <- function(here) {
    con <- db_connect_env()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    assert_db_sanity(con)
    run_shared_ddl(con, file.path(here, "schema-shared.sql"))
    applied <- run_migrations(con, file.path(here, "migrations"))
    if (length(applied)) {
        message(sprintf(
            "Applied %d migration(s): %s",
            length(applied),
            paste(applied, collapse = ", ")
        ))
    } else {
        message("No pending migrations; schema is up to date.")
    }
}

here <- script_dir()
source(file.path(here, "migrate-lib.R"))

status <- tryCatch(
    {
        main(here)
        0L
    },
    error = function(e) {
        message("Migration failed: ", conditionMessage(e))
        1L
    }
)
quit(status = status, save = "no")
