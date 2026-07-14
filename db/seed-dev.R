#!/usr/bin/env Rscript
# Idempotently seeds dev data: one guest user and one demo dataset (mtcars,
# serialized to jsonb via yyjsonr). Safe to run repeatedly. Run after migrations.
# Connection from PG* env vars (defaults match the dev compose).
# Usage: Rscript db/seed-dev.R

script_dir <- function() {
    args <- commandArgs(FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg)) {
        return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
    }
    normalizePath(getwd())
}

# INSERT ... WHERE NOT EXISTS keeps both inserts idempotent: neither users.nickname
# nor datasets.name has a unique constraint, so ON CONFLICT is not applicable.
seed_guest_user <- function(con) {
    DBI::dbExecute(
        con,
        paste(
            "INSERT INTO users (auth0_sub, nickname, is_guest)",
            "SELECT NULL, 'guest', true",
            "WHERE NOT EXISTS (SELECT 1 FROM users WHERE nickname = 'guest' AND is_guest)",
            sep = "\n"
        )
    )
    DBI::dbGetQuery(
        con,
        "SELECT id FROM users WHERE nickname = 'guest' AND is_guest ORDER BY id LIMIT 1"
    )$id
}

seed_demo_dataset <- function(con, user_id) {
    # Wrapped shape ({columns, rows}): jsonb normalizes object key order, so the
    # column order must ride in an array (see back/R/datasets.R).
    # The car-model rownames ride inside the row objects as the jsonlite-style
    # "_row" field (yyjsonr drops rownames; inject_rownames/restore_rownames in
    # back/R/datasets.R is the contract), never in `columns`/n_cols.
    rows <- mtcars
    rows[["_row"]] <- rownames(mtcars)
    rownames(rows) <- NULL
    data_json <- yyjsonr::write_json_str(list(columns = names(mtcars), rows = rows))
    DBI::dbExecute(
        con,
        paste(
            "INSERT INTO datasets (user_id, name, description, data, n_rows, n_cols)",
            "SELECT $1, 'mtcars', 'Demo dataset: Motor Trend Car Road Tests (mtcars).', $2::jsonb, $3, $4",
            "WHERE NOT EXISTS (SELECT 1 FROM datasets WHERE user_id = $1 AND name = 'mtcars')",
            sep = "\n"
        ),
        params = list(user_id, data_json, nrow(mtcars), ncol(mtcars))
    )
}

main <- function() {
    con <- db_connect_env()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbWithTransaction(con, {
        user_id <- seed_guest_user(con)
        seed_demo_dataset(con, user_id)
    })
    message("Seed complete: guest user + mtcars demo dataset ensured.")
}

here <- script_dir()
source(file.path(here, "migrate-lib.R"))

status <- tryCatch(
    {
        main()
        0L
    },
    error = function(e) {
        message("Seed failed: ", conditionMessage(e))
        1L
    }
)
quit(status = status, save = "no")
