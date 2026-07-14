# Dataset persistence and shaping. Rows are stored as jsonb (yyjsonr, parity
# with shiny-base); every accessor is scoped to the owning user so ownership
# isolation is structural, not per-endpoint discipline. Missing/foreign ids
# return NULL and endpoints answer 404 (existence is not leaked).

DATASET_COLUMNS <- "id, user_id, name, description, n_rows, n_cols, created_at, updated_at"

# yyjsonr drops data.frame rownames on write (jsonlite emitted them as a "_row"
# string field per row object and restores it on parse). Keep meaningful
# rownames (e.g. mtcars car models) through the same jsonlite-compatible field:
# injected into the serialized rows ONLY - never listed in `columns`, never
# counted in n_cols - and restored to real rownames on read. Numeric-looking
# rownames (subset leftovers like "3", "5") are noise and are not persisted.
# SYNC CONTRACT: mirrored in shiny-base helpers_database.R (shared datasets).
inject_rownames <- function(df) {
    if (.row_names_info(df) <= 0L) {
        return(df)
    }
    if (!is.character(utils::type.convert(rownames(df), as.is = TRUE))) {
        return(df)
    }
    df[["_row"]] <- rownames(df)
    rownames(df) <- NULL
    df
}

restore_rownames <- function(df) {
    if (is.data.frame(df) && "_row" %in% names(df)) {
        rownames(df) <- df[["_row"]]
        df[["_row"]] <- NULL
    }
    df
}

db_insert_dataset <- function(pool, user_id, name, description, df) {
    DBI::dbGetQuery(
        pool,
        sprintf(
            "INSERT INTO datasets (user_id, name, description, data, n_rows, n_cols)
             VALUES ($1, $2, $3, $4::jsonb, $5, $6) RETURNING %s",
            DATASET_COLUMNS
        ),
        params = list(
            user_id,
            name,
            description %||% NA,
            # jsonb normalizes OBJECT key order (length, then bytewise), so the
            # column order must ride in an array, which jsonb preserves.
            # Rownames ride inside the row objects as "_row" (inject_rownames);
            # `columns` and n_cols stay rownames-free.
            yyjsonr::write_json_str(list(columns = names(df), rows = inject_rownames(df))),
            nrow(df),
            ncol(df)
        )
    )
}

# Cursor pagination: ORDER BY id DESC, `after` = the last id already seen.
# Filters mirror the Home sidebar (row-count range, creation date range).
db_list_datasets <- function(
    pool,
    user_id,
    after = NULL,
    limit = 20L,
    min_rows = NULL,
    max_rows = NULL,
    created_from = NULL,
    created_to = NULL
) {
    clauses <- "user_id = $1"
    params <- list(user_id)
    add <- function(clause, value) {
        params[[length(params) + 1]] <<- value
        clauses <<- c(clauses, sprintf(clause, length(params)))
    }
    if (!is.null(after)) {
        add("id < $%d", after)
    }
    if (!is.null(min_rows)) {
        add("n_rows >= $%d", min_rows)
    }
    if (!is.null(max_rows)) {
        add("n_rows <= $%d", max_rows)
    }
    if (!is.null(created_from)) {
        add("created_at >= $%d", created_from)
    }
    if (!is.null(created_to)) {
        add("created_at <= $%d", created_to)
    }
    params[[length(params) + 1]] <- limit
    DBI::dbGetQuery(
        pool,
        sprintf(
            "SELECT %s FROM datasets WHERE %s ORDER BY id DESC LIMIT $%d",
            DATASET_COLUMNS,
            paste(clauses, collapse = " AND "),
            length(params)
        ),
        params = params
    )
}

db_get_dataset <- function(pool, user_id, dataset_id) {
    row <- DBI::dbGetQuery(
        pool,
        sprintf("SELECT %s FROM datasets WHERE id = $1 AND user_id = $2", DATASET_COLUMNS),
        params = list(dataset_id, user_id)
    )
    if (nrow(row) == 0) NULL else row
}

# One page of rows for the preview table, sliced in Postgres (not in R). The
# preview paginates, and loading + parsing the whole jsonb blob per page just to
# return a handful of rows is wasteful on the single R thread at large row
# counts; jsonb_array_elements + LIMIT-style ordinal filtering does the slice
# server-side and ships only the page. Returns list(columns, n_rows, rows) or
# NULL when the dataset is not the caller's. Row objects come back with jsonb-
# normalized key order, so columns are reordered to the stored `columns` array.
db_get_dataset_page <- function(pool, user_id, dataset_id, offset, limit) {
    row <- DBI::dbGetQuery(
        pool,
        "SELECT data->'columns' AS columns,
                jsonb_array_length(data->'rows') AS n_rows,
                (SELECT jsonb_agg(elem ORDER BY ord)
                   FROM jsonb_array_elements(data->'rows') WITH ORDINALITY AS t(elem, ord)
                  WHERE ord > $3 AND ord <= $3 + $4) AS rows
         FROM datasets WHERE id = $1 AND user_id = $2",
        params = list(dataset_id, user_id, as.integer(offset), as.integer(limit))
    )
    if (nrow(row) == 0) {
        return(NULL)
    }
    columns_json <- row$columns[[1]]
    # Legacy bare-array shape (no {columns, rows} wrapper): data->'columns' is
    # NULL, so fall back to the full parse + R slice. Only the original dev seed
    # ever produced this shape.
    if (is.na(columns_json)) {
        df <- db_get_dataset_data(pool, user_id, dataset_id)
        if (is.null(df)) {
            return(NULL)
        }
        idx <- seq_len(nrow(df))
        idx <- idx[idx > offset & idx <= offset + limit]
        return(list(columns = names(df), n_rows = nrow(df), rows = df[idx, , drop = FALSE]))
    }
    columns <- unlist(yyjsonr::read_json_str(columns_json), use.names = FALSE)
    rows_json <- row$rows[[1]]
    rows <- if (is.na(rows_json)) {
        empty <- as.data.frame(matrix(nrow = 0, ncol = length(columns)))
        stats::setNames(empty, columns)
    } else {
        as.data.frame(yyjsonr::read_json_str(rows_json))[, columns, drop = FALSE]
    }
    list(columns = columns, n_rows = as.integer(row$n_rows), rows = rows)
}

# The parsed rows (data.frame), or NULL when the dataset is not the caller's.
db_get_dataset_data <- function(pool, user_id, dataset_id) {
    row <- DBI::dbGetQuery(
        pool,
        "SELECT data FROM datasets WHERE id = $1 AND user_id = $2",
        params = list(dataset_id, user_id)
    )
    if (nrow(row) == 0) {
        return(NULL)
    }
    parsed <- yyjsonr::read_json_str(row$data[[1]])
    # Wrapped shape check must come FIRST: yyjsonr promotes the wrapper object
    # itself to a data.frame ({rows: [...], columns: [...]} parses as a 2-column
    # df whose $rows is the real data).
    if (!is.null(parsed$columns) && !is.null(parsed$rows)) {
        df <- restore_rownames(as.data.frame(parsed$rows))
        return(df[, unlist(parsed$columns, use.names = FALSE), drop = FALSE])
    }
    # Legacy shape (bare row array, e.g. the original dev seed): column order is
    # whatever jsonb normalized it to.
    parsed
}

db_update_dataset <- function(pool, user_id, dataset_id, name = NULL, description = NULL) {
    sets <- character()
    params <- list()
    if (!is.null(name)) {
        params[[length(params) + 1]] <- name
        sets <- c(sets, sprintf("name = $%d", length(params)))
    }
    if (!is.null(description)) {
        params[[length(params) + 1]] <- description
        sets <- c(sets, sprintf("description = $%d", length(params)))
    }
    if (length(sets) == 0) {
        return(db_get_dataset(pool, user_id, dataset_id))
    }
    params[[length(params) + 1]] <- dataset_id
    params[[length(params) + 1]] <- user_id
    row <- DBI::dbGetQuery(
        pool,
        sprintf(
            "UPDATE datasets SET %s, updated_at = now() WHERE id = $%d AND user_id = $%d RETURNING %s",
            paste(sets, collapse = ", "),
            length(params) - 1,
            length(params),
            DATASET_COLUMNS
        ),
        params = params
    )
    if (nrow(row) == 0) NULL else row
}

# Models on the dataset go with it via the FK cascade.
db_delete_dataset <- function(pool, user_id, dataset_id) {
    DBI::dbExecute(
        pool,
        "DELETE FROM datasets WHERE id = $1 AND user_id = $2",
        params = list(dataset_id, user_id)
    ) >
        0
}

# Parse an ISO 8601 timestamp query value; NULL passes through, garbage aborts
# with a 400 naming the parameter.
parse_iso_time <- function(value, name) {
    if (is.null(value)) {
        return(NULL)
    }
    parsed <- tryCatch(
        as.POSIXct(
            value,
            tz = "UTC",
            tryFormats = c(
                "%Y-%m-%dT%H:%M:%SZ",
                "%Y-%m-%dT%H:%M:%S",
                "%Y-%m-%d"
            )
        ),
        error = function(e) NA
    )
    if (is.na(parsed)) {
        reqres::abort_bad_request(sprintf("'%s' must be an ISO 8601 timestamp", name))
    }
    parsed
}

# Compact per-column summary for the Explore page: type, missing count, and
# numeric range/mean or distinct-value count.
dataset_summary <- function(df) {
    lapply(stats::setNames(names(df), names(df)), function(col) {
        values <- df[[col]]
        base <- list(
            type = jsonlite::unbox(class(values)[1]),
            n_missing = jsonlite::unbox(sum(is.na(values)))
        )
        if (is.numeric(values)) {
            ok <- values[!is.na(values)]
            c(
                base,
                list(
                    min = jsonlite::unbox(if (length(ok)) min(ok) else NA),
                    max = jsonlite::unbox(if (length(ok)) max(ok) else NA),
                    mean = jsonlite::unbox(if (length(ok)) mean(ok) else NA)
                )
            )
        } else {
            c(base, list(n_unique = jsonlite::unbox(length(unique(values[!is.na(values)])))))
        }
    })
}

# One dataset row shaped for JSON output (unboxed scalars).
dataset_json <- function(row) {
    list(
        id = jsonlite::unbox(as.integer(row$id)),
        name = jsonlite::unbox(row$name),
        description = jsonlite::unbox(row$description),
        n_rows = jsonlite::unbox(row$n_rows),
        n_cols = jsonlite::unbox(row$n_cols),
        created_at = jsonlite::unbox(format(row$created_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
        updated_at = jsonlite::unbox(format(row$updated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    )
}
