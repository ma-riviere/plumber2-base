#* List the caller's datasets with cursor pagination and Home-sidebar filters.
#* @query after:integer Cursor: the smallest dataset id already seen (fetches older ones)
#* @query limit:integer Page size (default 20, max 100)
#* @query min_rows:integer Only datasets with at least this many rows
#* @query max_rows:integer Only datasets with at most this many rows
#* @query created_from:string Only datasets created at/after this timestamp (ISO 8601)
#* @query created_to:string Only datasets created at/before this timestamp (ISO 8601)
#* @get /v1/datasets
#* @serializer json
function(query, datastore, response) {
    principal <- request_principal(datastore, response)
    limit <- min(max(query$limit %||% 20L, 1L), 100L)
    rows <- db_list_datasets(
        app_pool(),
        principal$user_id,
        after = query$after,
        limit = limit,
        min_rows = query$min_rows,
        max_rows = query$max_rows,
        created_from = parse_iso_time(query$created_from, "created_from"),
        created_to = parse_iso_time(query$created_to, "created_to")
    )
    list(
        items = lapply(seq_len(nrow(rows)), function(i) dataset_json(rows[i, ])),
        next_after = if (nrow(rows) == limit) jsonlite::unbox(as.integer(min(rows$id))) else NULL
    )
}

#* Upload a dataset as multipart CSV. The proxy and the limits route bound the
#* body size before the parser runs; row/column caps are the third layer.
#* Fields: `file` (the CSV part, required), `name`, `description` (optional).
#* @parser multi
#* @post /v1/datasets
#* @serializer json
function(body, datastore, response) {
    scope <- require_scope(datastore, response, "write:datasets")
    if (!isTRUE(scope)) {
        return(scope)
    }
    principal <- request_principal(datastore, response)
    config <- app_config()

    file_part <- body$file
    if (is.null(file_part)) {
        reqres::abort_bad_request("multipart field 'file' (CSV) is required")
    }
    if (!is.data.frame(file_part)) {
        reqres::abort_http_problem(
            415L,
            detail = "the 'file' part must be CSV (send it with Content-Type: text/csv)"
        )
    }
    df <- as.data.frame(file_part)
    if (nrow(df) == 0 || ncol(df) == 0) {
        reqres::abort_bad_request("the uploaded CSV is empty")
    }
    if (nrow(df) > config$max_dataset_rows || ncol(df) > config$max_dataset_cols) {
        reqres::abort_http_problem(
            413L,
            detail = sprintf(
                "dataset exceeds the limits (%d rows x %d cols max)",
                config$max_dataset_rows,
                config$max_dataset_cols
            )
        )
    }

    name <- body$name %||% sub("\\.[Cc][Ss][Vv]$", "", attr(file_part, "filename") %||% "dataset")
    row <- db_insert_dataset(app_pool(), principal$user_id, name, body$description, df)
    response$status <- 201L
    response$set_header("Location", sprintf("/v1/datasets/%d", as.integer(row$id)))
    dataset_json(row)
}

#* Dataset metadata plus a per-column summary (Explore page).
#* @param id:integer The dataset id
#* @get /v1/datasets/<id:integer>
#* @serializer json
function(id, datastore, response) {
    principal <- request_principal(datastore, response)
    row <- db_get_dataset(app_pool(), principal$user_id, id)
    if (is.null(row)) {
        reqres::abort_not_found("no such dataset")
    }
    df <- db_get_dataset_data(app_pool(), principal$user_id, id)
    c(dataset_json(row), list(summary = dataset_summary(df)))
}

#* Dataset rows as paginated JSON (the preview table). The full-file CSV
#* download lives at /data.csv below: a handler cannot emit a non-JSON body
#* from a JSON endpoint (the negotiated serializer re-encodes even raw bodies,
#* so the planned ?format=csv variant is impossible; documented deviation).
#* @param id:integer The dataset id
#* @query offset:integer Row offset (default 0)
#* @query limit:integer Max rows (default 50, max 500)
#* @get /v1/datasets/<id:integer>/data
#* @serializer json
function(id, query, datastore, response) {
    principal <- request_principal(datastore, response)
    offset <- max(query$offset %||% 0L, 0L)
    limit <- min(max(query$limit %||% 50L, 1L), 500L)
    page <- db_get_dataset_page(app_pool(), principal$user_id, id, offset, limit)
    if (is.null(page)) {
        reqres::abort_not_found("no such dataset")
    }
    list(
        n_rows = jsonlite::unbox(page$n_rows),
        offset = jsonlite::unbox(as.integer(offset)),
        columns = page$columns,
        rows = page$rows
    )
}

#* Download the full dataset as a CSV attachment.
#* @param id:integer The dataset id
#* @get /v1/datasets/<id:integer>/data.csv
#* @serializer csv
function(id, datastore, response) {
    principal <- request_principal(datastore, response)
    df <- db_get_dataset_data(app_pool(), principal$user_id, id)
    if (is.null(df)) {
        reqres::abort_not_found("no such dataset")
    }
    row <- db_get_dataset(app_pool(), principal$user_id, id)
    # The name is user-controlled and reqres writes header values verbatim:
    # control characters (CRLF -> header injection), quotes and slashes must go.
    response$set_header(
        "Content-Disposition",
        sprintf('attachment; filename="%s.csv"', gsub('[[:cntrl:]\\"/]', "_", row$name))
    )
    df
}

#* Rename a dataset or edit its description.
#* @param id:integer The dataset id
#* @body name:string New name
#* @body description:string New description
#* @patch /v1/datasets/<id:integer>
#* @serializer json
function(id, body, datastore, response) {
    scope <- require_scope(datastore, response, "write:datasets")
    if (!isTRUE(scope)) {
        return(scope)
    }
    principal <- request_principal(datastore, response)
    if (is.null(body$name) && is.null(body$description)) {
        reqres::abort_bad_request("nothing to update: provide name and/or description")
    }
    row <- db_update_dataset(
        app_pool(),
        principal$user_id,
        id,
        name = body$name,
        description = body$description
    )
    if (is.null(row)) {
        reqres::abort_not_found("no such dataset")
    }
    dataset_json(row)
}

#* Delete a dataset (its models cascade via the FK).
#* @param id:integer The dataset id
#* @delete /v1/datasets/<id:integer>
#* @serializer json
function(id, datastore, response) {
    scope <- require_scope(datastore, response, "write:datasets")
    if (!isTRUE(scope)) {
        return(scope)
    }
    principal <- request_principal(datastore, response)
    if (!db_delete_dataset(app_pool(), principal$user_id, id)) {
        reqres::abort_not_found("no such dataset")
    }
    response$status <- 204L
    plumber2::Break
}
