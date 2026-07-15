#* Fit a linear model on a dataset, asynchronously. Returns 202 + a job to poll.
#* Guardrails: formula AST allowlist (R/formula_safety.R), idempotent dedupe (an
#* identical live fit returns the same job), and a per-user cap on concurrent
#* jobs (429 beyond it).
#* NOTE: requiredness is enforced in the handler; combining several @body tags
#* with a `*` marker crashes plumber2 0.2.0's file parsing (spike addendum).
#* @body dataset_id:integer The dataset to fit on
#* @body formula:string The model formula, e.g. "mpg ~ wt + hp"
#* @post /v1/models
#* @serializer json
function(body, datastore, response) {
    scope <- require_scope(datastore, response, "write:models")
    if (!isTRUE(scope)) {
        return(scope)
    }
    principal <- request_principal(datastore, response)
    config <- app_config()

    if (is.null(body$dataset_id) || is.null(body$formula)) {
        reqres::abort_bad_request("'dataset_id' and 'formula' are required")
    }
    df <- db_get_dataset_data(app_pool(), principal$user_id, body$dataset_id)
    if (is.null(df)) {
        reqres::abort_not_found("no such dataset")
    }
    formula_str <- trimws(body$formula)
    formula <- tryCatch(
        validate_formula(formula_str, names(df)),
        error = function(e) reqres::abort_http_problem(422L, detail = conditionMessage(e))
    )

    accepted <- function(job_id) {
        response$status <- 202L
        response$set_header("Location", sprintf("/v1/jobs/%s", job_id))
        response$set_header("Retry-After", "2")
        list(job_id = jsonlite::unbox(job_id))
    }

    existing <- db_find_active_fit_job(app_pool(), principal$user_id, body$dataset_id, formula_str)
    if (!is.null(existing)) {
        return(accepted(existing))
    }
    if (db_count_active_jobs(app_pool(), principal$user_id) >= config$max_jobs_per_user) {
        # Manual response: Retry-After would be dropped by the abort_* renderer.
        response$set_header("Retry-After", "5")
        respond_problem(
            response,
            429L,
            "Too Many Requests",
            "too many jobs in flight, retry after the current ones finish"
        )
        return(plumber2::Break)
    }

    job_id <- db_create_job(
        app_pool(),
        principal$user_id,
        "fit_model",
        list(dataset_id = as.integer(body$dataset_id), formula = formula_str)
    )
    launch_fit_job(job_id, principal$user_id, as.integer(body$dataset_id), formula_str, formula, df)
    accepted(job_id)
}

#* List the caller's saved models, optionally for one dataset.
#* @query dataset_id:integer Only models fitted on this dataset
#* @query after:integer Cursor: the smallest model id already seen
#* @query limit:integer Page size (default 20, max 100)
#* @get /v1/models
#* @serializer json
function(query, datastore, response) {
    principal <- request_principal(datastore, response)
    limit <- min(max(query$limit %||% 20L, 1L), 100L)
    rows <- db_list_models(
        app_pool(),
        principal$user_id,
        dataset_id = query$dataset_id,
        after = query$after,
        limit = limit
    )
    list(
        items = lapply(seq_len(nrow(rows)), function(i) model_json(rows[i, ])),
        next_after = if (nrow(rows) == limit) jsonlite::unbox(as.integer(min(rows$id))) else NULL
    )
}

#* One saved model: metrics plus the captured summary() text.
#* @param id:integer The model id
#* @get /v1/models/<id:integer>
#* @serializer json
function(id, datastore, response) {
    principal <- request_principal(datastore, response)
    row <- db_get_model(app_pool(), principal$user_id, id)
    if (is.null(row)) {
        reqres::abort_not_found("no such model")
    }
    model_json(row)
}

#* Delete a saved model.
#* @param id:integer The model id
#* @delete /v1/models/<id:integer>
#* @serializer json
function(id, datastore, response) {
    scope <- require_scope(datastore, response, "write:models")
    if (!isTRUE(scope)) {
        return(scope)
    }
    principal <- request_principal(datastore, response)
    if (!db_delete_model(app_pool(), principal$user_id, id)) {
        reqres::abort_not_found("no such model")
    }
    response$status <- 204L
    plumber2::Break
}
