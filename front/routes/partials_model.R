# Model page partials: dataset switch, fit submission, job polling and saved
# models. The fit flow follows the backend contract: POST /v1/models answers
# 202 + job id; the polling fragment re-fetches the job partial until the job
# is terminal (done/error, including jobs failed by the backend's stale-job
# recovery), at which point the returned fragment carries no trigger and the
# polling stops.
#
# Active-model state lives in the URL (/model?dataset=D&model=M, kept canonical
# via HX-Push-Url). Responses that change it re-state the dependent fragments
# together: #model-toolbar (Delete wiring + enabled state), #formula-input,
# #saved-models (highlight) and #fit-status (summary), so they always describe
# the same model.

#* The whole Model page body for a (new) dataset selection.
#* @query dataset The selected dataset id (untyped: the picker placeholder submits "")
#* @get /partials/model/content
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        model <- gather_model(state, datastore, query$dataset)
        url <- if (is.null(model$selected_id)) "/model" else sprintf("/model?dataset=%d", model$selected_id)
        response$set_header("HX-Push-Url", url)
        set_html_headers(response)
        model_content(
            model,
            lang,
            state$translations,
            can_write = session_can(state, datastore, "write:models")
        )
    })
}

#* Submit a model fit. Backend rejections surface as alerts in #fit-status
#* (422 unsafe/invalid formula, 429 job cap); success returns the polling
#* fragment for the created (or deduplicated) job plus an out-of-band toolbar
#* with Fit/Delete disabled for the duration of the job. The pre-fit active
#* model id (hidden `model` field) rides along so a failed fit can restore it.
#* @body dataset:integer The dataset id
#* @body formula:string The model formula
#* @body model:string The currently active model id, if any
#* @parser form
#* @post /models/fit
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        formula <- scalar_field(body$formula) %||% ""
        dataset_id <- suppressWarnings(as.integer(body$dataset))
        if (is.na(dataset_id) || !nzchar(formula)) {
            response$status <- 422L
            return(render_error_alert(
                backend_error(
                    422L,
                    "",
                    tr("Enter an R formula (e.g., y ~ x1 + x2, y ~ poly(x, 2))", lang, state$translations)
                ),
                lang,
                state$translations
            ))
        }
        job <- be_send(
            state,
            datastore,
            "/v1/models",
            "POST",
            body = list(dataset_id = dataset_id, formula = formula)
        )
        set_html_headers(response)
        paste0(
            job_polling_fragment(
                job$job_id,
                dataset_id,
                lang,
                state$translations,
                delay = "1s",
                active_model_id = scalar_field(body$model)
            ),
            model_toolbar_html(
                dataset_id,
                lang,
                state$translations,
                can_write = session_can(state, datastore, "write:models"),
                fitting = TRUE,
                oob = TRUE
            )
        )
    })
}

#* Poll a fit job. Pending/running returns another polling fragment; done
#* returns the model result, marks it active (toolbar/formula/highlight/URL);
#* error returns a terminal alert and re-enables the toolbar for the pre-fit
#* active model. A backend outage mid-poll (5xx) keeps polling instead of
#* silently freezing the fragment.
#* @param id:string The job id
#* @query dataset The dataset id (for the sidebar refresh)
#* @query model The pre-fit active model id, if any
#* @get /partials/model/job/<id:string>
#* @serializer html
function(id, request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    translations <- state$translations
    dataset_id <- suppressWarnings(as.integer(query$dataset %||% NA))
    with_fe_errors(request, response, state, datastore, {
        can_write <- session_can(state, datastore, "write:models")
        result <- tryCatch(
            {
                job <- be_get(state, datastore, sprintf("/v1/jobs/%s", id))
                if (job$status %in% c("pending", "running")) {
                    job_polling_fragment(id, dataset_id, lang, translations, active_model_id = query$model)
                } else if (identical(job$status, "done")) {
                    model <- be_get(state, datastore, sprintf("/v1/models/%d", as.integer(job$result$model_id)))
                    models <- be_get(state, datastore, "/v1/models", query = list(dataset_id = dataset_id))$items
                    response$set_header(
                        "HX-Push-Url",
                        sprintf("/model?dataset=%d&model=%d", dataset_id, as.integer(model$id))
                    )
                    paste0(
                        model_result_fragment(model, lang, translations),
                        saved_models_html(
                            models,
                            dataset_id,
                            lang,
                            translations,
                            active_model_id = model$id,
                            oob = TRUE
                        ),
                        model_toolbar_html(
                            dataset_id,
                            lang,
                            translations,
                            active_model_id = model$id,
                            can_write = can_write,
                            oob = TRUE
                        ),
                        formula_input_oob(model$formula, lang, translations),
                        render_toast(tr("Model fitted successfully", lang, translations), "success")
                    )
                } else {
                    paste0(
                        render_error_alert(
                            backend_error(500L, "", job$error %||% tr("Model fitting failed", lang, translations)),
                            lang,
                            translations
                        ),
                        model_toolbar_html(
                            dataset_id,
                            lang,
                            translations,
                            active_model_id = query$model,
                            can_write = can_write,
                            oob = TRUE
                        ),
                        render_toast(tr("Model fitting failed", lang, translations), "error")
                    )
                }
            },
            fe_backend_error = function(e) {
                if (e$status >= 500L) {
                    job_polling_fragment(
                        id,
                        dataset_id,
                        lang,
                        translations,
                        delay = "3s",
                        active_model_id = query$model
                    )
                } else {
                    stop(e)
                }
            }
        )
        set_html_headers(response)
        result
    })
}

#* Load a saved model into the results area and make it the active model
#* (formula, toolbar Delete, sidebar highlight and URL follow).
#* @param id:integer The model id
#* @get /partials/model/saved/<id:integer>
#* @serializer html
function(id, request, response, server, datastore) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        model <- be_get(state, datastore, sprintf("/v1/models/%d", as.integer(id)))
        dataset_id <- as.integer(model$dataset_id)
        models <- be_get(state, datastore, "/v1/models", query = list(dataset_id = dataset_id))$items
        response$set_header("HX-Push-Url", sprintf("/model?dataset=%d&model=%d", dataset_id, as.integer(model$id)))
        set_html_headers(response)
        paste0(
            model_result_fragment(model, lang, state$translations),
            saved_models_html(
                models,
                dataset_id,
                lang,
                state$translations,
                active_model_id = model$id,
                oob = TRUE
            ),
            model_toolbar_html(
                dataset_id,
                lang,
                state$translations,
                active_model_id = model$id,
                can_write = session_can(state, datastore, "write:models"),
                oob = TRUE
            ),
            formula_input_oob(model$formula, lang, state$translations)
        )
    })
}

#* Delete a saved model (the delete buttons use hx-swap="none"). Always
#* refreshes the saved-models sidebar; when the deleted model was the ACTIVE
#* one (`model` carries the active id), also clears the results area, the
#* formula, the toolbar and the pushed URL in the same response.
#* @param id:integer The model id
#* @query dataset The dataset id (for the sidebar refresh)
#* @query model The currently active model id, if any
#* @delete /models/<id:integer>
#* @serializer html
function(id, request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        be_send(state, datastore, sprintf("/v1/models/%d", as.integer(id)), "DELETE")
        dataset_id <- suppressWarnings(as.integer(query$dataset %||% NA))
        active_id <- suppressWarnings(as.integer(query$model %||% NA))
        was_active <- is.na(active_id) || identical(active_id, as.integer(id))
        oob <- ""
        if (!is.na(dataset_id)) {
            models <- be_get(state, datastore, "/v1/models", query = list(dataset_id = dataset_id))$items
            oob <- saved_models_html(
                models,
                dataset_id,
                lang,
                state$translations,
                active_model_id = if (was_active) NULL else active_id,
                oob = TRUE
            )
            if (was_active) {
                response$set_header("HX-Push-Url", sprintf("/model?dataset=%d", dataset_id))
                oob <- paste0(
                    oob,
                    fit_status_clear_oob(),
                    model_toolbar_html(
                        dataset_id,
                        lang,
                        state$translations,
                        can_write = session_can(state, datastore, "write:models"),
                        oob = TRUE
                    ),
                    formula_input_oob("", lang, state$translations)
                )
            }
        }
        set_html_headers(response)
        paste0(render_toast(tr("Model deleted", lang, state$translations), "success"), oob)
    })
}
