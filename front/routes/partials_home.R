# Home page partials and dataset actions. All handlers proxy the backend with
# the session's credentials (R/backend_client.R); action responses raise
# fb:refresh-datasets via HX-Trigger so #home-data re-fetches itself with the
# current sidebar filters.

#* The refreshable dataset panel (stat card + list), filtered. Pushes the
#* canonical /home URL for the filter state so it stays shareable.
#* NOTE: params untyped - htmx includes empty inputs as "" and plumber2's
#* typed validation would 400; parse_home_filters() normalizes them.
#* @query max_rows Only datasets with at most this many rows
#* @query created_from Only datasets created at/after this date
#* @query created_to Only datasets created at/before this date
#* @get /partials/home/datasets
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        filters <- parse_home_filters(query)
        datasets <- be_get(
            state,
            datastore,
            "/v1/datasets",
            query = c(list(limit = 100L), home_filters_be_query(filters))
        )
        response$set_header("HX-Push-Url", home_filters_url(filters))
        set_html_headers(response)
        home_data_panel(
            datasets$items,
            lang,
            state$translations,
            can_write = session_can(state, datastore, "write:datasets")
        )
    })
}

#* A single dataset row (used by the inline-rename Cancel button). Neutral
#* path: the row is shared by Home and Explore, context picks the flavour.
#* @param id:integer The dataset id
#* @query context Row flavour, home (default) or explore
#* @get /partials/dataset/<id:integer>/row
#* @serializer html
function(id, request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        ds <- be_get(state, datastore, sprintf("/v1/datasets/%d", as.integer(id)))
        set_html_headers(response)
        dataset_row_html(
            ds,
            lang,
            state$translations,
            can_write = session_can(state, datastore, "write:datasets"),
            context = dataset_row_context(query)
        )
    })
}

#* The inline-rename form for a dataset row.
#* @param id:integer The dataset id
#* @query context Row flavour, home (default) or explore
#* @get /partials/dataset/<id:integer>/edit
#* @serializer html
function(id, request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        ds <- be_get(state, datastore, sprintf("/v1/datasets/%d", as.integer(id)))
        set_html_headers(response)
        dataset_row_edit_html(ds, lang, state$translations, context = dataset_row_context(query))
    })
}

#* Upload one CSV dataset per selected file (multipart proxy to the backend).
#* Success closes the modal and refreshes the dataset panel; backend rejections
#* (413/415/422) come back as an alert swapped into #upload-status. A partial
#* multi-file failure answers 200 so the panel still refreshes, with the failed
#* files listed in the alert.
#* @parser multi
#* @post /datasets/upload
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        parts <- upload_file_parts(body)
        invalid_csv <- tr("Please select a valid CSV file", lang, state$translations)
        if (length(parts) == 0) {
            response$status <- 422L
            return(render_error_alert(backend_error(422L, "", invalid_csv), lang, state$translations))
        }
        # A lone file honors the optional name/description; several files each
        # become a dataset named after their own file (the backend's default).
        single <- length(parts) == 1
        uploaded <- 0L
        failures <- character()
        first_status <- NULL
        for (part in parts) {
            filename <- attr(part, "filename") %||% "dataset.csv"
            csv_bytes <- part_as_csv_bytes(part)
            failure <- if (is.null(csv_bytes)) {
                backend_error(422L, "", invalid_csv)
            } else {
                tryCatch(
                    {
                        be_upload_dataset(
                            state,
                            datastore,
                            csv_bytes,
                            filename = filename,
                            name = if (single) scalar_field(body$name),
                            description = if (single) scalar_field(body$description)
                        )
                        NULL
                    },
                    fe_backend_error = \(e) e
                )
            }
            if (is.null(failure)) {
                uploaded <- uploaded + 1L
                next
            }
            detail <- if (nzchar(failure$detail)) failure$detail else failure$title
            failures <- c(
                failures,
                if (single) {
                    detail
                } else {
                    sprintf(tr("%s: Error saving - %s", lang, state$translations), filename, detail)
                }
            )
            first_status <- first_status %||% failure$status
        }
        set_html_headers(response)
        if (length(failures) > 0) {
            alert <- render_error_alert(
                backend_error(first_status, "", paste(failures, collapse = "; ")),
                lang,
                state$translations
            )
            if (uploaded == 0L) {
                response$status <- first_status
                return(alert)
            }
            response$set_header("HX-Trigger", "fb:refresh-datasets")
            toast <- sprintf(
                tr("%s of %s datasets uploaded", lang, state$translations),
                uploaded,
                uploaded + length(failures)
            )
            return(paste0(alert, render_toast(toast, "warning")))
        }
        response$set_header("HX-Trigger", "fb:close-modal, fb:refresh-datasets")
        message <- if (uploaded == 1L) {
            tr("Dataset uploaded successfully", lang, state$translations)
        } else {
            sprintf(tr("%s datasets uploaded successfully", lang, state$translations), uploaded)
        }
        render_toast(message, "success")
    })
}

#* Rename a dataset (inline form). Home returns the updated row in place;
#* Explore answers with an HX-Redirect to the canonical /explore URL so the
#* whole page (sidebar picker name included) re-renders.
#* @param id:integer The dataset id
#* @query context Row flavour, home (default) or explore
#* @body name:string The new dataset name
#* @parser form
#* @patch /datasets/<id:integer>
#* @serializer html
function(id, request, response, server, datastore, query, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        context <- dataset_row_context(query)
        name <- trimws(scalar_field(body$name) %||% "")
        if (!nzchar(name)) {
            ds <- be_get(state, datastore, sprintf("/v1/datasets/%d", as.integer(id)))
            set_html_headers(response)
            return(dataset_row_edit_html(
                ds,
                lang,
                state$translations,
                error = tr("Dataset name cannot be empty", lang, state$translations),
                context = context
            ))
        }
        ds <- be_send(state, datastore, sprintf("/v1/datasets/%d", as.integer(id)), "PATCH", body = list(name = name))
        set_html_headers(response)
        if (identical(context, "explore")) {
            response$set_header("HX-Redirect", sprintf("/explore?dataset=%d", as.integer(id)))
            return("")
        }
        paste0(
            dataset_row_html(ds, lang, state$translations, can_write = TRUE),
            render_toast(tr("Dataset renamed successfully", lang, state$translations), "success")
        )
    })
}

#* Delete a dataset (models cascade on the backend). The button uses
#* hx-swap="none": on Home only the toast (oob) and the refresh trigger
#* matter; on Explore the deleted dataset was the displayed one, so answer
#* with an HX-Redirect back to the /explore picker state.
#* @param id:integer The dataset id
#* @query context Row flavour, home (default) or explore
#* @delete /datasets/<id:integer>
#* @serializer html
function(id, request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        be_send(state, datastore, sprintf("/v1/datasets/%d", as.integer(id)), "DELETE")
        set_html_headers(response)
        if (identical(dataset_row_context(query), "explore")) {
            response$set_header("HX-Redirect", "/explore")
            return("")
        }
        response$set_header("HX-Trigger", "fb:refresh-datasets")
        render_toast(tr("Dataset deleted successfully", lang, state$translations), "success")
    })
}
