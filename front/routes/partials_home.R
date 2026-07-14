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

#* Upload a CSV dataset (multipart proxy to the backend). Success closes the
#* modal and refreshes the dataset panel; backend rejections (413/415/422)
#* come back as an alert swapped into #upload-status.
#* @parser multi
#* @post /datasets/upload
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        file_part <- body$file
        if (is.null(file_part)) {
            response$status <- 422L
            return(render_error_alert(
                backend_error(422L, "", tr("Please select a valid CSV file", lang, state$translations)),
                lang,
                state$translations
            ))
        }
        filename <- attr(file_part, "filename") %||% "dataset.csv"
        csv_bytes <- part_as_csv_bytes(file_part)
        if (is.null(csv_bytes)) {
            response$status <- 422L
            return(render_error_alert(
                backend_error(422L, "", tr("Please select a valid CSV file", lang, state$translations)),
                lang,
                state$translations
            ))
        }
        be_upload_dataset(
            state,
            datastore,
            csv_bytes,
            filename = filename,
            name = scalar_field(body$name),
            description = scalar_field(body$description)
        )
        response$set_header("HX-Trigger", "fb:close-modal, fb:refresh-datasets")
        set_html_headers(response)
        render_toast(tr("Dataset uploaded successfully", lang, state$translations), "success")
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
