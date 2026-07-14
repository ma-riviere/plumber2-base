# Explore page partials.

#* The whole Explore page body for a (new) dataset selection; pushes the
#* canonical /explore URL so the selection stays shareable.
#* @query dataset The selected dataset id (untyped: the picker placeholder submits "")
#* @get /partials/explore/content
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        explore <- gather_explore(state, datastore, query$dataset)
        url <- if (is.null(explore$selected_id)) "/explore" else sprintf("/explore?dataset=%d", explore$selected_id)
        response$set_header("HX-Push-Url", url)
        set_html_headers(response)
        explore_content(
            explore,
            lang,
            state$translations,
            can_write = session_can(state, datastore, "write:datasets")
        )
    })
}

#* One page of the data preview table.
#* @query dataset The dataset id
#* @query offset Row offset
#* @get /partials/explore/preview
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        dataset_id <- suppressWarnings(as.integer(query$dataset))
        if (is.na(dataset_id)) {
            stop(backend_error(404L, "Not Found", "no such dataset"))
        }
        offset <- suppressWarnings(as.integer(query$offset %||% 0L))
        if (is.na(offset) || offset < 0) {
            offset <- 0L
        }
        preview <- be_get(
            state,
            datastore,
            sprintf("/v1/datasets/%d/data", dataset_id),
            query = list(offset = offset, limit = PREVIEW_PAGE_SIZE)
        )
        # A stale page click can point past the end (rows deleted meanwhile):
        # clamp to the last page instead of rendering an empty table.
        n_rows <- as.integer(preview$n_rows %||% 0L)
        if (length(preview$rows) == 0 && n_rows > 0L) {
            offset <- ((n_rows - 1L) %/% PREVIEW_PAGE_SIZE) * PREVIEW_PAGE_SIZE
            preview <- be_get(
                state,
                datastore,
                sprintf("/v1/datasets/%d/data", dataset_id),
                query = list(offset = offset, limit = PREVIEW_PAGE_SIZE)
            )
        }
        set_html_headers(response)
        preview_html(dataset_id, preview, lang, state$translations)
    })
}
