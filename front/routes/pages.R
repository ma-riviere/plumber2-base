# Page routes. Handlers gather data from the backend (R/backend_client.R) and
# delegate the markup to the R/ui_*.R builders; render_page answers both full
# pages and htmx fragments. The auth gate guarantees a session exists before
# any of these run.

#* Root redirects to the Home page.
#* @get /
function(response) {
    redirect(response, "/home")
}

#* Home: dataset stat card + filterable dataset list + upload modal.
#* NOTE: filter params are deliberately untyped - htmx includes empty inputs
#* as empty strings, which plumber2's typed validation rejects with a 400;
#* parse_home_filters() normalizes them instead.
#* @query max_rows Only datasets with at most this many rows
#* @query created_from Only datasets created at/after this date
#* @query created_to Only datasets created at/before this date
#* @get /home
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
        scopes <- session_scopes(state, datastore)
        render_page(
            request,
            response,
            content = home_content(
                datasets$items,
                filters,
                lang,
                state$translations,
                can_write = "write:datasets" %in% scopes
            ),
            title = tr("Home", lang, state$translations),
            lang = lang,
            state = state,
            user = datastore$session$auth,
            scopes = scopes
        )
    })
}

#* Explore: dataset picker + description, summary and paginated preview.
#* @query dataset The selected dataset id (untyped: the picker placeholder submits "")
#* @get /explore
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        explore <- gather_explore(state, datastore, query$dataset)
        scopes <- session_scopes(state, datastore)
        render_page(
            request,
            response,
            content = explore_content(
                explore,
                lang,
                state$translations,
                can_write = "write:datasets" %in% scopes
            ),
            title = tr("Explore", lang, state$translations),
            lang = lang,
            state = state,
            user = datastore$session$auth,
            scopes = scopes
        )
    })
}

#* Model: formula input + async fit with polling, saved models sidebar. The
#* `model` param is the active (loaded) model; a stale id degrades to none.
#* @query dataset The selected dataset id (untyped: the picker placeholder submits "")
#* @query model The active model id (untyped: may be absent or stale)
#* @get /model
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        model <- gather_model(state, datastore, query$dataset, model_id = query$model)
        scopes <- session_scopes(state, datastore)
        render_page(
            request,
            response,
            content = model_content(
                model,
                lang,
                state$translations,
                can_write = "write:models" %in% scopes
            ),
            title = tr("Model", lang, state$translations),
            lang = lang,
            state = state,
            user = datastore$session$auth,
            scopes = scopes
        )
    })
}

#* Admin: user cards (with Auth0 role management) and request-stats from the
#* backend admin endpoints. Tab switching is plain navigation (?tab=).
#* Requires view:admin (the backend enforces it; this render check is UX).
#* @query tab One of users, requests
#* @query hours Request-stats window in hours (default 24)
#* @query seen Users filter: all (default) or recent (last 15 minutes)
#* @get /admin
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        scopes <- session_scopes(state, datastore)
        if (!"view:admin" %in% scopes) {
            response$status <- 403L
            content <- access_denied_content(lang, state$translations)
        } else {
            tab <- if (identical(query$tab, "requests")) "requests" else "users"
            hours <- suppressWarnings(as.integer(query$hours %||% ""))
            if (is.na(hours)) {
                hours <- 24L
            }
            seen <- if (identical(query$seen, "recent")) "recent" else "all"
            data <- switch(
                tab,
                users = be_get(state, datastore, "/v1/admin/users"),
                requests = be_get(state, datastore, "/v1/admin/requests", query = list(hours = hours))
            )
            content <- admin_content(
                tab,
                data,
                hours,
                seen,
                lang,
                state$translations,
                can_manage_roles = "manage:admin:roles" %in% scopes
            )
        }
        render_page(
            request,
            response,
            content = content,
            title = tr("Admin", lang, state$translations),
            lang = lang,
            state = state,
            user = datastore$session$auth,
            scopes = scopes
        )
    })
}

#* Account: API key management (list / create / revoke). The backend only
#* accepts JWT (or dev bypass) credentials on /v1/keys.
#* @get /account
#* @serializer html
function(request, response, server, datastore) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        keys <- be_get(state, datastore, "/v1/keys")
        scopes <- session_scopes(state, datastore)
        render_page(
            request,
            response,
            content = account_content(keys$items, lang, state$translations),
            title = tr("Account", lang, state$translations),
            lang = lang,
            state = state,
            user = datastore$session$auth,
            scopes = scopes
        )
    })
}

#* Set the interface language cookie and redirect back to the referring page.
#* @get /lang/<code:string>
function(code, request, response, server) {
    state <- server$get_data("state")
    if (code %in% state$translations$languages) {
        response$set_cookie(
            "lang",
            code,
            path = "/",
            same_site = "Lax",
            http_only = TRUE
        )
    }
    redirect(response, safe_referer_path(request, state$config$app_url))
}
