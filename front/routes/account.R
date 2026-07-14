# Account page actions: API key create / revoke, proxied to the backend's
# JWT-only /v1/keys endpoints with the session's token.

#* Create an API key. The one-time secret is rendered into #key-result; the
#* keys table refreshes out-of-band. Backend rejections (409 duplicate name,
#* 400 missing name) swap into #key-result as alerts.
#* @body name:string The key name
#* @body scopes:[string] Requested scopes (bounded server-side)
#* @body expires_at:string Optional expiry date
#* @parser form
#* @post /keys
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        payload <- list(
            name = scalar_field(body$name),
            scopes = as.list(unlist(body$scopes, use.names = FALSE) %||% character())
        )
        expires_at <- scalar_field(body$expires_at)
        if (!is.null(expires_at)) {
            payload$expires_at <- expires_at
        }
        created <- be_send(state, datastore, "/v1/keys", "POST", body = payload)
        keys <- be_get(state, datastore, "/v1/keys")
        set_html_headers(response)
        paste0(
            key_created_html(created, lang, state$translations),
            keys_table_html(keys$items, lang, state$translations, oob = TRUE)
        )
    })
}

#* Revoke an API key (hx-swap="none": the toast and the refreshed table are
#* the only visible effects).
#* @param id:integer The key id
#* @delete /keys/<id:integer>
#* @serializer html
function(id, request, response, server, datastore) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        be_send(state, datastore, sprintf("/v1/keys/%d", as.integer(id)), "DELETE")
        keys <- be_get(state, datastore, "/v1/keys")
        set_html_headers(response)
        paste0(
            render_toast(tr("API key revoked", lang, state$translations), "success"),
            keys_table_html(keys$items, lang, state$translations, oob = TRUE)
        )
    })
}
