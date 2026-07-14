# Admin partials: Auth0 role management (modal + update). The backend is the
# enforcer (view:admin to read, manage:admin:roles to change); these handlers
# only shape the UI and surface backend rejections (409 self-demotion, 422
# guest, 503 mgmt client unconfigured) as alerts inside the modal.

#* The role-edit modal for one user, served into #modal-slot.
#* @param id:integer The user id
#* @get /partials/admin/users/<id:integer>/role
#* @serializer html
function(id, request, response, server, datastore) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        user <- admin_find_user(state, datastore, id)
        if (is.null(user)) {
            stop(backend_error(404L, "Not Found", "no such user"))
        }
        roles <- be_get(state, datastore, "/v1/admin/roles")
        set_html_headers(response)
        admin_role_modal_html(user, roles$items, be_scalar(roles$default_role), lang, state$translations)
    })
}

#* Apply a role change, refresh that user's card out-of-band and close the
#* modal. The card swap is silently discarded when the card is not on screen
#* (e.g. the Recently-active filter hides it) - the next render re-fetches.
#* @param id:integer The user id
#* @body role_id:string The Auth0 role id to assign ("" clears to the default role)
#* @parser form
#* @put /admin/users/<id:integer>/role
#* @serializer html
function(id, request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    with_fe_errors(request, response, state, datastore, {
        be_send(
            state,
            datastore,
            sprintf("/v1/admin/users/%d/role", as.integer(id)),
            "PUT",
            body = list(role_id = scalar_field(body$role_id) %||% "")
        )
        user <- admin_find_user(state, datastore, id)
        response$set_header("HX-Trigger", "fb:close-modal")
        set_html_headers(response)
        paste0(
            if (!is.null(user)) {
                admin_user_card_html(user, lang, state$translations, can_manage_roles = TRUE, oob = TRUE)
            } else {
                ""
            },
            render_toast(tr("Role updated", lang, state$translations), "success")
        )
    })
}
