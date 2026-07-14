# Profile modal: server-rendered content and the save action. Guests have no
# Auth0 profile: the navbar disables the trigger, and both handlers refuse
# guest sessions anyway (defense in depth).

#* The profile modal content, swapped into #modal-slot.
#* @get /partials/profile
#* @serializer html
function(request, response, server, datastore) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    auth <- session_auth(datastore)
    if (is.null(auth) || isTRUE(auth$is_guest)) {
        response$status <- 403L
        return(render_error_alert(
            backend_error(403L, "", tr("Access denied", lang, state$translations)),
            lang,
            state$translations
        ))
    }
    set_html_headers(response)
    profile_modal_content(auth, lang, state$translations)
}

#* Save the profile: nickname goes to Auth0 (Management API, enforced to the
#* session's own sub) and the local users mirror; the language preference is
#* the lang cookie. A language change answers HX-Refresh so the whole page
#* re-renders with the new labels.
#* @body nickname:string The new nickname
#* @body language:string The preferred interface language
#* @parser form
#* @post /profile
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    config <- state$config
    lang <- resolve_lang(request, state$translations)
    translations <- state$translations
    auth <- session_auth(datastore)
    if (is.null(auth) || isTRUE(auth$is_guest) || is.null(auth$sub)) {
        response$status <- 403L
        return(render_error_alert(
            backend_error(403L, "", tr("Access denied", lang, translations)),
            lang,
            translations
        ))
    }

    nickname <- scalar_field(body$nickname)
    if (is.null(nickname)) {
        response$status <- 422L
        return(render_error_alert(
            backend_error(422L, "", tr("Nickname cannot be empty", lang, translations)),
            lang,
            translations
        ))
    }

    if (!identical(nickname, auth$nickname)) {
        mgmt_ok <- tryCatch(
            {
                if (nzchar(config$auth0$mgmt_client_id %||% "")) {
                    mgmt_update_nickname(config, auth$sub, nickname)
                }
                TRUE
            },
            error = function(e) FALSE
        )
        if (!mgmt_ok) {
            response$status <- 502L
            return(render_error_alert(
                backend_error(502L, "", tr("Error updating profile:", lang, translations)),
                lang,
                translations
            ))
        }
        fe_update_nickname(state$con, auth$user_id, nickname)
        auth$nickname <- nickname
        datastore$session$auth <- auth
    }

    language <- scalar_field(body$language)
    language_changed <- !is.null(language) && language %in% state$translations$languages && !identical(language, lang)
    if (language_changed) {
        response$set_cookie("lang", language, path = "/", same_site = "Lax", http_only = TRUE)
        response$set_header("HX-Refresh", "true")
        return("")
    }

    response$set_header("HX-Trigger", "fb:close-modal")
    set_html_headers(response)
    paste0(
        render_toast(tr("Profile updated successfully", lang, translations), "success"),
        navbar_name_oob(nickname)
    )
}
