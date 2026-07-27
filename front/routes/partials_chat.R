# Dataset chat partials. All three are behind the FE gate (the POSTs therefore
# also carry the gate's CSRF + Origin checks) and re-check chat eligibility
# server-side: hiding the widget is not access control.
#
# Only #chat-stream is polled, with the self-replacing `load delay:1s` fragment
# the fit-job flow uses - plumber2/fiery have no server push. Each fragment
# carries the chat generation id, and a poll whose id no longer matches the
# live session answers 204 (htmx is configured not to swap 204s), so a stale
# poll can neither repaint nor keep polling over a newer conversation.

#* Submit a question. Creates the session (workdir + pi subprocess + awaited
#* set_model) on first use, replaces it atomically when the dataset changed,
#* and refuses a second turn while one is in flight with 409.
#* @body dataset:integer The dataset id the chat is bound to
#* @body message:string The user's question
#* @parser form
#* @post /partials/chat/send
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    translations <- state$translations
    dataset_id <- suppressWarnings(as.integer(body$dataset))
    with_fe_errors(request, response, state, datastore, {
        denial <- chat_denial(state, datastore)
        if (!is.null(denial)) {
            response$status <- 403L
            set_html_headers(response)
            return(chat_stream_html(NULL, dataset_id, lang, translations, notice = denial))
        }
        prompt <- scalar_field(body$message) %||% ""
        key <- chat_session_key(datastore)
        session <- chat_get(key)
        if (is.na(dataset_id) || !nzchar(prompt)) {
            response$status <- 422L
            set_html_headers(response)
            return(chat_stream_html(
                session,
                dataset_id,
                lang,
                translations,
                notice = "Ask a question about this dataset"
            ))
        }
        if (nchar(prompt) > CHAT_MAX_PROMPT_CHARS) {
            response$status <- 422L
            set_html_headers(response)
            return(chat_stream_html(session, dataset_id, lang, translations, notice = "Your question is too long"))
        }
        # hx-sync only guards one tab; a second tab (or a replayed POST) has to
        # be refused here.
        if (chat_is_active(session) && identical(session$dataset_id, dataset_id)) {
            response$status <- 409L
            set_html_headers(response)
            return(chat_stream_html(
                session,
                dataset_id,
                lang,
                translations,
                notice = "Please wait for the current answer to finish"
            ))
        }
        create <- chat_needs_create(session, dataset_id)
        if (create && length(chat_live_sessions()) >= state$config$chat$max_sessions) {
            response$status <- 503L
            set_html_headers(response)
            return(chat_stream_html(
                session,
                dataset_id,
                lang,
                translations,
                notice = "The assistant is busy, please try again in a moment"
            ))
        }
        auth <- datastore$session$auth
        if (!chat_quota_take(auth$user_id, state$config$chat$daily_turns)) {
            response$status <- 429L
            set_html_headers(response)
            return(chat_stream_html(
                session,
                dataset_id,
                lang,
                translations,
                notice = "You have reached today's question limit"
            ))
        }
        with_chat_errors(response, dataset_id, lang, translations, {
            detail <- if (create) be_get(state, datastore, sprintf("/v1/datasets/%d", dataset_id)) else NULL
            session <- chat_submit(state, datastore, key, auth$user_id, dataset_id, lang, prompt, detail)
            set_html_headers(response)
            paste0(
                chat_stream_html(session, dataset_id, lang, translations),
                chat_form_html(dataset_id, lang, translations, oob = TRUE)
            )
        })
    })
}

#* Poll the transcript while a turn is in flight. `chat` is the generation id
#* the fragment was rendered with; a mismatch means the conversation was reset
#* or replaced, and the poll is answered with 204 (no swap, no further poll).
#* @query chat The chat generation id (untyped: absent on the first render)
#* @query dataset The dataset id the widget is mounted for
#* @get /partials/chat/stream
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    dataset_id <- suppressWarnings(as.integer(query$dataset %||% NA))
    with_fe_errors(request, response, state, datastore, {
        denial <- chat_denial(state, datastore)
        if (!is.null(denial)) {
            response$status <- 403L
            set_html_headers(response)
            return(chat_stream_html(NULL, dataset_id, lang, state$translations, notice = denial))
        }
        session <- chat_get(chat_session_key(datastore))
        if (is.null(session) || !identical(session$id, query$chat %||% "")) {
            response$status <- 204L
            set_html_headers(response)
            return("")
        }
        session$last_poll_at <- as.numeric(Sys.time())
        set_html_headers(response)
        chat_stream_html(session, session$dataset_id, lang, state$translations)
    })
}

#* Start a new chat: full cleanup ladder (abort, kill, workdir removal), the
#* persisted store for this (user, dataset) forgotten - pi session files and
#* transcript sidecar both - and an empty transcript. The next question starts
#* a fresh session with no memory of the old conversation.
#* @body dataset:integer The dataset id the widget is mounted for
#* @parser form
#* @post /partials/chat/reset
#* @serializer html
function(request, response, server, datastore, body) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    dataset_id <- suppressWarnings(as.integer(body$dataset))
    with_fe_errors(request, response, state, datastore, {
        denial <- chat_denial(state, datastore)
        if (!is.null(denial)) {
            response$status <- 403L
            set_html_headers(response)
            return(chat_stream_html(NULL, dataset_id, lang, state$translations, notice = denial))
        }
        chat_cleanup_key(chat_session_key(datastore))
        if (!is.na(dataset_id)) {
            chat_forget(state, datastore$session$auth$user_id, dataset_id)
        }
        set_html_headers(response)
        paste0(
            chat_stream_html(NULL, dataset_id, lang, state$translations),
            chat_form_html(dataset_id, lang, state$translations, oob = TRUE)
        )
    })
}
