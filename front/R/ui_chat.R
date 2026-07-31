# Dataset chat widget: a floating launcher (bottom-right, collapsed on every
# page load) that expands into a card panel with the transcript, the activity
# indicator and the input form.
#
# Only #chat-stream polls. The form is deliberately OUTSIDE the polled region:
# re-rendering it every second would fight the user's typing and focus. Every
# polling fragment carries the CHAT GENERATION id, so a poll belonging to a
# reset/replaced conversation can never swap itself over a newer one.

chat_stream_url <- function(chat_id) {
    sprintf("/partials/chat/stream?chat=%s", utils::URLencode(chat_id %||% "", reserved = TRUE))
}

# The whole widget, mounted on Explore when a dataset is selected.
chat_widget_html <- function(session, dataset_id, lang, translations) {
    htmltools::div(
        class = "chat-widget",
        id = "chat-widget",
        htmltools::tags$button(
            type = "button",
            class = "btn btn-primary chat-launcher",
            id = "chat-launcher",
            `data-chat-toggle` = "",
            `aria-controls` = "chat-panel",
            `aria-expanded` = "false",
            bs_icon("chat-dots", class = "me-1"),
            tr("Ask about this dataset", lang, translations)
        ),
        htmltools::div(
            class = "card chat-panel d-none",
            id = "chat-panel",
            htmltools::div(
                class = "card-header d-flex align-items-center justify-content-between",
                htmltools::div(
                    class = "d-flex align-items-center gap-1",
                    htmltools::tags$span(class = "fw-semibold", tr("Dataset assistant", lang, translations)),
                    htmltools::HTML(chat_model_info_html(session, lang, translations))
                ),
                htmltools::div(
                    class = "d-flex align-items-center gap-2",
                    htmltools::tags$button(
                        type = "button",
                        class = "btn-close",
                        `data-chat-toggle` = "",
                        `aria-label` = tr("Close", lang, translations)
                    ),
                    htmltools::tags$button(
                        type = "button",
                        class = "btn btn-sm btn-outline-secondary text-nowrap",
                        `hx-post` = "/partials/chat/reset",
                        `hx-vals` = sprintf('{"dataset": %d}', as.integer(dataset_id)),
                        `hx-target` = "#chat-stream",
                        `hx-swap` = "outerHTML",
                        `hx-sync` = "#chat-stream:replace",
                        tr("New chat", lang, translations)
                    )
                )
            ),
            htmltools::div(
                class = "card-body chat-body",
                htmltools::HTML(chat_stream_html(session, dataset_id, lang, translations))
            ),
            htmltools::div(
                class = "card-footer",
                htmltools::HTML(chat_form_html(dataset_id, lang, translations)),
                htmltools::p(
                    class = "chat-privacy text-muted small mb-0 mt-2",
                    tr(
                        "Your question and values from this dataset are sent to a remote model provider.",
                        lang,
                        translations
                    )
                )
            )
        )
    )
}

# The polled region. It re-fetches itself while a turn is in flight and stops
# (no hx-trigger) as soon as the session is idle, errored or gone.
chat_stream_html <- function(session, dataset_id, lang, translations, notice = NULL) {
    active <- chat_is_active(session)
    attrs <- list(
        id = "chat-stream",
        class = "chat-stream",
        role = "log",
        `aria-live` = "polite"
    )
    if (active) {
        attrs <- c(
            attrs,
            list(
                `hx-get` = chat_stream_url(session$id),
                `hx-trigger` = "load delay:1s",
                `hx-target` = "this",
                `hx-swap` = "outerHTML",
                `hx-sync` = "#chat-stream:drop"
            )
        )
    }
    body <- list(
        if (!is.null(notice)) chat_notice_html(notice, lang, translations),
        if (length(session$transcript %||% list()) == 0L && is.null(notice)) {
            htmltools::p(
                class = "text-muted small mb-0",
                tr("Ask a question about this dataset", lang, translations)
            )
        },
        lapply(session$transcript %||% list(), function(entry) chat_message_html(entry, lang, translations)),
        if (active) chat_pending_html(session, lang, translations),
        if (identical(session$status, "error")) {
            chat_notice_html(session$error %||% "The assistant connection failed", lang, translations)
        }
    )
    render_tags(do.call(htmltools::div, c(attrs, list(body))))
}

# The input form, never inside the polled region. `oob` re-states it after a
# send so the textarea comes back empty without any client-side scripting.
chat_form_html <- function(dataset_id, lang, translations, oob = FALSE) {
    render_tags(htmltools::tags$form(
        id = "chat-form",
        class = "chat-form d-flex gap-2",
        `hx-swap-oob` = if (oob) "outerHTML",
        `hx-post` = "/partials/chat/send",
        `hx-target` = "#chat-stream",
        `hx-swap` = "outerHTML",
        `hx-sync` = "#chat-stream:replace",
        `hx-disabled-elt` = "find button",
        htmltools::tags$input(type = "hidden", name = "dataset", value = as.integer(dataset_id)),
        htmltools::tags$label(
            class = "visually-hidden",
            `for` = "chat-message",
            tr("Ask a question about this dataset", lang, translations)
        ),
        htmltools::tags$textarea(
            id = "chat-message",
            name = "message",
            class = "form-control form-control-sm",
            rows = "2",
            maxlength = as.character(CHAT_MAX_PROMPT_CHARS),
            placeholder = tr("Ask a question about this dataset", lang, translations)
        ),
        htmltools::tags$button(
            type = "submit",
            class = "btn btn-primary btn-sm align-self-end",
            `aria-label` = tr("Send", lang, translations),
            title = tr("Send (Ctrl+Enter)", lang, translations),
            bs_icon("send")
        )
    ))
}

# The header's model indicator: an info icon whose hover title carries
# "[provider] model" ("[local]" for the platform router: llama.cpp or vllm).
# The model is only chosen when the pi session spawns (first
# question), so a plain render may leave the span empty; the send and reset
# responses re-state it OOB once the choice is made (or gone). Restored
# transcripts render without it: the model is not persisted, and the next
# question may pick a different one.
chat_model_info_html <- function(session, lang, translations, oob = FALSE) {
    provider <- session$provider %||% ""
    model <- session$model %||% ""
    if (provider %in% c("llama.cpp", "vllm")) {
        provider <- "local"
    }
    label <- sprintf("[%s] %s", provider, model)
    render_tags(htmltools::tags$span(
        id = "chat-model-info",
        class = "chat-model-info text-muted",
        `hx-swap-oob` = if (oob) "outerHTML",
        if (nzchar(provider) && nzchar(model)) {
            htmltools::tags$span(
                title = label,
                `aria-label` = paste0(tr("Model in use", lang, translations), ": ", label),
                bs_icon("info-circle")
            )
        }
    ))
}

# --- pieces --------------------------------------------------------------------

chat_message_html <- function(entry, lang, translations) {
    if (identical(entry$role, "error")) {
        return(chat_notice_html(entry$error, lang, translations))
    }
    is_user <- identical(entry$role, "user")
    body <- if (is_user) {
        htmltools::div(class = "chat-text", htmltools::HTML(chat_escape_text(entry$text)))
    } else {
        htmltools::div(class = "chat-markdown", htmltools::HTML(chat_render_markdown(entry$text %||% "")))
    }
    htmltools::div(
        class = paste0("chat-message chat-message-", if (is_user) "user" else "assistant"),
        if (!is_user) chat_thinking_html(entry$thinking, lang, translations),
        if (!is_user) chat_chips_html(entry$chips, lang, translations),
        body
    )
}

# Persisted thinking, collapsed by default. Escaped plain text only: thinking
# can quote dataset values and untrusted websearch content, so it never goes
# through the markdown renderer.
chat_thinking_html <- function(thinking, lang, translations) {
    if (is.null(thinking) || !nzchar(thinking)) {
        return(NULL)
    }
    htmltools::tags$details(
        class = "chat-thinking",
        htmltools::tags$summary(class = "text-muted small", tr("Thoughts", lang, translations)),
        htmltools::div(class = "chat-thinking-body small", htmltools::HTML(chat_escape_text(thinking)))
    )
}

# The live half of a turn: what has streamed so far (escaped, unparsed) plus the
# current activity chip.
chat_pending_html <- function(session, lang, translations) {
    text <- session$stream_text %||% ""
    htmltools::div(
        class = "chat-message chat-message-assistant chat-message-pending",
        chat_chips_html(session$chips, lang, translations),
        if (nzchar(text)) htmltools::div(class = "chat-text", htmltools::HTML(chat_escape_text(text))),
        chat_activity_html(session, lang, translations)
    )
}

chat_activity_html <- function(session, lang, translations) {
    activity <- session$activity
    label <- if (is.null(activity)) {
        tr("Thinking", lang, translations)
    } else if (identical(activity$kind, "tool")) {
        chat_tool_label(activity$tool, lang, translations)
    } else {
        tr("Thinking", lang, translations)
    }
    htmltools::div(
        class = "chat-activity text-muted small",
        htmltools::tags$span(class = "spinner-border spinner-border-sm me-2", `aria-hidden` = "true"),
        label
    )
}

chat_chips_html <- function(chips, lang, translations) {
    chips <- chips %||% list()
    if (length(chips) == 0L) {
        return(NULL)
    }
    htmltools::div(
        class = "chat-chips d-flex flex-wrap gap-1 mb-1",
        lapply(chips, function(chip) {
            badge_class <- paste0("badge chat-chip", if (isTRUE(chip$done)) " chat-chip-done" else "")
            label <- chat_tool_label(chip$tool, lang, translations)
            if (is.null(chip$args_text) || !nzchar(chip$args_text)) {
                return(htmltools::tags$span(class = badge_class, label))
            }
            # Escaped monospace only: args are model-written (SQL, search terms).
            htmltools::tags$details(
                class = "chat-chip-details",
                htmltools::tags$summary(class = badge_class, label),
                htmltools::tags$pre(
                    class = "chat-chip-args",
                    htmltools::tags$code(htmltools::HTML(chat_escape_text(chip$args_text)))
                )
            )
        })
    )
}

chat_tool_label <- function(tool, lang, translations) {
    key <- CHAT_TOOL_LABELS[[tool %||% ""]] %||% "Working"
    tr(key, lang, translations)
}

chat_notice_html <- function(key, lang, translations) {
    htmltools::div(
        class = "alert alert-warning py-2 px-3 small mb-2",
        role = "alert",
        tr(key %||% "The assistant connection failed", lang, translations)
    )
}
