# Profile modal (shiny-base 006_profile_modal parity): read-only email and
# roles, editable nickname (persisted to Auth0 via the Management API and
# mirrored locally) and preferred interface language (the lang cookie).
# Server-rendered into #modal-slot; app.js shows/disposes the Bootstrap modal.

profile_modal_content <- function(auth, lang, translations) {
    field <- function(label, value) {
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(class = "form-label", label),
            htmltools::div(class = "form-control-plaintext", value)
        )
    }
    language_options <- list(
        htmltools::tags$option(
            value = "en",
            selected = if (identical(lang, "en")) NA,
            tr("English", lang, translations)
        ),
        htmltools::tags$option(
            value = "fr",
            selected = if (identical(lang, "fr")) NA,
            tr("French", lang, translations)
        )
    )
    body <- htmltools::tagList(
        if (!is.null(auth$picture) && nzchar(auth$picture %||% "")) {
            htmltools::div(
                class = "text-center mb-3",
                htmltools::tags$img(
                    src = auth$picture,
                    class = "rounded-circle",
                    width = "96",
                    height = "96",
                    alt = "Profile picture"
                )
            )
        },
        field(tr("Email", lang, translations), auth$email %||% ""),
        field(
            tr("Roles", lang, translations),
            if (length(auth$roles)) {
                paste(unlist(auth$roles, use.names = FALSE), collapse = ", ")
            } else {
                tr("None", lang, translations)
            }
        ),
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(
                class = "form-label",
                `for` = "profile-nickname",
                tr("Nickname", lang, translations)
            ),
            htmltools::tags$input(
                type = "text",
                class = "form-control",
                id = "profile-nickname",
                name = "nickname",
                value = auth$nickname %||% ""
            )
        ),
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(
                class = "form-label",
                `for` = "profile-language",
                tr("Preferred Language", lang, translations)
            ),
            htmltools::tags$select(
                class = "form-select",
                id = "profile-language",
                name = "language",
                language_options
            )
        ),
        htmltools::div(id = "profile-status")
    )
    render_tags(modal_html(
        id = "profile-modal",
        title = tr("Profile", lang, translations),
        body = htmltools::tags$form(
            id = "profile-form",
            `hx-post` = "/profile",
            `hx-target` = "#profile-status",
            `hx-swap` = "innerHTML",
            body,
            htmltools::div(
                class = "d-flex justify-content-end gap-2",
                htmltools::tags$button(
                    type = "button",
                    class = "btn btn-outline-secondary",
                    `data-bs-dismiss` = "modal",
                    tr("Cancel", lang, translations)
                ),
                htmltools::tags$button(type = "submit", class = "btn btn-primary", tr("Save", lang, translations))
            )
        )
    ))
}

# Out-of-band refresh of the navbar identity after a nickname change.
navbar_name_oob <- function(nickname) {
    render_tags(htmltools::tags$span(id = "navbar-user-name", `hx-swap-oob` = "true", nickname))
}
