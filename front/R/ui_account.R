# Account page: self-service API key management against the backend's
# JWT-only /v1/keys endpoints. The created secret is rendered ONCE, in the
# response to the create action, with a copy-to-clipboard button (app.js).

# Scopes a key may carry, mirrored from the backend's key-safe allowlist
# (permissions.yaml): requested scopes are bounded server-side regardless.
KEY_SCOPE_CHOICES <- c("write:datasets", "write:models")

account_content <- function(keys, lang, translations) {
    render_tags(htmltools::div(
        id = "page-body",
        page_header(
            tr("Account", lang, translations),
            tr("Manage the API keys that grant direct access to the backend API.", lang, translations)
        ),
        htmltools::div(
            class = "card mb-4",
            htmltools::div(
                class = "card-body",
                htmltools::h5(class = "card-title", tr("Create API Key", lang, translations)),
                create_key_form(lang, translations),
                htmltools::div(id = "key-result", class = "mt-3")
            )
        ),
        htmltools::div(
            class = "card",
            htmltools::div(
                class = "card-body",
                htmltools::h5(class = "card-title", tr("API Keys", lang, translations)),
                htmltools::HTML(keys_table_html(keys, lang, translations))
            )
        )
    ))
}

create_key_form <- function(lang, translations) {
    scope_checkbox <- function(scope) {
        input_id <- paste0("key-scope-", gsub("[^a-z]", "-", scope))
        htmltools::div(
            class = "form-check form-check-inline",
            htmltools::tags$input(
                type = "checkbox",
                class = "form-check-input",
                id = input_id,
                name = "scopes",
                value = scope,
                checked = NA
            ),
            htmltools::tags$label(class = "form-check-label", `for` = input_id, htmltools::tags$code(scope))
        )
    }
    htmltools::tags$form(
        id = "create-key-form",
        `hx-post` = "/keys",
        `hx-target` = "#key-result",
        `hx-swap` = "innerHTML",
        htmltools::div(
            class = "row g-3 align-items-end",
            htmltools::div(
                class = "col-md-4",
                htmltools::tags$label(class = "form-label", `for` = "key-name", tr("Key name", lang, translations)),
                htmltools::tags$input(
                    type = "text",
                    class = "form-control",
                    id = "key-name",
                    name = "name",
                    required = NA
                )
            ),
            htmltools::div(
                class = "col-md-4",
                htmltools::tags$label(class = "form-label d-block", tr("Scopes", lang, translations)),
                lapply(KEY_SCOPE_CHOICES, scope_checkbox)
            ),
            htmltools::div(
                class = "col-md-2",
                htmltools::tags$label(
                    class = "form-label",
                    `for` = "key-expires",
                    tr("Expires (optional)", lang, translations)
                ),
                htmltools::tags$input(type = "date", class = "form-control", id = "key-expires", name = "expires_at")
            ),
            htmltools::div(
                class = "col-md-2",
                htmltools::tags$button(
                    type = "submit",
                    class = "btn btn-primary w-100",
                    tr("Create", lang, translations)
                )
            )
        )
    )
}

# The one-time secret panel returned by a successful create. Uses a calm
# success-tinted panel with the secret in a readable inset (not flatly's bright
# alert-success, whose pink <code> on teal is illegible).
key_created_html <- function(created, lang, translations) {
    render_tags(htmltools::div(
        class = "key-created",
        role = "alert",
        htmltools::div(
            class = "key-created-title",
            bs_icon("check-circle-fill"),
            tr("This key is shown only once. Copy it now.", lang, translations)
        ),
        htmltools::div(
            class = "key-created-secret",
            htmltools::tags$code(class = "key-created-code", created$secret),
            htmltools::tags$button(
                type = "button",
                class = "btn btn-sm btn-outline-secondary flex-shrink-0",
                `data-clipboard-text` = created$secret,
                `data-clipboard-message` = tr("Copied to clipboard", lang, translations),
                bs_icon("clipboard", class = "me-1"),
                tr("Copy", lang, translations)
            )
        )
    ))
}

keys_table_html <- function(keys, lang, translations, oob = FALSE) {
    content <- if (length(keys) == 0) {
        htmltools::p(class = "text-muted mb-0", tr("No API keys yet.", lang, translations))
    } else {
        never <- tr("Never", lang, translations)
        date_or_never <- function(iso) {
            value <- fmt_date(iso)
            if (nzchar(value)) value else never
        }
        rows <- lapply(keys, function(k) {
            revoked <- isTRUE(k$revoked)
            htmltools::tags$tr(
                class = if (revoked) "text-muted",
                htmltools::tags$td(k$name),
                htmltools::tags$td(htmltools::tags$code(paste0(k$key_prefix, "..."))),
                htmltools::tags$td(paste(unlist(k$scopes, use.names = FALSE), collapse = ", ")),
                htmltools::tags$td(fmt_date(k$created_at)),
                htmltools::tags$td(date_or_never(k$last_used_at)),
                htmltools::tags$td(date_or_never(k$expires_at)),
                htmltools::tags$td(
                    if (revoked) {
                        htmltools::tags$span(class = "badge text-bg-secondary", tr("Revoked", lang, translations))
                    } else {
                        htmltools::tags$button(
                            type = "button",
                            class = "btn btn-sm btn-outline-danger",
                            `hx-delete` = sprintf("/keys/%d", as.integer(k$id)),
                            `hx-confirm` = tr("Are you sure you want to revoke this key?", lang, translations),
                            `hx-swap` = "none",
                            tr("Revoke", lang, translations)
                        )
                    }
                )
            )
        })
        data_table(
            headers = c(
                tr("Name", lang, translations),
                "Prefix",
                tr("Scopes", lang, translations),
                tr("Created", lang, translations),
                tr("Last used", lang, translations),
                tr("Expires", lang, translations),
                ""
            ),
            rows = rows,
            class = "table table-sm align-middle"
        )
    }
    render_tags(htmltools::div(
        id = "keys-table",
        `hx-swap-oob` = if (oob) "true",
        content
    ))
}
