# Shared UI building blocks for the feature pages. Everything returns htmltools
# tags (collapsed to bare strings by the callers via render_tags) and is pure -
# data in, markup out - so the page content is snapshot-testable without a
# server. CSP rules: no inline scripts, no hx-on; all behaviour comes from
# htmx attributes and the delegated listeners in assets/js/app.js.

bs_icon <- function(name, class = NULL) {
    htmltools::tags$i(class = paste(c(paste0("bi bi-", name), class), collapse = " "), `aria-hidden` = "true")
}

# Two-column page body: contextual sidebar (filters / dataset picker / saved
# models) + main column, the layout shiny-base built with bslib::sidebar.
page_layout <- function(sidebar, main) {
    htmltools::div(
        class = "row g-4",
        htmltools::tags$aside(class = "col-md-4 col-lg-3 col-xl-2", sidebar),
        htmltools::div(class = "col-md-8 col-lg-9 col-xl-10", main)
    )
}

page_header <- function(title, lead = NULL) {
    htmltools::div(
        class = "page-header mb-4",
        htmltools::h1(title),
        if (!is.null(lead)) htmltools::p(class = "lead", lead)
    )
}

sidebar_section <- function(title, ...) {
    htmltools::div(
        class = "mb-4",
        htmltools::h6(class = "text-uppercase text-muted fw-semibold mb-3", title),
        ...
    )
}

empty_state <- function(icon, message, hint = NULL, actions = NULL) {
    htmltools::div(
        class = "empty-state text-center text-muted py-5",
        bs_icon(icon, class = "fs-1 d-block mb-3"),
        htmltools::p(message),
        if (!is.null(hint)) htmltools::p(htmltools::tags$small(class = "text-muted", hint)),
        if (!is.null(actions)) htmltools::div(class = "d-flex gap-2 justify-content-center", actions)
    )
}

# Bootstrap modal shell for server-rendered modal content swapped into
# #modal-slot (app.js shows it on htmx:afterSwap and empties the slot on
# hidden.bs.modal).
modal_html <- function(id, title, body, footer = NULL) {
    htmltools::div(
        class = "modal fade",
        id = id,
        tabindex = "-1",
        `aria-hidden` = "true",
        htmltools::div(
            class = "modal-dialog",
            htmltools::div(
                class = "modal-content",
                htmltools::div(
                    class = "modal-header",
                    htmltools::h5(class = "modal-title", title),
                    htmltools::tags$button(
                        type = "button",
                        class = "btn-close",
                        `data-bs-dismiss` = "modal",
                        `aria-label` = "Close"
                    )
                ),
                htmltools::div(class = "modal-body", body),
                if (!is.null(footer)) htmltools::div(class = "modal-footer", footer)
            )
        )
    )
}

# Bare <table class="table"> with a header row; rows is a list of <tr> tags.
data_table <- function(headers, rows, class = "table table-hover align-middle") {
    htmltools::div(
        class = "table-responsive",
        htmltools::tags$table(
            class = class,
            htmltools::tags$thead(htmltools::tags$tr(lapply(headers, htmltools::tags$th))),
            htmltools::tags$tbody(rows)
        )
    )
}

# Dataset picker shared by Explore and Model: changing the selection fetches
# the page's content partial (which swaps #page-body and pushes the canonical
# page URL via HX-Push-Url). htmx sends the select's own name=value pair.
dataset_picker <- function(datasets, selected_id, partial_url, lang, translations) {
    options <- c(
        list(htmltools::tags$option(value = "", tr("Select Dataset", lang, translations))),
        lapply(datasets, function(ds) {
            id <- as.integer(ds$id)
            htmltools::tags$option(
                value = id,
                selected = if (identical(id, selected_id)) NA,
                ds$name
            )
        })
    )
    sidebar_section(
        tr("Dataset", lang, translations),
        htmltools::tags$label(
            class = "form-label visually-hidden",
            `for` = "dataset-select",
            tr("Select Dataset", lang, translations)
        ),
        htmltools::tags$select(
            class = "form-select form-select-sm",
            id = "dataset-select",
            name = "dataset",
            `hx-get` = partial_url,
            `hx-target` = "#page-body",
            `hx-swap` = "outerHTML",
            `hx-trigger` = "change",
            # A dataset switch aborts any in-flight request synced on #page-body
            # (fit submits and job polls), so a stale response can never OOB-swap
            # fragments into the next dataset's page.
            `hx-sync` = "#page-body:replace",
            options
        )
    )
}

# A backend fetch where 404 means "render the empty state" rather than an error.
be_maybe <- function(expr) {
    tryCatch(
        expr,
        fe_backend_error = function(e) {
            if (e$status == 404L) NULL else stop(e)
        }
    )
}

# --- value formatting ----------------------------------------------------------

# Normalize any scalar-ish backend value to a length-1 vector, or NULL. JSON
# null arrives as NULL (yyjsonr on both sides), but jsonb-sourced values can
# still surface as empty lists ({} / []), so both shapes are handled.
be_scalar <- function(x) {
    if (is.list(x)) {
        x <- unlist(x, use.names = FALSE)
    }
    if (is.null(x) || length(x) == 0 || is.na(x[1])) NULL else x[1]
}

fmt_count <- function(n) {
    format(as.integer(be_scalar(n) %||% 0), big.mark = ",")
}

fmt_dims <- function(n_rows, n_cols) {
    sprintf("%s rows × %s cols", fmt_count(n_rows), fmt_count(n_cols))
}

# ISO 8601 timestamp -> date portion (what shiny-base displayed).
fmt_date <- function(iso) {
    iso <- be_scalar(iso)
    if (is.null(iso) || !nzchar(iso)) {
        return("")
    }
    substr(as.character(iso), 1, 10)
}

fmt_metric <- function(x) {
    x <- be_scalar(x)
    if (is.null(x) || !is.numeric(x)) {
        return("NA")
    }
    format(signif(x, 4))
}
