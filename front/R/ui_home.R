# Home page: dataset stat card + dataset list with sidebar filters, upload
# modal, inline rename, delete and download (shiny-base 100_home / 002_sidebar /
# 003_dataset_row / 004_upload / 005_edit parity).
#
# Refresh flow: #home-data re-fetches itself (keeping the current sidebar
# filters via hx-include) whenever the body receives fb:refresh-datasets, which
# action responses raise through the HX-Trigger header.

# Row-count slider ceiling; mirrors the backend upload cap (back/R/config.R
# max_dataset_rows). Fixed rather than data-derived: /v1/datasets is filtered
# and capped at 100 items, so the response cannot provide a global max.
HOME_MAX_ROWS <- 50000L

# Normalized filter state from the query params (empty strings dropped).
# min_rows has no UI control (single-thumb slider, accepted divergence from
# shiny's dual slider): stale min_rows in incoming URLs is ignored, though the
# backend still supports the param for API clients.
parse_home_filters <- function(query) {
    clean <- function(x) {
        if (is.null(x) || is.na(x) || !nzchar(as.character(x))) NULL else as.character(x)
    }
    max_rows <- clean(query$max_rows)
    if (!is.null(max_rows) && !isTRUE(suppressWarnings(as.integer(max_rows)) < HOME_MAX_ROWS)) {
        # Slider at (or past) the ceiling excludes nothing: treat as no filter.
        max_rows <- NULL
    }
    list(
        max_rows = max_rows,
        created_from = clean(query$created_from),
        created_to = clean(query$created_to)
    )
}

# Row flavour carried by the shared dataset partial routes' ?context= param.
dataset_row_context <- function(query) {
    if (identical(query$context, "explore")) "explore" else "home"
}

# Filters -> backend /v1/datasets query. A bare "to" date is made inclusive of
# that whole day (shiny-base compared on dates, the backend on timestamps).
home_filters_be_query <- function(filters) {
    query <- filters
    if (!is.null(query$created_to) && grepl("^\\d{4}-\\d{2}-\\d{2}$", query$created_to)) {
        query$created_to <- paste0(query$created_to, "T23:59:59")
    }
    query
}

# Canonical page URL for the current filter state (HX-Push-Url).
home_filters_url <- function(filters) {
    filters <- filters[!vapply(filters, is.null, logical(1))]
    if (length(filters) == 0) {
        return("/home")
    }
    pairs <- vapply(
        names(filters),
        function(name) paste0(name, "=", utils::URLencode(filters[[name]], reserved = TRUE)),
        character(1)
    )
    paste0("/home?", paste(pairs, collapse = "&"))
}

home_content <- function(datasets, filters, lang, translations, can_write = TRUE) {
    render_tags(
        page_layout(
            sidebar = home_filters_form(filters, lang, translations),
            main = htmltools::tagList(
                page_header(
                    tr("Home", lang, translations),
                    tr("Your uploaded datasets.", lang, translations)
                ),
                htmltools::HTML(home_data_panel(datasets, lang, translations, can_write))
            )
        ),
        if (can_write) upload_modal(lang, translations)
    )
}

# Collapsible filter sidebar (shiny-base sidebar parity): a chevron toggle in
# the section title collapses the form body (pure Bootstrap Collapse, no JS of
# ours), and the row-count filter is a native single-thumb range slider whose
# live value lands in the <output> via the delegated listener in app.js.
home_filters_form <- function(filters, lang, translations) {
    date_input <- function(name, label, value) {
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(class = "form-label", `for` = paste0("filter-", name), label),
            htmltools::tags$input(
                type = "date",
                class = "form-control form-control-sm",
                id = paste0("filter-", name),
                name = name,
                value = value
            )
        )
    }
    max_rows <- suppressWarnings(as.integer(filters$max_rows %||% NA))
    if (is.na(max_rows) || max_rows < 0L || max_rows > HOME_MAX_ROWS) {
        max_rows <- HOME_MAX_ROWS
    }
    sidebar_section(
        htmltools::tags$button(
            type = "button",
            class = "filters-toggle",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#home-filters-body",
            `aria-expanded` = "true",
            `aria-controls` = "home-filters-body",
            tr("Filters", lang, translations),
            bs_icon("chevron-down", class = "filters-toggle-icon")
        ),
        htmltools::div(
            id = "home-filters-body",
            class = "collapse show",
            htmltools::tags$form(
                id = "home-filters",
                `hx-get` = "/partials/home/datasets",
                `hx-target` = "#home-data",
                `hx-swap` = "outerHTML",
                `hx-trigger` = "change delay:300ms, input changed delay:300ms from:#filter-max_rows",
                htmltools::tags$fieldset(
                    class = "mb-2",
                    htmltools::tags$legend(class = "form-label fs-6", tr("Filter by row count", lang, translations)),
                    htmltools::div(
                        class = "mb-3",
                        htmltools::tags$label(
                            class = "form-label d-flex justify-content-between align-items-baseline",
                            `for` = "filter-max_rows",
                            htmltools::tags$span(tr("Max rows", lang, translations)),
                            htmltools::tags$output(
                                id = "filter-max_rows-value",
                                class = "text-muted small",
                                `for` = "filter-max_rows",
                                max_rows
                            )
                        ),
                        htmltools::tags$input(
                            type = "range",
                            class = "form-range live-slider",
                            id = "filter-max_rows",
                            name = "max_rows",
                            min = "0",
                            max = as.character(HOME_MAX_ROWS),
                            step = "100",
                            value = max_rows,
                            `data-value-target` = "filter-max_rows-value"
                        )
                    )
                ),
                htmltools::tags$fieldset(
                    htmltools::tags$legend(class = "form-label fs-6", tr("Filter by date", lang, translations)),
                    date_input("created_from", tr("From", lang, translations), filters$created_from),
                    date_input("created_to", tr("To", lang, translations), filters$created_to)
                )
            )
        )
    )
}

# The refreshable region: stat card + dataset list card. Swapped wholesale
# (outerHTML) by the sidebar filters and the fb:refresh-datasets event.
home_data_panel <- function(datasets, lang, translations, can_write = TRUE) {
    n <- length(datasets)
    render_tags(htmltools::div(
        id = "home-data",
        `hx-get` = "/partials/home/datasets",
        `hx-trigger` = "fb:refresh-datasets from:body",
        `hx-include` = "#home-filters",
        `hx-swap` = "outerHTML",
        htmltools::div(
            class = "card stat-card mb-4",
            htmltools::div(
                class = "card-body d-flex align-items-center gap-3",
                bs_icon("database", class = "fs-2 text-primary"),
                htmltools::div(
                    htmltools::tags$span(class = "display-6 d-block", id = "dataset-count", fmt_count(n)),
                    htmltools::tags$span(class = "text-muted", tr("Datasets", lang, translations))
                )
            )
        ),
        htmltools::div(
            class = "card datasets-card",
            htmltools::div(
                class = "card-header d-flex justify-content-between align-items-center",
                htmltools::h3(class = "h5 mb-0", tr("Your Datasets", lang, translations)),
                if (can_write) {
                    htmltools::tags$button(
                        type = "button",
                        class = "btn btn-primary btn-sm",
                        `data-bs-toggle` = "modal",
                        `data-bs-target` = "#upload-modal",
                        bs_icon("upload", class = "me-1"),
                        tr("Upload Dataset", lang, translations)
                    )
                }
            ),
            htmltools::div(
                class = "card-body",
                if (n == 0) {
                    empty_state(
                        "folder2-open",
                        tr("No datasets match the current filter", lang, translations)
                    )
                } else {
                    htmltools::tagList(lapply(datasets, function(ds) {
                        htmltools::HTML(dataset_row_html(ds, lang, translations, can_write))
                    }))
                }
            )
        )
    ))
}

# One dataset row shared by Home and Explore (shiny-base dataset_row_ui parity):
# name | age | size grid columns + the edit / download / delete actions.
# context picks the flavour: "home" (name links to Explore, actions stay on the
# list via fb:refresh-datasets) or "explore" (static name; rename/delete answer
# with an HX-Redirect back to /explore so the sidebar picker stays in sync).
dataset_row_html <- function(ds, lang, translations, can_write = TRUE, context = c("home", "explore")) {
    context <- match.arg(context)
    id <- as.integer(ds$id)
    main_content <- htmltools::tagList(
        htmltools::tags$span(
            class = "dataset-col dataset-col-name",
            htmltools::tags$span(class = "dataset-name", ds$name)
        ),
        htmltools::tags$span(
            class = "dataset-col dataset-col-age",
            bs_icon("calendar-plus"),
            htmltools::tags$span(fmt_date(ds$created_at))
        ),
        htmltools::tags$span(
            class = "dataset-col dataset-col-size",
            bs_icon("table"),
            htmltools::tags$span(fmt_dims(ds$n_rows, ds$n_cols))
        )
    )
    row_body <- if (identical(context, "home")) {
        htmltools::a(
            class = "dataset-row-link clickable",
            href = sprintf("/explore?dataset=%d", id),
            main_content
        )
    } else {
        htmltools::div(class = "dataset-row-link", main_content)
    }
    render_tags(htmltools::div(
        class = "dataset-row",
        id = sprintf("dataset-row-%d", id),
        row_body,
        htmltools::div(
            class = "dataset-col dataset-col-actions",
            if (can_write) {
                htmltools::tags$button(
                    type = "button",
                    class = "btn btn-sm btn-outline-secondary btn-action-dataset",
                    title = tr("Rename Dataset", lang, translations),
                    `hx-get` = sprintf("/partials/dataset/%d/edit?context=%s", id, context),
                    `hx-target` = sprintf("#dataset-row-%d", id),
                    `hx-swap` = "outerHTML",
                    bs_icon("pencil")
                )
            },
            htmltools::a(
                class = "btn btn-sm btn-outline-primary btn-action-dataset",
                title = tr("Download", lang, translations),
                href = sprintf("/datasets/%d/download", id),
                bs_icon("download")
            ),
            if (can_write) {
                htmltools::tags$button(
                    type = "button",
                    class = "btn btn-sm btn-outline-danger btn-action-dataset",
                    title = tr("Delete", lang, translations),
                    `hx-delete` = paste0(
                        sprintf("/datasets/%d", id),
                        if (identical(context, "explore")) "?context=explore" else ""
                    ),
                    `hx-confirm` = tr("Are you sure you want to delete this dataset?", lang, translations),
                    `hx-swap` = "none",
                    bs_icon("trash")
                )
            }
        )
    ))
}

# Inline rename form that swaps in place of the row. context follows the row's
# flavour (see dataset_row_html) so rename/cancel answer for the right page.
dataset_row_edit_html <- function(ds, lang, translations, error = NULL, context = c("home", "explore")) {
    context <- match.arg(context)
    id <- as.integer(ds$id)
    render_tags(htmltools::tags$form(
        class = "dataset-row d-flex align-items-center gap-2",
        id = sprintf("dataset-row-%d", id),
        `hx-patch` = paste0(
            sprintf("/datasets/%d", id),
            if (identical(context, "explore")) "?context=explore" else ""
        ),
        `hx-target` = "this",
        `hx-swap` = "outerHTML",
        htmltools::div(
            class = "flex-grow-1",
            htmltools::tags$input(
                type = "text",
                class = paste(c("form-control form-control-sm", if (!is.null(error)) "is-invalid"), collapse = " "),
                name = "name",
                id = sprintf("dataset-name-%d", id),
                value = ds$name,
                placeholder = tr("Enter new dataset name", lang, translations),
                `aria-label` = tr("New Name", lang, translations)
            ),
            if (!is.null(error)) htmltools::div(class = "invalid-feedback", error)
        ),
        htmltools::tags$button(
            type = "submit",
            class = "btn btn-sm btn-primary",
            tr("Rename", lang, translations)
        ),
        htmltools::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            `hx-get` = sprintf("/partials/dataset/%d/row?context=%s", id, context),
            `hx-target` = sprintf("#dataset-row-%d", id),
            `hx-swap` = "outerHTML",
            tr("Cancel", lang, translations)
        )
    ))
}

upload_modal <- function(lang, translations) {
    body <- htmltools::tagList(
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(class = "form-label", `for` = "upload-file", tr("CSV File(s)", lang, translations)),
            # Dropzone: a dashed target wrapping a visually-hidden file input.
            # Clicking anywhere opens the picker; drag&drop is wired in app.js.
            htmltools::tags$label(
                class = "file-dropzone",
                htmltools::tags$input(
                    type = "file",
                    class = "file-dropzone-input",
                    id = "upload-file",
                    name = "file",
                    accept = ".csv,text/csv",
                    required = NA
                ),
                bs_icon("cloud-arrow-up", class = "file-dropzone-icon"),
                htmltools::tags$span(
                    class = "file-dropzone-text",
                    tr("Drag & drop, or click to browse", lang, translations)
                ),
                htmltools::tags$span(
                    class = "file-dropzone-hint",
                    tr("CSV only, max 10MB per file", lang, translations)
                ),
                htmltools::tags$span(class = "file-dropzone-file", id = "upload-file-name")
            )
        ),
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(
                class = "form-label",
                `for` = "upload-name",
                tr("Name (optional)", lang, translations)
            ),
            htmltools::tags$input(type = "text", class = "form-control", id = "upload-name", name = "name")
        ),
        htmltools::div(
            class = "mb-3",
            htmltools::tags$label(
                class = "form-label",
                `for` = "upload-description",
                tr("Description (optional)", lang, translations)
            ),
            htmltools::tags$textarea(
                class = "form-control",
                id = "upload-description",
                name = "description",
                rows = "2"
            )
        ),
        htmltools::div(id = "upload-status")
    )
    modal_html(
        id = "upload-modal",
        title = tr("Upload Dataset", lang, translations),
        body = htmltools::tags$form(
            id = "upload-form",
            `hx-post` = "/datasets/upload",
            `hx-encoding` = "multipart/form-data",
            `hx-target` = "#upload-status",
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
                htmltools::tags$button(
                    type = "submit",
                    class = "btn btn-primary",
                    bs_icon("upload", class = "me-1"),
                    tr("Upload", lang, translations)
                )
            )
        )
    )
}
