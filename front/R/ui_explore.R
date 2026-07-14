# Explore page: dataset picker, shared dataset row (with actions) and the
# paginated data preview (shiny-base 200_explore parity; DT is replaced by a
# server-rendered Bootstrap table with numbered offset pagination).

PREVIEW_PAGE_SIZE <- 10L

# Everything the Explore page needs from the backend. A missing/foreign
# dataset id degrades to the no-selection state (the backend answers 404 for
# both, deliberately not distinguishing them).
gather_explore <- function(state, datastore, dataset_id) {
    datasets <- be_get(state, datastore, "/v1/datasets", query = list(limit = 100L))$items
    selected_id <- suppressWarnings(as.integer(dataset_id %||% NA))
    if (is.na(selected_id)) {
        selected_id <- NULL
    }
    detail <- NULL
    preview <- NULL
    if (!is.null(selected_id)) {
        detail <- be_maybe(be_get(state, datastore, sprintf("/v1/datasets/%d", selected_id)))
        if (!is.null(detail)) {
            preview <- be_get(
                state,
                datastore,
                sprintf("/v1/datasets/%d/data", selected_id),
                query = list(offset = 0L, limit = PREVIEW_PAGE_SIZE)
            )
        } else {
            selected_id <- NULL
        }
    }
    list(datasets = datasets, selected_id = selected_id, detail = detail, preview = preview)
}

explore_content <- function(explore, lang, translations, can_write = TRUE) {
    detail <- explore$detail
    description <- be_scalar(detail$description)
    lead <- if (is.null(detail)) {
        tr("Select a dataset to explore", lang, translations)
    } else if (!is.null(description) && nzchar(description)) {
        description
    } else {
        tr("Explore your uploaded dataset", lang, translations)
    }
    main <- if (is.null(detail)) {
        empty_state(
            "table",
            tr("No dataset selected", lang, translations),
            hint = tr("Upload a new dataset or go to Home to select one", lang, translations),
            actions = htmltools::a(
                class = "btn btn-outline-secondary",
                href = "/home",
                bs_icon("house", class = "me-1"),
                tr("Go to Home", lang, translations)
            )
        )
    } else {
        htmltools::tagList(
            htmltools::div(
                class = "mb-4",
                htmltools::HTML(dataset_row_html(detail, lang, translations, can_write, context = "explore"))
            ),
            htmltools::div(
                class = "card",
                htmltools::div(
                    class = "card-body",
                    htmltools::h5(class = "card-title", tr("Data Preview", lang, translations)),
                    htmltools::HTML(preview_html(explore$selected_id, explore$preview, lang, translations))
                )
            )
        )
    }
    render_tags(htmltools::div(
        id = "page-body",
        page_layout(
            sidebar = dataset_picker(
                explore$datasets,
                explore$selected_id,
                "/partials/explore/content",
                lang,
                translations
            ),
            main = htmltools::tagList(
                page_header(tr("Explore", lang, translations), lead),
                main
            )
        )
    ))
}

# The preview table + numbered pagination, swapped as one #preview region.
preview_html <- function(dataset_id, preview, lang, translations) {
    columns <- unlist(preview$columns, use.names = FALSE)
    rows <- preview$rows
    n_rows <- as.integer(preview$n_rows %||% 0L)
    offset <- as.integer(preview$offset %||% 0L)
    shown <- length(rows)

    fmt_cell <- function(x) {
        x <- be_scalar(x)
        if (is.null(x)) {
            ""
        } else if (is.numeric(x)) {
            fmt_metric(x)
        } else {
            as.character(x)
        }
    }
    body_rows <- lapply(rows, function(row) {
        htmltools::tags$tr(lapply(columns, function(col) htmltools::tags$td(fmt_cell(row[[col]]))))
    })

    render_tags(htmltools::div(
        id = "preview",
        data_table(
            headers = columns,
            rows = body_rows,
            class = "table table-sm table-striped table-hover align-middle"
        ),
        htmltools::div(
            class = "d-flex flex-wrap align-items-center justify-content-between gap-2",
            htmltools::tags$span(
                class = "text-muted small",
                sprintf("%d-%d / %s", if (shown == 0) 0L else offset + 1L, offset + shown, fmt_count(n_rows))
            ),
            pagination_html(dataset_id, offset, n_rows, lang, translations)
        )
    ))
}

# Numbered pagination (shiny-base DT parity): Previous | windowed page numbers
# with ellipsis gaps | Next. Every live button re-fetches the #preview region;
# hx-sync on #preview aborts a stale in-flight page when clicks come fast.
pagination_html <- function(dataset_id, offset, n_rows, lang, translations) {
    total_pages <- max(1L, as.integer(ceiling(n_rows / PREVIEW_PAGE_SIZE)))
    current <- min(total_pages, offset %/% PREVIEW_PAGE_SIZE + 1L)
    item <- function(label, page, disabled = FALSE, active = FALSE) {
        htmltools::tags$li(
            class = paste(c("page-item", if (disabled) "disabled", if (active) "active"), collapse = " "),
            `aria-current` = if (active) "page",
            if (disabled) {
                htmltools::tags$span(class = "page-link", label)
            } else {
                htmltools::tags$button(
                    type = "button",
                    class = "page-link",
                    `hx-get` = "/partials/explore/preview",
                    `hx-vals` = sprintf(
                        '{"dataset": %d, "offset": %d}',
                        as.integer(dataset_id),
                        (page - 1L) * PREVIEW_PAGE_SIZE
                    ),
                    `hx-target` = "#preview",
                    `hx-swap` = "outerHTML",
                    `hx-sync` = "#preview:replace",
                    label
                )
            }
        )
    }
    ellipsis <- htmltools::tags$li(
        class = "page-item disabled",
        htmltools::tags$span(class = "page-link", "…")
    )
    items <- list(item(tr("Previous", lang, translations), current - 1L, disabled = current == 1L))
    previous_page <- 0L
    for (page in pagination_pages(current, total_pages)) {
        if (page > previous_page + 1L) {
            items <- c(items, list(ellipsis))
        }
        items <- c(items, list(item(as.character(page), page, active = page == current)))
        previous_page <- page
    }
    items <- c(items, list(item(tr("Next", lang, translations), current + 1L, disabled = current == total_pages)))
    htmltools::tags$nav(
        `aria-label` = tr("Data preview pages", lang, translations),
        htmltools::tags$ul(class = "pagination pagination-sm mb-0", items)
    )
}

# Page numbers worth showing: first, last, and a window around the current page.
pagination_pages <- function(current, total_pages, window = 1L) {
    pages <- c(1L, seq(max(1L, current - window), min(total_pages, current + window)), total_pages)
    unique(pages[pages >= 1L & pages <= total_pages])
}
