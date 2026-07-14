# Model page: formula input + async fit through the backend's 202-and-poll job
# flow (self-replacing htmx "load polling" fragments), metrics + summary
# rendering, and the saved-models sidebar (shiny-base 300_model parity; fit
# persists the model server-side, so there is no separate Save action).

# Everything the Model page needs from the backend. The active model (the
# `model` query param, kept canonical in the URL via HX-Push-Url) degrades to
# NULL when stale: deleted/foreign ids 404 on the backend, and an id belonging
# to another dataset is dropped rather than trusted.
gather_model <- function(state, datastore, dataset_id, model_id = NULL) {
    datasets <- be_get(state, datastore, "/v1/datasets", query = list(limit = 100L))$items
    selected_id <- suppressWarnings(as.integer(dataset_id %||% NA))
    if (is.na(selected_id)) {
        selected_id <- NULL
    }
    detail <- NULL
    models <- list()
    active_model <- NULL
    if (!is.null(selected_id)) {
        detail <- be_maybe(be_get(state, datastore, sprintf("/v1/datasets/%d", selected_id)))
        if (!is.null(detail)) {
            models <- be_get(state, datastore, "/v1/models", query = list(dataset_id = selected_id))$items
            active_id <- suppressWarnings(as.integer(model_id %||% NA))
            if (!is.na(active_id)) {
                active_model <- be_maybe(be_get(state, datastore, sprintf("/v1/models/%d", active_id)))
                if (!is.null(active_model) && !identical(as.integer(active_model$dataset_id), selected_id)) {
                    active_model <- NULL
                }
            }
        } else {
            selected_id <- NULL
        }
    }
    list(
        datasets = datasets,
        selected_id = selected_id,
        detail = detail,
        models = models,
        active_model = active_model
    )
}

model_content <- function(model, lang, translations, can_write = TRUE) {
    detail <- model$detail
    main <- if (is.null(detail)) {
        empty_state(
            "graph-up",
            tr("No dataset selected", lang, translations),
            hint = tr("Select a dataset from the sidebar to start modeling", lang, translations)
        )
    } else {
        htmltools::tagList(
            model_fit_card(model, lang, translations, can_write = can_write),
            htmltools::div(
                id = "fit-status",
                class = "mt-4",
                if (!is.null(model$active_model)) {
                    htmltools::HTML(model_result_fragment(model$active_model, lang, translations))
                }
            )
        )
    }
    render_tags(htmltools::div(
        id = "page-body",
        page_layout(
            sidebar = htmltools::tagList(
                dataset_picker(model$datasets, model$selected_id, "/partials/model/content", lang, translations),
                if (!is.null(detail)) {
                    htmltools::HTML(saved_models_html(
                        model$models,
                        model$selected_id,
                        lang,
                        translations,
                        active_model_id = model$active_model$id
                    ))
                }
            ),
            main = htmltools::tagList(
                page_header(
                    tr("Model", lang, translations),
                    tr("Fit linear models to your data", lang, translations)
                ),
                main
            )
        )
    ))
}

# Equation card (shiny-base parity): card-header carries the title and the
# Fit/Delete toolbar, card-body the formula input + hint + variable badges.
# The Fit button submits #fit-form from the header via the form attribute; the
# toolbar and the formula input have stable ids so fit/load/delete responses
# can re-state them out-of-band as one coherent set.
model_fit_card <- function(model, lang, translations, can_write = TRUE) {
    columns <- names(model$detail$summary)
    active <- model$active_model
    htmltools::div(
        class = "card model-equation-card",
        htmltools::div(
            class = "card-header d-flex justify-content-between align-items-center gap-2",
            htmltools::h5(class = "card-title mb-0", tr("Model Equation", lang, translations)),
            htmltools::HTML(model_toolbar_html(
                model$selected_id,
                lang,
                translations,
                active_model_id = active$id,
                can_write = can_write
            ))
        ),
        htmltools::div(
            class = "card-body",
            htmltools::tags$form(
                id = "fit-form",
                `hx-post` = "/models/fit",
                `hx-target` = "#fit-status",
                `hx-swap` = "innerHTML",
                `hx-sync` = "#page-body:drop",
                htmltools::tags$input(type = "hidden", name = "dataset", value = model$selected_id),
                htmltools::tags$input(
                    type = "hidden",
                    name = "model",
                    value = if (!is.null(active)) as.integer(active$id) else ""
                ),
                htmltools::tags$input(
                    type = "text",
                    class = "form-control",
                    id = "formula-input",
                    name = "formula",
                    value = if (!is.null(active)) active$formula,
                    placeholder = "y ~ x1 + x2",
                    `aria-label` = tr("Model Equation", lang, translations)
                ),
                htmltools::tags$small(
                    class = "text-muted d-block mt-2",
                    tr("Enter an R formula (e.g., y ~ x1 + x2, y ~ poly(x, 2))", lang, translations)
                ),
                htmltools::tags$small(
                    class = "text-muted d-block mt-1",
                    tr("Available variables:", lang, translations),
                    lapply(columns, function(col) {
                        htmltools::tags$span(class = "badge text-bg-light ms-1", col)
                    })
                )
            )
        )
    )
}

# The Fit/Delete toolbar living in the equation card header. Delete targets the
# ACTIVE model only (per-row deletes stay in the sidebar picker); both buttons
# are disabled while a fit job is polling. Swapped out-of-band by every
# response that changes the active model or the fitting state.
model_toolbar_html <- function(
    dataset_id,
    lang,
    translations,
    active_model_id = NULL,
    can_write = TRUE,
    fitting = FALSE,
    oob = FALSE
) {
    active_id <- suppressWarnings(as.integer(active_model_id %||% NA))
    if (is.na(active_id)) {
        active_id <- NULL
    }
    can_delete <- can_write && !fitting && !is.null(active_id)
    render_tags(htmltools::div(
        id = "model-toolbar",
        class = "d-flex align-items-center gap-2",
        `hx-swap-oob` = if (oob) "true",
        htmltools::tags$button(
            type = "submit",
            form = "fit-form",
            class = "btn btn-sm btn-primary",
            disabled = if (!can_write || fitting) NA,
            bs_icon("play-fill", class = "me-1"),
            tr("Fit", lang, translations)
        ),
        htmltools::tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-danger",
            disabled = if (!can_delete) NA,
            title = tr("Delete model", lang, translations),
            `hx-delete` = if (can_delete) sprintf("/models/%d", active_id),
            `hx-vals` = if (can_delete) {
                sprintf('{"dataset": %d, "model": %d}', as.integer(dataset_id), active_id)
            },
            `hx-confirm` = if (can_delete) tr("Are you sure?", lang, translations),
            `hx-swap` = if (can_delete) "none",
            bs_icon("trash", class = "me-1"),
            tr("Delete", lang, translations)
        )
    ))
}

# The self-replacing polling fragment: htmx re-fetches the job partial after
# `delay`, swapping this element with either another polling fragment or the
# terminal result, which naturally stops the polling. It carries the pre-fit
# active model id so a failed fit can restore the Delete button, and drops its
# request when a dataset switch (#page-body swap) is already in flight.
job_polling_fragment <- function(job_id, dataset_id, lang, translations, delay = "2s", active_model_id = NULL) {
    active_id <- suppressWarnings(as.integer(active_model_id %||% NA))
    render_tags(htmltools::div(
        class = "d-flex align-items-center gap-2 text-muted",
        `hx-get` = sprintf(
            "/partials/model/job/%s?dataset=%d&model=%s",
            job_id,
            as.integer(dataset_id),
            if (is.na(active_id)) "" else active_id
        ),
        `hx-trigger` = sprintf("load delay:%s", delay),
        `hx-target` = "this",
        `hx-swap` = "outerHTML",
        `hx-sync` = "#page-body:drop",
        htmltools::div(class = "spinner-border spinner-border-sm", role = "status"),
        htmltools::tags$span(tr("Fitting model...", lang, translations))
    ))
}

# Metrics cards + captured summary() output for a fitted/saved model
# (shiny-base parity: no delete button here - deleting the active model is the
# equation-card toolbar's job).
model_result_fragment <- function(model_detail, lang, translations) {
    metrics <- model_detail$metrics
    metric_card <- function(label, value) {
        htmltools::div(
            class = "col-md-4",
            htmltools::div(
                class = "metric-card p-3 border rounded",
                htmltools::tags$small(class = "text-muted", label),
                htmltools::div(class = "h4 mb-0", fmt_metric(value))
            )
        )
    }
    render_tags(htmltools::div(
        class = "card",
        htmltools::div(
            class = "card-body",
            htmltools::h5(class = "card-title mb-3", tr("Model Summary", lang, translations)),
            htmltools::div(
                class = "row g-3 mb-3",
                metric_card("R-squared", metrics$r_squared),
                metric_card("RMSE", metrics$rmse),
                metric_card("AIC", metrics$aic)
            ),
            htmltools::tags$pre(
                class = "border rounded p-3 bg-body-tertiary small mb-0",
                metrics$summary_text %||% ""
            )
        )
    ))
}

# Sidebar list of the dataset's saved models (shiny-base model-picker parity:
# formula only, no date; the active model's row is highlighted). Carries its
# own id so action responses can refresh it out-of-band. Per-row deletes pass
# the CURRENT active model id along, so the response knows whether the active
# state must be cleared.
saved_models_html <- function(models, dataset_id, lang, translations, active_model_id = NULL, oob = FALSE) {
    active_id <- suppressWarnings(as.integer(active_model_id %||% NA))
    items <- if (length(models) == 0) {
        htmltools::p(
            class = "text-muted small fst-italic mb-0",
            tr("No saved models for this dataset", lang, translations)
        )
    } else {
        htmltools::div(
            class = "model-picker",
            lapply(models, function(m) {
                id <- as.integer(m$id)
                selected <- !is.na(active_id) && identical(id, active_id)
                htmltools::div(
                    class = paste(c("model-picker-row", if (selected) "selected"), collapse = " "),
                    htmltools::tags$button(
                        type = "button",
                        class = "model-picker-select",
                        title = m$formula,
                        `hx-get` = sprintf("/partials/model/saved/%d", id),
                        `hx-target` = "#fit-status",
                        `hx-swap` = "innerHTML",
                        `hx-sync` = "#page-body:drop",
                        htmltools::tags$span(class = "model-picker-formula", m$formula)
                    ),
                    htmltools::tags$button(
                        type = "button",
                        class = "model-picker-delete",
                        title = tr("Delete model", lang, translations),
                        `hx-delete` = sprintf("/models/%d", id),
                        `hx-vals` = sprintf(
                            '{"dataset": %d, "model": %s}',
                            as.integer(dataset_id),
                            if (is.na(active_id)) '""' else active_id
                        ),
                        `hx-confirm` = tr("Are you sure?", lang, translations),
                        `hx-swap` = "none",
                        bs_icon("trash")
                    )
                )
            })
        )
    }
    section <- htmltools::div(
        id = "saved-models",
        `hx-swap-oob` = if (oob) "true",
        sidebar_section(tr("Saved Models", lang, translations), items)
    )
    render_tags(section)
}

# Out-of-band #fit-status reset used when a shown model is deleted.
fit_status_clear_oob <- function() {
    render_tags(htmltools::div(id = "fit-status", class = "mt-4", `hx-swap-oob` = "innerHTML"))
}

# The formula input mirrored out-of-band when the active model changes
# (loaded, fitted, or deleted - pass "" to clear).
formula_input_oob <- function(formula, lang, translations) {
    render_tags(htmltools::tags$input(
        type = "text",
        class = "form-control",
        id = "formula-input",
        name = "formula",
        value = formula,
        placeholder = "y ~ x1 + x2",
        `aria-label` = tr("Model Equation", lang, translations),
        `hx-swap-oob` = "true"
    ))
}
