# Admin page: Users (card grid + Auth0 role management) and Requests
# (request-log statistics) tabs. Tab switching is plain navigation (?tab=...),
# so each render fetches exactly one dataset. Scope exceptions vs shiny-base:
# request-log statistics replace the OTel traces tab, and the "Recently
# active" filter (last_seen_at based) replaces the connected-sessions view -
# the FE session store is not user-joinable, so activity is the honest signal.

ADMIN_RECENT_SEEN_SECONDS <- 15L * 60L

admin_content <- function(
    tab,
    data,
    hours,
    seen,
    lang,
    translations,
    can_manage_roles = FALSE,
    can_manage_users = FALSE,
    viewer_id = NULL
) {
    tab_link <- function(value, label) {
        htmltools::tags$li(
            class = "nav-item",
            htmltools::a(
                class = paste(c("nav-link", if (identical(tab, value)) "active"), collapse = " "),
                href = paste0("/admin?tab=", value),
                label
            )
        )
    }
    body <- switch(
        tab,
        users = admin_users_panel(data$items, seen, lang, translations, can_manage_roles, can_manage_users, viewer_id),
        requests = htmltools::div(
            class = "card",
            htmltools::div(class = "card-body", admin_requests_table(data$items, hours, lang, translations))
        )
    )
    render_tags(htmltools::div(
        id = "page-body",
        page_header(tr("Admin", lang, translations)),
        htmltools::tags$ul(
            class = "nav nav-tabs mb-4",
            tab_link("users", tr("Users", lang, translations)),
            tab_link("requests", tr("Requests", lang, translations))
        ),
        body
    ))
}

access_denied_content <- function(lang, translations) {
    render_tags(htmltools::div(
        id = "page-body",
        page_header(tr("Access denied", lang, translations)),
        htmltools::p(tr("You do not have permission to view this page.", lang, translations)),
        htmltools::a(class = "btn btn-primary", href = "/home", tr("Go to Home", lang, translations))
    ))
}

# Users tab: an all/recently-active filter plus one card per user.
admin_users_panel <- function(
    users,
    seen,
    lang,
    translations,
    can_manage_roles = FALSE,
    can_manage_users = FALSE,
    viewer_id = NULL
) {
    if (identical(seen, "recent")) {
        cutoff <- Sys.time() - ADMIN_RECENT_SEEN_SECONDS
        users <- Filter(function(u) isTRUE(parse_be_time(u$last_seen_at) >= cutoff), users)
    }
    seen_link <- function(value, label) {
        htmltools::a(
            class = paste(
                c("btn btn-sm", if (identical(seen, value)) "btn-secondary" else "btn-outline-secondary"),
                collapse = " "
            ),
            href = if (identical(value, "all")) "/admin?tab=users" else paste0("/admin?tab=users&seen=", value),
            label
        )
    }
    htmltools::tagList(
        htmltools::div(
            class = "d-flex gap-2 mb-3",
            seen_link("all", tr("All users", lang, translations)),
            seen_link("recent", tr("Recently active", lang, translations))
        ),
        if (length(users) == 0) {
            empty_state("people", tr("No users to show", lang, translations))
        } else {
            htmltools::div(
                class = "row g-3",
                lapply(users, function(u) {
                    htmltools::div(
                        class = "col-md-6 col-xl-4",
                        htmltools::HTML(admin_user_card_html(
                            u,
                            lang,
                            translations,
                            can_manage_roles,
                            can_manage_users,
                            viewer_id
                        ))
                    )
                })
            )
        }
    )
}

# One admin user card. Carries a stable id so a role or status change can
# refresh it out-of-band. Role and ban management target Auth0 identities, so
# both controls are absent for guests (no auth0_sub) and for viewers without the
# matching scope; the ban toggle is additionally absent on the viewer's own card
# (the backend refuses a self-ban with a 409 anyway).
admin_user_card_html <- function(
    user,
    lang,
    translations,
    can_manage_roles = FALSE,
    can_manage_users = FALSE,
    viewer_id = NULL,
    oob = FALSE
) {
    id <- as.integer(user$id)
    is_guest <- isTRUE(user$is_guest)
    email <- be_scalar(user$email)
    nickname <- be_scalar(user$nickname)
    role <- be_scalar(user$role)
    status <- be_scalar(user$status) %||% "active"
    is_banned <- identical(status, "banned")
    is_self <- !is.null(viewer_id) && identical(as.integer(viewer_id), id)
    count_item <- function(icon, title, value) {
        htmltools::tags$span(
            class = "text-muted small me-3",
            title = title,
            bs_icon(icon, class = "me-1"),
            fmt_count(value)
        )
    }
    render_tags(htmltools::div(
        class = "card admin-user-card h-100",
        id = sprintf("admin-user-%d", id),
        `hx-swap-oob` = if (oob) "true",
        htmltools::div(
            class = "card-body d-flex gap-3",
            bs_icon("person-fill", class = "admin-user-avatar"),
            htmltools::div(
                class = "flex-grow-1 overflow-hidden",
                htmltools::div(
                    class = "d-flex justify-content-between align-items-start gap-2",
                    htmltools::div(
                        class = "overflow-hidden",
                        htmltools::div(
                            class = "fw-semibold text-truncate",
                            email %||% nickname %||% sprintf("#%d", id)
                        ),
                        if (!is.null(email) && !is.null(nickname)) {
                            htmltools::div(class = "text-muted small text-truncate", nickname)
                        }
                    ),
                    if (can_manage_roles && !is_guest) {
                        htmltools::tags$button(
                            type = "button",
                            class = "btn btn-sm btn-outline-secondary",
                            title = tr("Edit role", lang, translations),
                            `hx-get` = sprintf("/partials/admin/users/%d/role", id),
                            `hx-target` = "#modal-slot",
                            `hx-swap` = "innerHTML",
                            bs_icon("person-gear")
                        )
                    },
                    if (can_manage_users && !is_guest && !is_self) {
                        htmltools::tags$button(
                            type = "button",
                            class = paste(
                                c("btn btn-sm", if (is_banned) "btn-outline-success" else "btn-outline-danger"),
                                collapse = " "
                            ),
                            title = if (is_banned) {
                                tr("Unban user", lang, translations)
                            } else {
                                tr("Ban user", lang, translations)
                            },
                            `hx-put` = sprintf("/admin/users/%d/status", id),
                            `hx-vals` = sprintf('{"status": "%s"}', if (is_banned) "active" else "banned"),
                            `hx-target` = sprintf("#admin-user-%d", id),
                            `hx-swap` = "outerHTML",
                            `hx-confirm` = if (is_banned) {
                                tr("Are you sure you want to unban this user?", lang, translations)
                            } else {
                                tr("Are you sure you want to ban this user?", lang, translations)
                            },
                            `hx-disabled-elt` = "this",
                            bs_icon(if (is_banned) "unlock" else "slash-circle"),
                            htmltools::tags$span(
                                class = "ms-1",
                                if (is_banned) tr("Unban", lang, translations) else tr("Ban", lang, translations)
                            )
                        )
                    }
                ),
                htmltools::div(
                    class = "mt-1",
                    htmltools::tags$span(
                        class = paste(
                            c("badge me-1", if (identical(role, "admin")) "text-bg-primary" else "text-bg-secondary"),
                            collapse = " "
                        ),
                        title = if (is.null(role)) tr("Default role (no explicit Auth0 role)", lang, translations),
                        role %||% "user"
                    ),
                    if (is_guest) {
                        htmltools::tags$span(class = "badge text-bg-warning", tr("Guest", lang, translations))
                    },
                    if (!identical(status, "active")) {
                        htmltools::tags$span(
                            class = "badge text-bg-danger ms-1",
                            tr(if (is_banned) "Banned" else "Deleted", lang, translations)
                        )
                    }
                ),
                htmltools::div(
                    class = "mt-2",
                    count_item("folder2-open", tr("Datasets", lang, translations), user$n_datasets),
                    count_item("graph-up", tr("Models", lang, translations), user$n_models),
                    count_item("key", tr("API Keys", lang, translations), user$n_api_keys)
                ),
                htmltools::div(
                    class = "text-muted small mt-1",
                    bs_icon("clock", class = "me-1"),
                    paste(tr("Last seen", lang, translations), fmt_date(user$last_seen_at))
                )
            )
        )
    ))
}

# Role-edit modal, served into #modal-slot (app.js opens it on swap). The
# select offers the tenant's Auth0 roles (the empty value = clear back to the
# default role); roles absent from permissions.yaml grant nothing beyond the
# default scopes and are flagged as such.
admin_role_modal_html <- function(user, roles, default_role, lang, translations) {
    user_id <- as.integer(user$id)
    current <- be_scalar(user$role)
    role_option <- function(role) {
        name <- be_scalar(role$name)
        label <- if (isTRUE(be_scalar(role$in_yaml))) {
            name
        } else {
            sprintf("%s (%s)", name, tr("no scopes mapped", lang, translations))
        }
        htmltools::tags$option(
            value = be_scalar(role$id),
            selected = if (identical(name, current)) NA,
            label
        )
    }
    options <- c(
        list(htmltools::tags$option(
            value = "",
            selected = if (is.null(current)) NA,
            sprintf("%s (%s)", default_role %||% "user", tr("default", lang, translations))
        )),
        lapply(roles, role_option)
    )
    modal_html(
        id = "role-modal",
        title = paste0(
            tr("Change role", lang, translations),
            ": ",
            be_scalar(user$email) %||% be_scalar(user$nickname) %||% sprintf("#%d", user_id)
        ),
        body = htmltools::tags$form(
            `hx-put` = sprintf("/admin/users/%d/role", user_id),
            `hx-target` = "#role-modal-status",
            `hx-swap` = "innerHTML",
            htmltools::div(
                class = "mb-3",
                htmltools::tags$label(class = "form-label", `for` = "role-select", tr("Role", lang, translations)),
                htmltools::tags$select(class = "form-select", id = "role-select", name = "role_id", options)
            ),
            htmltools::p(
                class = "text-muted small",
                tr("Role changes apply on the next token refresh (up to 15 minutes).", lang, translations)
            ),
            htmltools::div(id = "role-modal-status", class = "mb-3"),
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
                    tr("Save", lang, translations)
                )
            )
        )
    )
}

# One user from the admin listing (there is no single-user admin endpoint;
# the list is one indexed query, so re-fetching it is cheap).
admin_find_user <- function(state, datastore, user_id) {
    users <- be_get(state, datastore, "/v1/admin/users")$items
    Find(function(u) identical(as.integer(u$id), as.integer(user_id)), users)
}

# Backend ISO-8601 UTC timestamp (format_time_or_null) -> POSIXct, or NULL.
parse_be_time <- function(x) {
    x <- be_scalar(x)
    if (is.null(x)) {
        return(NULL)
    }
    parsed <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    if (is.na(parsed)) NULL else parsed
}

admin_requests_table <- function(requests, hours, lang, translations) {
    window_link <- function(h, label) {
        htmltools::a(
            class = paste(
                c("btn btn-sm", if (identical(hours, h)) "btn-secondary" else "btn-outline-secondary"),
                collapse = " "
            ),
            href = sprintf("/admin?tab=requests&hours=%d", h),
            label
        )
    }
    rows <- lapply(requests, function(r) {
        htmltools::tags$tr(
            htmltools::tags$td(r$service),
            htmltools::tags$td(r$method),
            htmltools::tags$td(htmltools::tags$code(r$path)),
            htmltools::tags$td(as.character(r$status)),
            htmltools::tags$td(fmt_count(r$n)),
            htmltools::tags$td(fmt_metric(r$avg_ms)),
            htmltools::tags$td(fmt_count(r$max_ms))
        )
    })
    htmltools::tagList(
        htmltools::div(
            class = "d-flex gap-2 mb-3",
            window_link(24L, tr("Last 24 hours", lang, translations)),
            window_link(168L, tr("Last 7 days", lang, translations)),
            window_link(720L, tr("Last 30 days", lang, translations))
        ),
        data_table(
            headers = c(
                tr("Service", lang, translations),
                tr("Method", lang, translations),
                tr("Path", lang, translations),
                tr("Status", lang, translations),
                tr("Count", lang, translations),
                tr("Avg ms", lang, translations),
                tr("Max ms", lang, translations)
            ),
            rows = rows,
            class = "table table-sm table-hover align-middle"
        )
    )
}
