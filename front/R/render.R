# Page-rendering core. render_page() is the single decision point for htmx
# content negotiation (fragment vs full page) and the single place that stamps
# the shared HTML response headers, so no route handler repeats either concern.
#
# HTML is emitted as a plain (bare) character string returned from a handler that
# declares `@serializer html`: plumber2's html serializer passes bare strings
# through verbatim (it only escapes/XML-wraps non-bare objects). The whisker T3
# shell and htmltools content are therefore collapsed to a single unclassed
# string before returning.

render_page <- function(
    request,
    response,
    content,
    title,
    lang,
    state,
    oob = NULL,
    user = NULL,
    scopes = character(),
    show_nav = TRUE
) {
    set_html_headers(response)
    body <- paste0(content, oob %||% "", collapse = "")
    if (is_htmx_request(request)) {
        return(body)
    }
    render_shell(
        content = body,
        title = title,
        lang = lang,
        state = state,
        csrf_token = response$get_data("csrf_token") %||% "",
        user = user,
        scopes = scopes,
        active_path = request$path,
        show_nav = show_nav
    )
}

is_htmx_request <- function(request) {
    identical(request$get_header("HX-Request"), "true")
}

# Issue an HTTP redirect. Handlers must return the value of this call so the
# response is sent as-is (Break stops the handler stack). Redirects are always
# session-dependent (login/logout/lang/next targets), so never cacheable.
redirect <- function(response, location, status = 302L) {
    response$status <- status
    response$set_header("Location", location)
    response$set_header("Cache-Control", "private, no-store")
    plumber2::Break
}

# Redirect that works whether the caller is a normal navigation (302 + Location)
# or an htmx request (200 + HX-Redirect; htmx does not follow 3xx headers). Used
# by the logout action, which is triggered by an htmx POST button but may also be
# reached by a plain form post. Handlers must return the value of this call.
redirect_htmx_aware <- function(request, response, location) {
    response$set_header("Cache-Control", "private, no-store")
    if (is_htmx_request(request)) {
        response$status <- 200L
        response$set_header("HX-Redirect", location)
        return("")
    }
    response$status <- 302L
    response$set_header("Location", location)
    plumber2::Break
}

# Out-of-band fragment appended to a response to surface a toast in the shell's
# #toasts container. Mirrors the markup app.js builds client-side.
render_toast <- function(message, level = "info") {
    variant <- switch(level, error = "danger", success = "success", warning = "warning", info = "info", "info")
    toast <- htmltools::div(
        class = paste0("toast align-items-center text-bg-", variant, " border-0"),
        role = "alert",
        `aria-live` = "assertive",
        `aria-atomic` = "true",
        htmltools::div(
            class = "d-flex",
            htmltools::div(class = "toast-body", message)
        )
    )
    render_tags(htmltools::div(`hx-swap-oob` = "beforeend:#toasts", toast))
}

# Resolve the interface language: an explicit `lang` cookie wins, then the
# Accept-Language header, then English.
resolve_lang <- function(request, translations) {
    valid <- translations$languages
    cookie <- request$cookies$lang
    if (!is.null(cookie) && cookie %in% valid) {
        return(cookie)
    }
    accepted <- request$accepts_language(valid)
    if (!is.null(accepted) && accepted %in% valid) {
        return(accepted)
    }
    "en"
}

# --- helpers ---------------------------------------------------------------

set_html_headers <- function(response) {
    response$set_header("Vary", "HX-Request")
    response$set_header("Cache-Control", "private, no-store")
    invisible(response)
}

render_shell <- function(
    content,
    title,
    lang,
    state,
    csrf_token = "",
    user = NULL,
    scopes = character(),
    active_path = "",
    show_nav = TRUE
) {
    manifest <- state$manifest
    translations <- state$translations
    data <- list(
        lang = lang,
        title = title,
        csrf_token = csrf_token,
        show_nav = show_nav,
        msg_server_error = tr("Server error, please try again", lang, translations),
        asset_bootstrap_css = static_url(manifest, "vendor/bootstrap.min.css"),
        asset_icons_css = static_url(manifest, "vendor/bootstrap-icons.min.css"),
        asset_app_css = static_url(manifest, "css/app.css"),
        asset_htmx_js = static_url(manifest, "vendor/htmx.min.js"),
        asset_bootstrap_js = static_url(manifest, "vendor/bootstrap.bundle.min.js"),
        asset_app_js = static_url(manifest, "js/app.js"),
        brand = tr("Plumber2 Base", lang, translations),
        nav_links = nav_links_html(
            lang,
            translations,
            show_admin = "view:admin" %in% scopes,
            active_path = active_path
        ),
        user_menu = user_menu_html(user, lang, translations, manifest),
        content = content
    )
    whisker::whisker.render(state$template, data)
}

# Lang switcher for everyone; a user dropdown (Profile / Account / Logout) when
# a session exists. The profile item opens the server-rendered modal via htmx
# (#modal-slot + app.js); it is disabled for guests, who have no Auth0 profile
# to edit (shiny-base parity).
user_menu_html <- function(user, lang, translations, manifest) {
    menu <- lang_switcher_html(lang, translations, manifest)
    if (is.null(user)) {
        return(menu)
    }
    is_guest <- isTRUE(user$is_guest)
    profile_item <- htmltools::tags$button(
        type = "button",
        class = paste(c("dropdown-item", if (is_guest) "disabled"), collapse = " "),
        disabled = if (is_guest) NA,
        `hx-get` = "/partials/profile",
        `hx-target` = "#modal-slot",
        `hx-swap` = "innerHTML",
        tr("Profile", lang, translations)
    )
    dropdown <- htmltools::tags$li(
        class = "nav-item dropdown",
        htmltools::a(
            class = "nav-link dropdown-toggle",
            href = "#",
            role = "button",
            `data-bs-toggle` = "dropdown",
            `aria-expanded` = "false",
            htmltools::tags$span(
                id = "navbar-user-name",
                title = tr("Signed in as", lang, translations),
                display_name(user)
            )
        ),
        htmltools::tags$ul(
            class = "dropdown-menu dropdown-menu-end",
            htmltools::tags$li(profile_item),
            htmltools::tags$li(htmltools::a(
                class = "dropdown-item",
                href = "/account",
                tr("Account", lang, translations)
            )),
            htmltools::tags$li(htmltools::tags$hr(class = "dropdown-divider")),
            htmltools::tags$li(htmltools::tags$button(
                type = "button",
                class = "dropdown-item",
                `hx-post` = "/logout",
                `hx-swap` = "none",
                tr("Logout", lang, translations)
            ))
        )
    )
    paste0(menu, render_tags(dropdown))
}

display_name <- function(user) {
    for (candidate in c(user$nickname, user$email)) {
        if (!is.null(candidate) && !is.na(candidate) && nzchar(candidate)) {
            return(candidate)
        }
    }
    "user"
}

# Navbar-less terminal page (login failures, email gate): a centered card on
# the bare shell, for states where no usable session exists so app navigation
# is dead weight. Mirrors auth0r's terminal_login_error page in shiny-base.
render_terminal_page <- function(request, response, state, lang, title, message, footer = NULL) {
    content <- render_tags(
        htmltools::div(
            class = "row justify-content-center mt-5",
            htmltools::div(
                class = "col-11 col-sm-8 col-md-6 col-xl-4",
                htmltools::div(
                    class = "card text-center shadow-sm",
                    htmltools::div(
                        class = "card-body p-4",
                        htmltools::h1(class = "h4 card-title mb-3", title),
                        htmltools::p(message),
                        footer
                    )
                )
            )
        )
    )
    render_page(
        request,
        response,
        content = content,
        title = title,
        lang = lang,
        state = state,
        show_nav = FALSE
    )
}

# 403 login-failure page used by the /callback error paths.
render_login_error <- function(request, response, state, blocked = FALSE) {
    response$status <- 403L
    config <- state$config
    lang <- resolve_lang(request, state$translations)
    title_key <- if (blocked) "Account suspended" else "Login failed"
    message_key <- if (blocked) {
        "Your account has been suspended by an administrator. If you believe this is a mistake, please contact them."
    } else {
        "The sign-in attempt could not be completed. Please try again."
    }
    # No local session exists on a failed login, but Auth0's SSO cookie may:
    # without an explicit Auth0 logout the next /login silently replays the
    # same (e.g. blocked) account. Legacy /v2/logout because there is no ID
    # token to hint /oidc/logout with (auth0r terminal-page parity).
    logout_href <- if (!config$bypass_auth && nzchar(config$auth0$domain)) {
        app_auth0_client(config)$logout_url(return_to = config$app_url, legacy = TRUE)
    }
    render_terminal_page(
        request,
        response,
        state,
        lang,
        title = tr(title_key, lang, state$translations),
        message = tr(message_key, lang, state$translations),
        footer = list(
            # Retrying is pointless while the account is blocked upstream.
            if (!blocked) {
                htmltools::p(htmltools::a(
                    class = "btn btn-primary",
                    href = "/login",
                    tr("Try again", lang, state$translations)
                ))
            },
            if (!is.null(logout_href)) {
                htmltools::a(href = logout_href, tr("Log out of Auth0", lang, state$translations))
            }
        )
    )
}

static_url <- function(manifest, logical) {
    paste0("/static/", asset_path(manifest, logical))
}

nav_links_html <- function(lang, translations, show_admin = FALSE, active_path = "") {
    item <- function(href, label) {
        class <- if (identical(active_path, href)) "nav-link active" else "nav-link"
        render_tags(htmltools::tags$li(
            class = "nav-item",
            htmltools::a(class = class, href = href, label)
        ))
    }
    paste0(
        item("/home", tr("Home", lang, translations)),
        item("/explore", tr("Explore", lang, translations)),
        item("/model", tr("Model", lang, translations)),
        if (show_admin) item("/admin", tr("Admin", lang, translations)) else ""
    )
}

# Compact language switcher rendered as a Bootstrap dropdown (shiny-base parity):
# a bordered toggle showing the active language's flag + code, and a menu of the
# available languages linking to /lang/{code}. Flags are vendored SVGs resolved
# through the asset manifest (shiny renders emoji flags instead; the strict CSP
# system-font stack here has no reliable emoji coverage).
lang_switcher_html <- function(lang, translations, manifest) {
    labels <- list(en = tr("English", lang, translations), fr = tr("French", lang, translations))
    flags <- list(en = "vendor/flags/gb.svg", fr = "vendor/flags/fr.svg")
    flag_img <- function(code) {
        htmltools::tags$img(
            class = "language-flag",
            src = static_url(manifest, flags[[code]]),
            alt = "",
            width = "20",
            height = "15"
        )
    }
    item <- function(code) {
        class <- paste(c("dropdown-item", if (identical(code, lang)) "active"), collapse = " ")
        htmltools::tags$li(htmltools::a(
            class = class,
            href = paste0("/lang/", code),
            flag_img(code),
            labels[[code]]
        ))
    }
    render_tags(htmltools::tags$li(
        class = "nav-item dropdown language-dropdown",
        htmltools::tags$button(
            class = "language-dropdown-toggle dropdown-toggle",
            type = "button",
            `data-bs-toggle` = "dropdown",
            `aria-expanded` = "false",
            `aria-label` = tr("Language", lang, translations),
            flag_img(lang),
            htmltools::tags$span(class = "language-code", toupper(lang))
        ),
        htmltools::tags$ul(
            class = "dropdown-menu dropdown-menu-end",
            item("en"),
            item("fr")
        )
    ))
}

# Collapse htmltools tags to a single unclassed string (required for the bare
# string / html-serializer verbatim path).
render_tags <- function(...) {
    paste0(as.character(htmltools::tagList(...)), collapse = "")
}
