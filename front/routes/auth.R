#* Sign in. Guest mode (BYPASS_AUTH, dev only) creates the guest session
#* directly; otherwise stores the one-shot OIDC transaction (state/nonce/PKCE
#* verifier + target) in the server-side session and redirects to Auth0's
#* /authorize. `next` must be a local path (open-redirect guard).
#* @query next:string Where to land after login (local absolute path)
#* @get /login
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    config <- state$config
    next_path <- if (is_safe_next(query[["next"]] %||% "")) query[["next"]] else "/home"
    if (!is.null(session_auth(datastore))) {
        return(redirect(response, next_path))
    }
    if (config$bypass_auth) {
        guest <- fe_get_or_create_guest(state$con)
        create_auth_session(datastore, user = guest, roles = character(), tokens = NULL, config = config)
        return(redirect(response, next_path))
    }
    # offline_access requests the rotating refresh token; the API must have
    # "Allow Offline Access" enabled in Auth0.
    login_start <- auth0r::oidc_login_start(
        app_auth0_client(config),
        redirect_uri = oidc_redirect_uri(config),
        scope = "openid profile email offline_access",
        audience = config$auth0$audience
    )
    datastore$session$oidc <- list(txn = login_start$txn, next_path = next_path)
    redirect(response, login_start$url)
}

#* OIDC callback: auth0r::oidc_login_complete verifies the callback shape and
#* state echo, exchanges the code (client secret + PKCE verifier) and validates
#* the ID token (signature/iss/aud/azp/exp/iat/nonce); the route gates on
#* email_verified, provisions the user and creates the session. The stored OIDC
#* transaction is cleared up front so a code/state can never be replayed
#* against the same session.
#* @query code:string The authorization code
#* @query state:string The state echo
#* @query error:string Auth0 error code, when the flow failed upstream
#* @get /callback
#* @serializer html
function(request, response, server, datastore, query) {
    state <- server$get_data("state")
    config <- state$config
    oidc <- datastore$session$oidc
    datastore$session$oidc <- NULL

    login <- tryCatch(
        auth0r::oidc_login_complete(
            app_auth0_client(config),
            txn = oidc$txn,
            params = query,
            redirect_uri = oidc_redirect_uri(config),
            fetch_userinfo = FALSE
        ),
        auth0r_login_error = function(e) {
            reason <- paste0(
                class(e)[1],
                ": ",
                gsub("\n", " ", conditionMessage(e), fixed = TRUE),
                if (!is.null(e$upstream_error)) paste0(" (upstream '", e$upstream_error, "')") else ""
            )
            server$log("warning", paste0("login callback rejected: ", reason), request)
            NULL
        }
    )
    if (is.null(login)) {
        return(render_login_error(request, response, state))
    }
    claims <- login$claims
    if (!isTRUE(claims$email_verified)) {
        return(redirect(response, "/unverified"))
    }

    user <- fe_get_or_create_user(
        state$con,
        sub = claims$sub,
        email = claims$email,
        nickname = claims$nickname %||% claims$name
    )
    roles <- unlist(claims[[paste0(config$auth0$claim_namespace, "roles")]], use.names = FALSE) %||% character()
    create_auth_session(
        datastore,
        user = user,
        roles = roles,
        tokens = login$tokens,
        config = config,
        picture = claims$picture,
        sid = claims$sid
    )
    redirect(response, oidc$next_path %||% "/home")
}

#* Sign out: revoke the refresh token at Auth0 (best effort), destroy the
#* server-side session, clear client state, then finish the logout at Auth0's
#* OIDC /oidc/logout endpoint so its SSO cookie dies too. The session does not
#* retain the ID token, so the logout hint is the session's OIDC `sid` claim
#* (config$app_url must be in the application's Allowed Logout URLs).
#* Outstanding BE access tokens stay valid until exp (bounded by the short TTL;
#* documented tradeoff). POST (not GET) so the gate's CSRF + Origin checks
#* apply - a public GET logout would be CSRFable. The navbar triggers it with
#* an htmx POST, so the response uses HX-Redirect for htmx.
#* @post /logout
#* @serializer html
function(request, response, server, datastore) {
    state <- server$get_data("state")
    config <- state$config
    auth <- datastore$session$auth
    if (!is.null(auth$refresh_token_enc)) {
        tryCatch(
            revoke_refresh_token(config, decrypt_secret(auth$refresh_token_enc, refresh_key(config))),
            error = function(e) NULL
        )
    }
    destroy_auth_session(datastore)
    response$set_header("Clear-Site-Data", '"cookies", "storage"')
    response$remove_cookie(csrf_cookie_name(identical(config$environment, "prod")))
    target <- if (config$bypass_auth || !nzchar(config$auth0$domain)) {
        "/login"
    } else {
        app_auth0_client(config)$logout_url(
            return_to = config$app_url,
            logout_hint = auth$sid
        )
    }
    redirect_htmx_aware(request, response, target)
}

#* Email-verification gate page (UX only; the real enforcement is the callback's
#* email_verified check plus the BE's verified-email claim check).
#* @get /unverified
#* @serializer html
function(request, response, server) {
    state <- server$get_data("state")
    lang <- resolve_lang(request, state$translations)
    title <- tr("Email verification required", lang, state$translations)
    content <- render_tags(
        htmltools::h1(class = "mb-4", title),
        htmltools::p(tr("Please verify your email address, then sign in again.", lang, state$translations)),
        htmltools::a(class = "btn btn-primary", href = "/login", tr("Try again", lang, state$translations))
    )
    render_page(request, response, content = content, title = title, lang = lang, state = state)
}
