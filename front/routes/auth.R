#* Sign in. Guest mode (BYPASS_AUTH, dev only) creates the guest session
#* directly; otherwise stores the one-shot OIDC state (state/nonce/PKCE
#* verifier/target) in the server-side session and redirects to Auth0's
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
    oidc <- list(
        state = random_url_token(),
        nonce = random_url_token(),
        verifier = random_url_token(),
        next_path = next_path,
        created_at = as.numeric(Sys.time())
    )
    datastore$session$oidc <- oidc
    redirect(response, build_authorize_url(config, oidc$state, oidc$nonce, pkce_challenge(oidc$verifier)))
}

#* OIDC callback: verify the state echo, exchange the code (client secret +
#* PKCE verifier), validate the ID token (signature/iss/aud/azp/exp/iat/nonce),
#* gate on email_verified, provision the user and create the session. The
#* stored OIDC state is cleared up front so a code/state can never be replayed
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

    if (!is.null(query$error) || is.null(oidc) || is.null(query$code) || !identical(query$state, oidc$state)) {
        reason <- if (!is.null(query$error)) {
            paste0("upstream error '", query$error, "'")
        } else if (is.null(oidc)) {
            "no pending OIDC state in the session"
        } else if (is.null(query$code)) {
            "missing code"
        } else {
            "state mismatch"
        }
        server$log("warning", paste0("login callback rejected: ", reason), request)
        return(render_login_error(request, response, state))
    }
    tokens <- tryCatch(exchange_code(config, query$code, oidc$verifier), error = function(e) {
        message <- gsub("\n", " ", conditionMessage(e), fixed = TRUE)
        server$log("warning", paste0("login code exchange failed: ", message), request)
        NULL
    })
    if (is.null(tokens)) {
        return(render_login_error(request, response, state))
    }
    if (is.null(tokens$id_token) || is.null(tokens$access_token)) {
        server$log("warning", "login token response is missing id_token or access_token", request)
        return(render_login_error(request, response, state))
    }
    claims <- tryCatch(
        validate_id_token(tokens$id_token, config, expected_nonce = oidc$nonce),
        error = function(e) {
            server$log("warning", paste0("login ID token rejected: ", conditionMessage(e)), request)
            NULL
        }
    )
    if (is.null(claims)) {
        return(render_login_error(request, response, state))
    }
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
        tokens = tokens,
        config = config,
        picture = claims$picture
    )
    redirect(response, oidc$next_path %||% "/home")
}

#* Sign out: revoke the refresh token at Auth0 (best effort), destroy the
#* server-side session, clear client state, then finish the logout at Auth0 so
#* its SSO cookie dies too. Outstanding BE access tokens stay valid until exp
#* (bounded by the short TTL; documented tradeoff). POST (not GET) so the gate's
#* CSRF + Origin checks apply - a public GET logout would be CSRFable. The navbar
#* triggers it with an htmx POST, so the response uses HX-Redirect for htmx.
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
        paste0(
            auth0_base_url(config$auth0$domain),
            "/v2/logout?client_id=",
            config$auth0$client_id,
            "&returnTo=",
            utils::URLencode(config$app_url, reserved = TRUE)
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
