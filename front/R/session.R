# Server-side login sessions. The session data (profile, roles, tokens) lives in
# the Postgres-backed firesale datastore keyed by the client-id cookie; the
# browser holds ONLY that opaque id. Refresh tokens are encrypted at rest
# (sodium secretbox, key derived from SESSION_KEY). Lifetimes: rolling idle 8h,
# absolute 7d (documented deviation from OWASP's 15-30min idle; personal app).

SESSION_IDLE_SECONDS <- 28800L
SESSION_ABSOLUTE_SECONDS <- 604800L
SESSION_TOUCH_SECONDS <- 60L

# Client-id cookie converter used instead of fiery::session_id_cookie, which
# hardcodes SameSite=Strict (plain) / None (secure). Strict drops the cookie on
# the top-level redirect back from Auth0 (the /callback request would start a
# fresh session and the state/nonce lookup would fail), and None gives up the
# browser-side CSRF layer. Lax is the correct setting for an OIDC code-flow app.
session_cookie_converter <- function(name, secure) {
    pattern <- paste0("(?:^|;\\s*)", name, "=([^;]+)")
    function(request) {
        cookie <- request$origin$HTTP_COOKIE
        if (!is.null(cookie)) {
            found <- regmatches(cookie, regexec(pattern, cookie, perl = TRUE))[[1]]
            if (length(found) == 2) {
                return(found[2])
            }
        }
        id <- reqres::random_key()
        request$respond()$set_cookie(
            name,
            id,
            http_only = TRUE,
            path = "/",
            secure = secure,
            same_site = "Lax"
        )
        request$origin$HTTP_COOKIE <- paste0(c(cookie, paste0(name, "=", id)), collapse = "; ")
        id
    }
}

session_cookie_name <- function(is_prod) {
    if (is_prod) "__Host-session" else "fb_session"
}

# --- session lifecycle -------------------------------------------------------

# Create the authenticated session. `user` is a users-table row; `tokens` is the
# Auth0 token response (NULL for guest sessions, which authenticate to the BE
# via its bypass guard instead of a bearer token).
create_auth_session <- function(datastore, user, roles, tokens, config, picture = NULL) {
    now <- as.numeric(Sys.time())
    auth <- list(
        user_id = as.integer(user$id),
        sub = if (!is.na(user$auth0_sub)) user$auth0_sub,
        email = if (!is.na(user$email)) user$email,
        nickname = if (!is.na(user$nickname)) user$nickname,
        picture = picture,
        roles = roles,
        is_guest = isTRUE(user$is_guest),
        csrf_id = sodium::bin2hex(sodium::random(16)),
        created_at = now,
        last_seen = now
    )
    if (!is.null(tokens)) {
        auth$access_token <- tokens$access_token
        auth$access_expires_at <- now + (tokens$expires_in %||% 900)
        if (!is.null(tokens$refresh_token)) {
            auth$refresh_token_enc <- encrypt_secret(tokens$refresh_token, refresh_key(config))
        }
    }
    datastore$session$auth <- auth
    auth
}

# The valid auth session, or NULL. An idle- or absolutely-expired session is
# destroyed on sight so a replayed cookie cannot resurrect it.
session_auth <- function(datastore) {
    auth <- datastore$session$auth
    if (is.null(auth)) {
        return(NULL)
    }
    now <- as.numeric(Sys.time())
    if (now - auth$created_at > SESSION_ABSOLUTE_SECONDS || now - auth$last_seen > SESSION_IDLE_SECONDS) {
        destroy_auth_session(datastore)
        return(NULL)
    }
    auth
}

# Refresh the rolling idle timeout, throttled so not every request rewrites the
# session row.
touch_session <- function(datastore, auth) {
    now <- as.numeric(Sys.time())
    if (now - auth$last_seen > SESSION_TOUCH_SECONDS) {
        auth$last_seen <- now
        datastore$session$auth <- auth
    }
    invisible(auth)
}

destroy_auth_session <- function(datastore) {
    datastore$session$auth <- NULL
    datastore$session$oidc <- NULL
    invisible()
}

# --- secrets at rest ---------------------------------------------------------

# Purpose-separated 32-byte keys derived from SESSION_KEY (delegated to auth0r;
# the "base-front:" prefix keeps this service's keys distinct). The prefix is a
# FROZEN constant from the service's original name: changing it would re-derive
# every key and make refresh tokens already encrypted at rest undecryptable.
derive_key <- function(session_key, purpose) {
    auth0r::derive_key(session_key, paste0("base-front:", purpose))
}

refresh_key <- function(config) {
    derive_key(config$session_key, "refresh-token")
}

encrypt_secret <- auth0r::encrypt_secret

decrypt_secret <- auth0r::decrypt_secret

# --- users table -------------------------------------------------------------

FE_USER_COLUMNS <- "id, auth0_sub, email, nickname, is_guest, created_at, last_seen_at"

# Provision/update the user at login. Auth0 is the profile source of truth, so
# fresh claims overwrite the stored email/nickname.
fe_get_or_create_user <- function(con, sub, email = NULL, nickname = NULL) {
    DBI::dbGetQuery(
        con,
        sprintf(
            "INSERT INTO users (auth0_sub, email, nickname, last_seen_at) VALUES ($1, $2, $3, now())
             ON CONFLICT (auth0_sub) DO UPDATE
             SET email = EXCLUDED.email, nickname = EXCLUDED.nickname, last_seen_at = now()
             RETURNING %s",
            FE_USER_COLUMNS
        ),
        params = list(sub, email %||% NA, nickname %||% NA)
    )
}

# Nickname edit from the profile modal (Auth0 is updated separately via the
# Management API; this keeps the local mirror in sync).
fe_update_nickname <- function(con, user_id, nickname) {
    DBI::dbExecute(
        con,
        "UPDATE users SET nickname = $1 WHERE id = $2",
        params = list(nickname, user_id)
    )
    invisible()
}

# The shared guest user for bypass mode (dev only); reuses the seeded row.
# Sentinel is `is_guest AND auth0_sub IS NULL`: shiny-base's per-session guests
# (shared users table) also carry is_guest = true but with guest_/tmp_ subs and
# get deleted by its cleanup (see back/R/users.R).
fe_get_or_create_guest <- function(con) {
    guest <- DBI::dbGetQuery(
        con,
        sprintf("SELECT %s FROM users WHERE is_guest AND auth0_sub IS NULL ORDER BY id LIMIT 1", FE_USER_COLUMNS)
    )
    if (nrow(guest) > 0) {
        return(guest)
    }
    DBI::dbGetQuery(
        con,
        sprintf(
            "INSERT INTO users (nickname, is_guest, last_seen_at) VALUES ('guest', true, now()) RETURNING %s",
            FE_USER_COLUMNS
        )
    )
}
