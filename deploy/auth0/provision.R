#!/usr/bin/env Rscript
# One-time Auth0 provisioning for plumber2-base, built on
# auth0r::Auth0Management's idempotent ensure_* helpers. Safe to re-run:
# existing artifacts are updated in place and the post-login trigger binding
# list is PRESERVED (existing bindings, e.g. shiny-base's Action, are
# re-referenced by binding_id; ours is appended only if absent).
#
# Creates/updates:
#   1. Resource server (API) "plumber2-base-api": RS256, 900s token TTL,
#      offline access (refresh tokens).
#   2. Regular Web Application "plumber2-base-front": code + refresh_token
#      grants, rotating refresh tokens, prod + localhost callbacks, RS256 ID
#      tokens (API-created clients default to HS256, which the FE validator
#      rejects; auth0r pins the alg).
#   3. M2M application "plumber2-base-mgmt" with minimal Management API scopes
#      (read:users update:users) for the profile modal.
#   4. Post-login Action from post-login-action.js (roles + email_verified on
#      the ACCESS token), deployed and bound to the post-login trigger.
#
# Input:  ~/.keys/.auth0 ("export KEY=value" lines, like the other .keys files)
#           AUTH0_TENANT_DOMAIN=<tenant>.eu.auth0.com
#           AUTH0_MGMT_TOKEN=<short-lived Management API token>
#         (Dashboard -> APIs -> Auth0 Management API -> API Explorer -> token;
#         24h lifetime, delete the token/application afterwards.)
# Output: <repo root>/app.env (0600, gitignored via *.env) - the server-side
#         app.env content (both services share it; each ignores the other's
#         vars). SESSION_KEY is generated once and preserved on re-runs.
#         Secrets are never printed to stdout.

suppressPackageStartupMessages({
    library(auth0r)
    library(cli)
})

fe_url <- "https://plumber2-base.ma-riviere.com"
be_url <- "https://plumber2-base-api.ma-riviere.com"
local_fe_url <- "http://localhost:8080"
claim_namespace <- paste0(fe_url, "/")
api_display_name <- "plumber2-base-api"
fe_client_name <- "plumber2-base-front"
mgmt_client_name <- "plumber2-base-mgmt"
action_name <- "plumber2-base-post-login"
# Short BE token TTL instead of a jti denylist (locked decision): bounds the
# post-logout exposure window.
access_token_lifetime <- 900L
# read:roles covers role listing AND is required alongside update:users for
# assigning/removing user roles (the BE admin role endpoints).
mgmt_scopes <- c("read:users", "update:users", "read:roles")

keys_env_path <- path.expand("~/.keys/.auth0")
script_dir <- dirname(sub("--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1], fixed = TRUE))
# Repo root (script lives in deploy/auth0/). NOT deploy/: the prod compose
# resolves `env_file: app.env` relative to itself, so a real app.env there
# would leak prod secrets into the local guest-mode stack.
output_env_path <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE) |> file.path("app.env")
action_js_path <- file.path(script_dir, "post-login-action.js")

main <- function() {
    cfg <- read_env_file(keys_env_path)
    for (key in c("AUTH0_TENANT_DOMAIN", "AUTH0_MGMT_TOKEN")) {
        if (!nzchar(cfg[key] %||% "")) {
            cli_abort("{.path {keys_env_path}} must define {.envvar {key}}.")
        }
    }
    tenant <- cfg[["AUTH0_TENANT_DOMAIN"]]
    mgmt <- Auth0Management$new(domain = tenant, token = cfg[["AUTH0_MGMT_TOKEN"]])

    mgmt$ensure_resource_server(
        identifier = be_url,
        name = api_display_name,
        settings = list(
            signing_alg = "RS256",
            # RFC 9068 access tokens (typ at+jwt + client_id + jti); the default
            # "access_token" dialect emits legacy typ JWT, which the BE rejects.
            token_dialect = "rfc9068_profile",
            token_lifetime = access_token_lifetime,
            allow_offline_access = TRUE,
            skip_consent_for_verifiable_first_party_clients = TRUE
        )
    )

    fe_client <- mgmt$ensure_client(
        fe_client_name,
        settings = list(
            app_type = "regular_web",
            oidc_conformant = TRUE,
            is_first_party = TRUE,
            # The FE token exchange posts the secret in the form body (auth0r).
            token_endpoint_auth_method = "client_secret_post",
            grant_types = list("authorization_code", "refresh_token"),
            callbacks = list(paste0(fe_url, "/callback"), paste0(local_fe_url, "/callback")),
            # returnTo is APP_URL exactly (routes/auth.R logout).
            allowed_logout_urls = list(fe_url, local_fe_url),
            refresh_token = list(
                rotation_type = "rotating", # single-use + reuse detection
                expiration_type = "expiring",
                token_lifetime = 604800L, # 7d = FE absolute session lifetime
                idle_token_lifetime = 86400L # > the FE's 8h idle session window
            )
        )
    )

    mgmt_client <- mgmt$ensure_client(
        mgmt_client_name,
        settings = list(app_type = "non_interactive", grant_types = list("client_credentials"))
    )
    mgmt$ensure_client_grant(mgmt_client$client_id, mgmt$api_url, mgmt_scopes)

    mgmt$ensure_action(
        action_name,
        code = paste(readLines(action_js_path, warn = FALSE), collapse = "\n"),
        runtime = "node22"
    )

    write_app_env(tenant, fe_client, mgmt_client)
    cli_alert_success("Done. app.env written to {.path {output_env_path}} (not printed).")
    invisible()
}

# --- Output ------------------------------------------------------------------

write_app_env <- function(tenant, fe_client, mgmt_client) {
    session_key <- existing_session_key() %||% paste(as.character(openssl::rand_bytes(32)), collapse = "")
    lines <- c(
        "# plumber2-base app.env - generated by deploy/auth0/provision.R",
        "# scp to the server per deploy-server examples/README.md (App Secrets).",
        "# Shared by both services; each ignores the other's variables.",
        paste0("AUTH0_DOMAIN=", tenant),
        paste0("AUTH0_AUDIENCE=", be_url),
        paste0("AUTH0_CLAIM_NAMESPACE=", claim_namespace),
        "# front only:",
        paste0("AUTH0_CLIENT_ID=", fe_client$client_id),
        paste0("AUTH0_CLIENT_SECRET=", fe_client$client_secret),
        paste0("AUTH0_MGMT_CLIENT_ID=", mgmt_client$client_id),
        paste0("AUTH0_MGMT_CLIENT_SECRET=", mgmt_client$client_secret),
        paste0("SESSION_KEY=", session_key)
    )
    writeLines(lines, output_env_path)
    Sys.chmod(output_env_path, "0600")
    invisible()
}

# Re-runs must not rotate SESSION_KEY: it derives the refresh-token-encryption
# and CSRF keys, so rotating it invalidates every live session.
existing_session_key <- function() {
    if (!file.exists(output_env_path)) {
        return(NULL)
    }
    values <- read_env_file(output_env_path)
    key <- values["SESSION_KEY"] %||% ""
    if (nzchar(key)) key else NULL
}

read_env_file <- function(path) {
    if (!file.exists(path)) {
        cli_abort("Missing {.path {path}}.")
    }
    lines <- readLines(path, warn = FALSE)
    lines <- trimws(lines)
    lines <- sub("^export ", "", lines)
    lines <- lines[nzchar(lines) & !startsWith(lines, "#") & grepl("=", lines, fixed = TRUE)]
    keys <- sub("=.*$", "", lines)
    values <- sub("^[^=]*=", "", lines)
    values <- gsub("^[\"']|[\"']$", "", values)
    setNames(values, keys)
}

# NA-aware fallback (read_env_file lookups return NA for missing keys).
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

main()
