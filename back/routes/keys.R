#* List the caller's API keys: prefix + metadata, never the secret. Revoked keys
#* are included (flagged) so the FE account page can show history. The whole
#* /v1/keys surface is JWT-only via the constructor's auth rules (an API key
#* must never mint or revoke keys).
#* @get /v1/keys
#* @serializer json
function(datastore, response) {
    require_scope(datastore, "manage:keys")
    principal <- request_principal(datastore, response)
    rows <- DBI::dbGetQuery(
        app_pool(),
        "SELECT id, name, key_prefix, scopes, last_used_at, expires_at, revoked_at, created_at
         FROM api_keys WHERE user_id = $1 ORDER BY id DESC",
        params = list(principal$user_id)
    )
    list(
        items = lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, ]
            list(
                id = jsonlite::unbox(as.integer(row$id)),
                name = jsonlite::unbox(row$name),
                key_prefix = jsonlite::unbox(row$key_prefix),
                scopes = parse_pg_text_array(row$scopes[[1]]),
                last_used_at = jsonlite::unbox(format_time_or_null(row$last_used_at)),
                expires_at = jsonlite::unbox(format_time_or_null(row$expires_at)),
                revoked = jsonlite::unbox(!is.na(row$revoked_at)),
                created_at = jsonlite::unbox(format_time_or_null(row$created_at))
            )
        })
    )
}

#* Create an API key. The secret is returned ONCE and never stored. Scope
#* bounding: requested scopes are intersected with the caller's own scopes AND
#* the key-safe allowlist (key-management/admin scopes are never grantable).
#* NOTE: `name` is required (enforced in the handler; a `*` marker with several
#* @body tags crashes plumber2 0.2.0's file parsing, spike addendum).
#* @body name:string A name for the key (unique per user)
#* @body scopes:[string] Requested scopes (bounded, may end up narrower)
#* @body expires_at:string Optional expiry (ISO 8601)
#* @post /v1/keys
#* @serializer json
function(body, datastore, response) {
    require_scope(datastore, "manage:keys")
    principal <- request_principal(datastore, response)
    permissions <- app_permissions()

    name <- trimws(body$name %||% "")
    if (!nzchar(name)) {
        reqres::abort_bad_request("'name' is required")
    }
    granted <- intersect(
        intersect(unlist(body$scopes, use.names = FALSE) %||% character(), principal$scopes),
        permissions$key_safe_scopes
    )
    expires_at <- parse_iso_time(body$expires_at, "expires_at")

    duplicate <- DBI::dbGetQuery(
        app_pool(),
        "SELECT 1 FROM api_keys WHERE user_id = $1 AND name = $2",
        params = list(principal$user_id, name)
    )
    if (nrow(duplicate) > 0) {
        reqres::abort_http_problem(409L, detail = "a key with this name already exists")
    }

    key <- create_api_key(app_pool(), principal$user_id, name, granted, expires_at)
    response$status <- 201L
    response$set_header("Location", sprintf("/v1/keys/%d", as.integer(key$id)))
    list(
        id = jsonlite::unbox(as.integer(key$id)),
        name = jsonlite::unbox(name),
        secret = jsonlite::unbox(key$secret),
        key_prefix = jsonlite::unbox(key$key_prefix),
        scopes = key$scopes
    )
}

#* Revoke an API key. Idempotent from the client's perspective: revoking an
#* already-revoked or unknown key of your own yields 404.
#* @param id:integer The key id
#* @delete /v1/keys/<id:integer>
#* @serializer json
function(id, datastore, response) {
    require_scope(datastore, "manage:keys")
    principal <- request_principal(datastore, response)
    if (!revoke_api_key(app_pool(), principal$user_id, id)) {
        reqres::abort_not_found("no such active key")
    }
    response$status <- 204L
    plumber2::Break
}
