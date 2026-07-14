#* All users with their resource counts and current Auth0 role. Powers the FE
#* admin panel. Role lookups go through the short mgmt cache (back/R/mgmt.R)
#* and degrade to null when the management client is unconfigured/unreachable
#* or the user has no explicit role (= the default role applies).
#* @get /v1/admin/users
#* @serializer json
#* @noDoc
function(datastore, response) {
    require_scope(datastore, "view:admin")
    request_principal(datastore, response)
    rows <- db_admin_users(app_pool())
    role_for <- function(row) {
        if (isTRUE(row$is_guest) || is.na(row$auth0_sub) || !mgmt_available()) {
            return(NA_character_)
        }
        roles <- tryCatch(user_roles_cached(row$auth0_sub), error = function(e) character())
        if (length(roles) == 0) NA_character_ else roles[[1]]
    }
    list(
        items = lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, ]
            list(
                id = jsonlite::unbox(as.integer(row$id)),
                auth0_sub = jsonlite::unbox(row$auth0_sub),
                email = jsonlite::unbox(row$email),
                nickname = jsonlite::unbox(row$nickname),
                is_guest = jsonlite::unbox(row$is_guest),
                created_at = jsonlite::unbox(format_time_or_null(row$created_at)),
                last_seen_at = jsonlite::unbox(format_time_or_null(row$last_seen_at)),
                n_datasets = jsonlite::unbox(as.integer(row$n_datasets)),
                n_models = jsonlite::unbox(as.integer(row$n_models)),
                n_api_keys = jsonlite::unbox(as.integer(row$n_api_keys)),
                role = jsonlite::unbox(role_for(row))
            )
        })
    )
}

#* The tenant's Auth0 roles, flagged with whether permissions.yaml actually
#* maps them to scopes (an out-of-yaml role grants nothing but default scopes).
#* @get /v1/admin/roles
#* @serializer json
#* @noDoc
function(datastore, response) {
    require_scope(datastore, "view:admin")
    request_principal(datastore, response)
    permissions <- app_permissions()
    roles <- mgmt_client()$list_roles()
    list(
        default_role = jsonlite::unbox(permissions$default_role %||% NA_character_),
        items = lapply(roles, function(role) {
            list(
                id = jsonlite::unbox(role$id),
                name = jsonlite::unbox(role$name),
                description = jsonlite::unbox(role$description %||% NA_character_),
                in_yaml = jsonlite::unbox(role$name %in% names(permissions$roles))
            )
        })
    )
}

#* Set a user's Auth0 role (single-role model: existing roles are replaced;
#* an empty role_id clears them, falling back to the default role). Guests
#* have no Auth0 identity, and removing your own admin role is refused (the
#* lockout guard). Remove+assign is not transactional: a failed assign leaves
#* the user role-less, which maps to the default role - visible, not harmful.
#* @param id:integer The user id
#* @body role_id:string The Auth0 role id to assign; "" clears to the default role
#* @put /v1/admin/users/<id:integer>/role
#* @serializer json
#* @noDoc
function(id, body, datastore, response) {
    require_scope(datastore, "manage:admin:roles")
    principal <- request_principal(datastore, response)
    target <- get_user_by_id(app_pool(), as.integer(id))
    if (is.null(target)) {
        reqres::abort_not_found("no such user")
    }
    if (isTRUE(target$is_guest) || is.na(target$auth0_sub)) {
        reqres::abort_http_problem(422L, detail = "guest users have no Auth0 identity")
    }
    client <- mgmt_client()
    role_id <- trimws(body$role_id %||% "")
    new_role <- if (nzchar(role_id)) {
        tryCatch(
            client$get_role(role_id),
            error = function(e) reqres::abort_http_problem(422L, detail = "unknown role id")
        )
    }
    current <- client$get_user_roles(target$auth0_sub, full = TRUE)
    current_names <- vapply(current, function(role) role$name, character(1))
    if (
        identical(principal$user$auth0_sub, target$auth0_sub) &&
            "admin" %in% current_names &&
            !identical(new_role$name, "admin")
    ) {
        reqres::abort_http_problem(409L, detail = "refusing to remove your own admin role")
    }
    if (length(current) > 0) {
        client$remove_user_roles(target$auth0_sub, vapply(current, function(role) role$id, character(1)))
    }
    if (!is.null(new_role)) {
        client$assign_user_roles(target$auth0_sub, role_id)
    }
    invalidate_user_roles(target$auth0_sub)
    list(
        user_id = jsonlite::unbox(as.integer(id)),
        role = jsonlite::unbox(new_role$name %||% NA_character_),
        role_id = jsonlite::unbox(if (nzchar(role_id)) role_id else NA_character_)
    )
}

#* FE server-side session store overview (storr namespaces + entry counts).
#* Refined in Phase 7 when the FE admin page consumes it.
#* @get /v1/admin/sessions
#* @serializer json
#* @noDoc
function(datastore, response) {
    require_scope(datastore, "view:admin")
    request_principal(datastore, response)
    rows <- db_admin_sessions(app_pool())
    list(
        items = lapply(seq_len(nrow(rows)), function(i) {
            list(
                namespace = jsonlite::unbox(rows$namespace[i]),
                n = jsonlite::unbox(as.integer(rows$n[i]))
            )
        })
    )
}

#* Request-log statistics over a time window (replaces shiny-base's OTel traces
#* tab, an accepted v1 scope exception).
#* @query hours:integer Window size in hours (default 24, max 720)
#* @get /v1/admin/requests
#* @serializer json
#* @noDoc
function(query, datastore, response) {
    require_scope(datastore, "view:admin")
    request_principal(datastore, response)
    hours <- min(query$hours %||% 24L, 720L)
    rows <- db_admin_requests(app_pool(), hours)
    list(
        window_hours = jsonlite::unbox(as.integer(hours)),
        items = lapply(seq_len(nrow(rows)), function(i) {
            row <- rows[i, ]
            list(
                service = jsonlite::unbox(row$service),
                method = jsonlite::unbox(row$method),
                path = jsonlite::unbox(row$path),
                status = jsonlite::unbox(as.integer(row$status)),
                n = jsonlite::unbox(as.integer(row$n)),
                avg_ms = jsonlite::unbox(as.numeric(row$avg_ms)),
                max_ms = jsonlite::unbox(as.integer(row$max_ms))
            )
        })
    )
}
