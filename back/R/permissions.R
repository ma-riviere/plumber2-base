# RBAC: load permissions.yaml and map Auth0 roles to API scopes. The yaml is
# validated at load (i.e. at service startup) so a typo in a scope name aborts
# the boot instead of silently granting nothing.

load_permissions <- function(path = "permissions.yaml") {
    permissions <- yaml::read_yaml(path)
    scopes <- unlist(permissions$scopes, use.names = FALSE)
    if (length(scopes) == 0) {
        cli::cli_abort("{.file {path}} must declare a non-empty {.field scopes} list.")
    }
    for (role in names(permissions$roles)) {
        granted <- unlist(permissions$roles[[role]], use.names = FALSE)
        unknown <- setdiff(setdiff(granted, "*"), scopes)
        if (length(unknown)) {
            cli::cli_abort("Role {.val {role}} grants unknown scope{?s} {.val {unknown}} in {.file {path}}.")
        }
    }
    if (!identical(permissions$default_role, NULL) && !permissions$default_role %in% names(permissions$roles)) {
        cli::cli_abort("{.field default_role} {.val {permissions$default_role}} is not a defined role.")
    }
    key_safe <- unlist(permissions$key_safe_scopes, use.names = FALSE)
    if (length(setdiff(key_safe, scopes))) {
        cli::cli_abort("{.field key_safe_scopes} contains scopes not in {.field scopes}.")
    }
    list(
        scopes = scopes,
        roles = lapply(permissions$roles, function(x) unlist(x, use.names = FALSE)),
        default_role = permissions$default_role,
        key_safe_scopes = key_safe %||% character()
    )
}

# Union of the scopes granted by `roles`. Unknown roles are dropped; no (known)
# roles at all falls back to default_role (documented parity choice, see yaml).
scopes_for_roles <- function(roles, permissions) {
    roles <- roles[roles %in% names(permissions$roles)]
    if (length(roles) == 0 && !is.null(permissions$default_role)) {
        roles <- permissions$default_role
    }
    granted <- unlist(
        lapply(roles, function(role) {
            defs <- permissions$roles[[role]]
            if (identical(defs, "*")) permissions$scopes else defs
        }),
        use.names = FALSE
    )
    sort(unique(granted))
}
