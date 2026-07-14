# permissions.yaml loading + role->scope mapping.

test_that("the shipped permissions.yaml loads and is internally consistent", {
    permissions <- load_permissions(file.path(BACK_DIR, "permissions.yaml"))

    expect_setequal(
        permissions$scopes,
        c("write:datasets", "write:models", "manage:keys", "view:admin", "manage:admin:roles")
    )
    expect_true(all(c("admin", "dev", "user") %in% names(permissions$roles)))
    expect_equal(permissions$default_role, "user")
    # Key-management and admin scopes must never be key-grantable.
    expect_false(any(c("manage:keys", "view:admin", "manage:admin:roles") %in% permissions$key_safe_scopes))
    # Role management is admin-only: dev must NOT carry it (shiny-base parity).
    expect_false("manage:admin:roles" %in% permissions$roles$dev)
})

test_that("scopes_for_roles maps roles, wildcards and defaults", {
    permissions <- load_permissions(file.path(BACK_DIR, "permissions.yaml"))

    expect_setequal(scopes_for_roles("admin", permissions), permissions$scopes)
    expect_setequal(scopes_for_roles("user", permissions), c("write:datasets", "write:models", "manage:keys"))
    expect_setequal(
        scopes_for_roles(c("user", "dev"), permissions),
        c("write:datasets", "write:models", "manage:keys", "view:admin")
    )
    # No roles / only unknown roles -> default_role (documented parity choice).
    expect_setequal(scopes_for_roles(character(), permissions), scopes_for_roles("user", permissions))
    expect_setequal(scopes_for_roles("bogus", permissions), scopes_for_roles("user", permissions))
})

test_that("a permissions file with a typoed scope is rejected at load", {
    bad <- withr::local_tempfile(fileext = ".yaml")
    writeLines(
        c(
            "scopes:",
            "  - write:datasets",
            "roles:",
            "  user:",
            "    - write:dataset", # missing s
            "default_role: user"
        ),
        bad
    )
    expect_error(load_permissions(bad), "unknown scope")
})

test_that("an undefined default_role is rejected at load", {
    bad <- withr::local_tempfile(fileext = ".yaml")
    writeLines(
        c(
            "scopes:",
            "  - write:datasets",
            "roles:",
            "  admin: '*'",
            "default_role: user"
        ),
        bad
    )
    expect_error(load_permissions(bad), "default_role")
})
