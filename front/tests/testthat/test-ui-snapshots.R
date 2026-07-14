# Snapshot tests on the key partial builders (pure functions: fixture data in,
# markup out). Review snapshot diffs like code diffs: they ARE the UI contract.

testthat::local_edition(3)

snap_translations <- load_translations(translations_path)

snap <- function(html) {
    # xml2 pretty-printing would reorder nothing but obscure diffs; raw HTML
    # with the builder's own structure is the most reviewable form.
    cat(gsub("><", ">\n<", html, fixed = TRUE))
}

fixture_dataset <- list(
    id = 1L,
    name = "cars",
    description = "Speed and stopping distances",
    n_rows = 50L,
    n_cols = 2L,
    created_at = "2026-07-01T10:00:00Z",
    updated_at = "2026-07-01T10:00:00Z"
)

fixture_model <- list(
    id = 7L,
    dataset_id = 1L,
    formula = "dist ~ speed",
    metrics = list(
        r_squared = 0.6511,
        rmse = 15.0688,
        aic = 419.157,
        summary_text = "Call:\nlm(formula = dist ~ speed, data = data)"
    ),
    created_at = "2026-07-02T10:00:00Z"
)

test_that("dataset row (home and explore contexts) and inline-edit form", {
    expect_snapshot(snap(dataset_row_html(fixture_dataset, "en", snap_translations)))
    expect_snapshot(snap(dataset_row_html(fixture_dataset, "en", snap_translations, context = "explore")))
    expect_snapshot(snap(dataset_row_edit_html(fixture_dataset, "en", snap_translations, error = "Nope")))
})

test_that("home data panel (empty state)", {
    expect_snapshot(snap(home_data_panel(list(), "en", snap_translations)))
})

test_that("explore preview with pagination state", {
    preview <- list(
        n_rows = 50L,
        offset = 10L,
        columns = list("speed", "dist"),
        rows = list(list(speed = 4, dist = 2), list(speed = 4, dist = 10))
    )
    expect_snapshot(snap(preview_html(1L, preview, "en", snap_translations)))
})

test_that("model polling and result fragments", {
    expect_snapshot(snap(job_polling_fragment("job-1", 1L, "en", snap_translations)))
    expect_snapshot(snap(model_result_fragment(fixture_model, "en", snap_translations)))
})

test_that("model toolbar states (idle, active model, fitting)", {
    expect_snapshot(snap(model_toolbar_html(1L, "en", snap_translations)))
    expect_snapshot(snap(model_toolbar_html(1L, "en", snap_translations, active_model_id = 7L, oob = TRUE)))
    expect_snapshot(snap(model_toolbar_html(1L, "en", snap_translations, fitting = TRUE)))
})

test_that("saved models sidebar (active highlight, empty oob variant)", {
    expect_snapshot(snap(saved_models_html(list(fixture_model), 1L, "en", snap_translations, active_model_id = 7L)))
    expect_snapshot(snap(saved_models_html(list(), 1L, "en", snap_translations, oob = TRUE)))
})

test_that("api keys table and one-time secret alert", {
    keys <- list(
        list(
            id = 9L,
            name = "ci",
            key_prefix = "pbk_abcd",
            scopes = list("write:datasets"),
            last_used_at = NULL,
            expires_at = NULL,
            revoked = FALSE,
            created_at = "2026-07-01T00:00:00Z"
        ),
        list(
            id = 8L,
            name = "old",
            key_prefix = "pbk_dead",
            scopes = list(),
            last_used_at = "2026-07-01T00:00:00Z",
            expires_at = NULL,
            revoked = TRUE,
            created_at = "2026-06-01T00:00:00Z"
        )
    )
    expect_snapshot(snap(keys_table_html(keys, "en", snap_translations)))
    created <- list(id = 10L, name = "ci", secret = "pbk_secret", key_prefix = "pbk_secr", scopes = list())
    expect_snapshot(snap(key_created_html(created, "en", snap_translations)))
})

test_that("profile modal content", {
    auth <- list(
        user_id = 2L,
        sub = "auth0|fe-user",
        email = "user@example.test",
        nickname = "tester",
        picture = "https://cdn.example.test/p.png",
        roles = list("user"),
        is_guest = FALSE
    )
    expect_snapshot(snap(profile_modal_content(auth, "en", snap_translations)))
})

test_that("admin requests table", {
    items <- list(list(
        service = "back",
        method = "GET",
        path = "/v1/datasets",
        status = 200L,
        n = 42L,
        avg_ms = 12.5,
        max_ms = 40L
    ))
    expect_snapshot(snap(admin_content("requests", list(items = items), 24L, "all", "en", snap_translations)))
})

test_that("admin user card and role modal", {
    user <- list(
        id = 2L,
        auth0_sub = "auth0|u2",
        email = "dev@example.com",
        nickname = "dev",
        is_guest = FALSE,
        created_at = "2026-07-01T00:00:00Z",
        last_seen_at = "2026-07-05T00:00:00Z",
        n_datasets = 3L,
        n_models = 1L,
        n_api_keys = 0L,
        role = "dev"
    )
    roles <- list(
        list(id = "rol_admin", name = "admin", description = "Administrator", in_yaml = TRUE),
        list(id = "rol_dev", name = "dev", description = "Developer", in_yaml = TRUE),
        list(id = "rol_beta", name = "beta", description = "Beta tester", in_yaml = FALSE)
    )
    expect_snapshot(snap(admin_user_card_html(user, "en", snap_translations, can_manage_roles = TRUE)))
    expect_snapshot(snap(render_tags(admin_role_modal_html(user, roles, "user", "en", snap_translations))))
})
