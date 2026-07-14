# Not a CRAN package: without this, bare test_dir() runs skip snapshot tests
# (testthat's on-CRAN heuristic treats non-package runs as CRAN).
Sys.setenv(NOT_CRAN = "true")

# Source the plain-function helpers under R/, resolving the directory whether
# tests run from front/tests/testthat (testthat default) or the repo root.
r_dir <- NULL
for (candidate in c("../../R", "front/R", "R")) {
    if (file.exists(file.path(candidate, "assets.R"))) {
        r_dir <- candidate
        break
    }
}
if (is.null(r_dir)) {
    stop("Could not locate front/R helpers")
}

# Source helpers into the GLOBAL env, mirroring how the constructor loads them in
# production: this keeps them in scope both for the unit tests here and for the
# route-file handlers parsed by the in-process integration api (test-routes.R).
fe_helpers <- c(
    "assets.R",
    "i18n.R",
    "config.R",
    "session.R",
    "csrf.R",
    "auth0.R",
    "mgmt.R",
    "gate.R",
    "render.R",
    "backend_client.R",
    "ui.R",
    "ui_home.R",
    "ui_explore.R",
    "ui_model.R",
    "ui_admin.R",
    "ui_account.R",
    "ui_profile.R",
    "app.R"
)
for (helper in fe_helpers) {
    sys.source(file.path(r_dir, helper), envir = globalenv())
}

# translations.json lives alongside the assets, one level up from R/.
translations_path <- file.path(dirname(r_dir), "assets", "translations.json")

# Migration runner, used to migrate the per-test scratch schemas (users table).
sys.source(file.path(dirname(r_dir), "..", "db", "migrate-lib.R"), envir = globalenv())
