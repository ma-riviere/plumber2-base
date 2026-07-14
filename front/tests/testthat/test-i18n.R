fixture_translations <- function() {
    path <- withr::local_tempfile(
        fileext = ".json",
        .local_envir = parent.frame()
    )
    writeLines(
        paste0(
            '{"languages":["en","fr"],',
            '"translation":[',
            '{"en":"Home","fr":"Accueil"},',
            '{"en":"Save","fr":"Enregistrer"}]}'
        ),
        path
    )
    load_translations(path)
}

test_that("tr returns the requested language when the key exists", {
    translations <- fixture_translations()
    expect_equal(tr("Home", "fr", translations), "Accueil")
    expect_equal(tr("Home", "en", translations), "Home")
})

test_that("tr falls back to en when the language is missing", {
    translations <- fixture_translations()
    expect_equal(tr("Home", "de", translations), "Home")
})

test_that("tr falls back to the key itself and warns once for unknown keys", {
    translations <- fixture_translations()
    key <- "TotallyUnknownKey_i18n_test"
    expect_warning(
        result <- tr(key, "fr", translations),
        "Missing translation key"
    )
    expect_equal(result, key)
    # Same key must not warn again.
    expect_no_warning(tr(key, "fr", translations))
})
