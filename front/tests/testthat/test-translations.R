test_that("translations.json parses with en and fr as the languages", {
    data <- yyjsonr::read_json_file(translations_path)
    expect_setequal(data$languages, c("en", "fr"))
    expect_s3_class(data$translation, "data.frame")
    expect_true(all(c("en", "fr") %in% names(data$translation)))
})

test_that("every translation entry has a non-empty en and fr value", {
    data <- yyjsonr::read_json_file(translations_path)
    for (language in c("en", "fr")) {
        values <- data$translation[[language]]
        expect_false(any(is.na(values)), info = language)
        expect_false(any(!nzchar(values)), info = language)
    }
})

test_that("load_translations indexes both languages and resolves keys", {
    translations <- load_translations(translations_path)
    expect_setequal(translations$languages, c("en", "fr"))
    expect_equal(tr("Home", "fr", translations), "Accueil")
    expect_equal(tr("Home", "en", translations), "Home")
})
