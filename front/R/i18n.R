# Interface translation helpers.
#
# translations.json keeps the shiny-base layout (a `languages` list plus a
# `translation` table of parallel per-language columns) so future diffs against
# that app stay trivial. The English source string doubles as the lookup key.
# load_translations() indexes it into one named character vector per language;
# tr() resolves a key with an en -> key fallback and warns once per missing key.

load_translations <- function(path) {
    data <- yyjsonr::read_json_file(path)
    languages <- data$languages
    table <- data$translation
    index <- lapply(languages, function(language) {
        stats::setNames(table[[language]], table[["en"]])
    })
    names(index) <- languages
    structure(list(languages = languages, index = index), class = "translations")
}

tr <- function(key, lang, translations) {
    value <- lookup_key(translations$index[[lang]], key)
    if (!is.null(value)) {
        return(value)
    }
    fallback <- lookup_key(translations$index[["en"]], key)
    if (!is.null(fallback)) {
        return(fallback)
    }
    warn_missing_key(key)
    key
}

# --- helpers ---------------------------------------------------------------

lookup_key <- function(dictionary, key) {
    if (is.null(dictionary) || !key %in% names(dictionary)) {
        return(NULL)
    }
    value <- unname(dictionary[[key]])
    if (is.na(value)) NULL else value
}

i18n_warned <- new.env(parent = emptyenv())

warn_missing_key <- function(key) {
    if (isTRUE(i18n_warned[[key]])) {
        return(invisible())
    }
    i18n_warned[[key]] <- TRUE
    warning(sprintf("Missing translation key: '%s'", key), call. = FALSE)
}
