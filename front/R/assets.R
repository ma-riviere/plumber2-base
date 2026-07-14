# Static asset fingerprinting pipeline.
#
# build_assets() copies the served asset files (js / vendor incl. fonts) into
# `dist/` under content-hashed names, compiles the app stylesheet from
# `assets/scss/` (via the sass R package - no Node), and writes
# `dist/manifest.json` mapping each logical path to its fingerprinted path.
# asset_path() looks a logical path up in that manifest. Fingerprinting is
# content-based, so the build is deterministic and idempotent (dist/ is wiped
# first).

build_assets <- function(src = "assets", dist = "dist") {
    stopifnot(dir.exists(src))
    if (dir.exists(dist)) {
        unlink(dist, recursive = TRUE)
    }
    dir.create(dist, recursive = TRUE)

    logical_paths <- collect_asset_files(src)
    icons_css <- "vendor/bootstrap-icons.min.css"

    manifest <- list()
    # Every file except the icons CSS is hashed from its raw bytes. The icons
    # CSS is deferred: its font url() references are rewritten to the
    # fingerprinted font names first, so its own hash covers what is served.
    for (logical in setdiff(logical_paths, icons_css)) {
        manifest[[logical]] <- fingerprint_file(
            file.path(src, logical),
            logical,
            dist
        )
    }
    if (icons_css %in% logical_paths) {
        manifest[[icons_css]] <- fingerprint_icons_css(
            file.path(src, icons_css),
            icons_css,
            dist,
            manifest
        )
    }

    # The app stylesheet is generated from SCSS (not copied), so it is compiled
    # and fingerprinted on its own after the copy-and-hash pass.
    manifest[["css/app.css"]] <- fingerprint_app_css(
        file.path(src, "scss", "main.scss"),
        dist
    )

    manifest <- manifest[order(names(manifest))]
    write_manifest(manifest, file.path(dist, "manifest.json"))
    invisible(manifest)
}

asset_path <- function(manifest, logical) {
    path <- manifest[[logical]]
    if (is.null(path)) {
        stop(sprintf("Unknown asset: '%s'", logical), call. = FALSE)
    }
    path
}

# --- helpers ---------------------------------------------------------------

# Source subdirectories whose files are copied verbatim and fingerprinted. The
# app stylesheet lives in `scss/` and is compiled separately (fingerprint_app_css).
asset_dirs <- c("js", "vendor")
# Extensions that are actually served (skips VERSIONS.md and other metadata).
servable_ext <- c("css", "js", "svg", "woff", "woff2")

collect_asset_files <- function(src) {
    files <- list.files(src, recursive = TRUE, full.names = FALSE)
    in_asset_dir <- grepl(
        paste0("^(", paste(asset_dirs, collapse = "|"), ")/"),
        files
    )
    files <- files[in_asset_dir]
    files <- files[tolower(tools::file_ext(files)) %in% servable_ext]
    sort(files)
}

fingerprint_file <- function(source_path, logical, dist) {
    bytes <- readBin(source_path, "raw", file.info(source_path)$size)
    write_fingerprinted(bytes, logical, dist)
}

# Compile assets/scss/main.scss to a minified stylesheet and fingerprint the
# result under the logical name css/app.css. @imports resolve relative to the
# entry file, so the whole scss/ tree is bundled into one output.
fingerprint_app_css <- function(scss_entry, dist) {
    css <- sass::sass(
        sass::sass_file(scss_entry),
        options = sass::sass_options(output_style = "compressed")
    )
    write_fingerprinted(charToRaw(enc2utf8(as.character(css))), "css/app.css", dist)
}

fingerprint_icons_css <- function(source_path, logical, dist, manifest) {
    css <- rawToChar(readBin(source_path, "raw", file.info(source_path)$size))
    css <- rewrite_font_urls(css, manifest)
    write_fingerprinted(charToRaw(enc2utf8(css)), logical, dist)
}

write_fingerprinted <- function(bytes, logical, dist) {
    hash <- substr(paste0(openssl::sha256(bytes)), 1, 8)
    out_logical <- fingerprint_logical(logical, hash)
    out_path <- file.path(dist, out_logical)
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    writeBin(bytes, out_path)
    out_logical
}

fingerprint_logical <- function(logical, hash) {
    parent <- dirname(logical)
    base <- basename(logical)
    stem <- tools::file_path_sans_ext(base)
    ext <- tools::file_ext(base)
    fingerprinted <- sprintf("%s.%s.%s", stem, hash, ext)
    if (parent == ".") fingerprinted else file.path(parent, fingerprinted)
}

# Point the bootstrap-icons font url()s at their fingerprinted names. Longer
# original basenames are handled first so `.woff2` is never partially matched
# by the `.woff` pattern.
rewrite_font_urls <- function(css, manifest) {
    fonts <- grep("^vendor/fonts/", names(manifest), value = TRUE)
    fonts <- fonts[order(nchar(basename(fonts)), decreasing = TRUE)]
    for (logical in fonts) {
        original <- basename(logical)
        fingerprinted <- basename(manifest[[logical]])
        pattern <- paste0("fonts/", escape_regex(original), "(\\?[^\"')]*)?")
        css <- gsub(pattern, paste0("fonts/", fingerprinted), css, perl = TRUE)
    }
    css
}

escape_regex <- function(x) {
    gsub("([][{}()*+?.\\^$|])", "\\\\\\1", x)
}

write_manifest <- function(manifest, path) {
    yyjsonr::write_json_file(
        manifest,
        path,
        opts = yyjsonr::opts_write_json(pretty = TRUE, auto_unbox = TRUE)
    )
}
