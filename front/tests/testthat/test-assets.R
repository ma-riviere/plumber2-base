# Fixture asset tree written into a temp dir (never touches the real vendor
# payloads). The icons CSS mimics bootstrap-icons' font url() references.
write_fixture <- function(root) {
    dir.create(file.path(root, "scss"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(root, "js"), recursive = TRUE, showWarnings = FALSE)
    dir.create(
        file.path(root, "vendor", "fonts"),
        recursive = TRUE,
        showWarnings = FALSE
    )

    # The app stylesheet is compiled from SCSS (not copied), so the fixture
    # provides a minimal scss/main.scss; build_assets emits css/app.css from it.
    writeLines("body { color: red; }", file.path(root, "scss", "main.scss"))
    writeLines("console.log('hi');", file.path(root, "js", "app.js"))
    writeLines("// htmx", file.path(root, "vendor", "htmx.min.js"))
    writeLines(
        paste0(
            '@font-face{font-family:"bootstrap-icons";',
            'src:url("fonts/bootstrap-icons.woff2?abc123") format("woff2"),',
            'url("fonts/bootstrap-icons.woff?abc123") format("woff")}'
        ),
        file.path(root, "vendor", "bootstrap-icons.min.css")
    )
    writeBin(
        charToRaw("woff2-bytes"),
        file.path(root, "vendor", "fonts", "bootstrap-icons.woff2")
    )
    writeBin(
        charToRaw("woff-bytes"),
        file.path(root, "vendor", "fonts", "bootstrap-icons.woff")
    )
    # Non-servable metadata that must be ignored by the build.
    writeLines("ignore me", file.path(root, "vendor", "VERSIONS.md"))
}

expected_fingerprint <- function(path, logical) {
    bytes <- readBin(path, "raw", file.info(path)$size)
    hash <- substr(paste0(openssl::sha256(bytes)), 1, 8)
    fingerprint_logical(logical, hash)
}

test_that("build_assets fingerprints every servable file and skips metadata", {
    src <- withr::local_tempdir()
    dist <- withr::local_tempdir()
    write_fixture(src)

    manifest <- build_assets(src, dist)

    expect_setequal(
        names(manifest),
        c(
            "css/app.css",
            "js/app.js",
            "vendor/htmx.min.js",
            "vendor/bootstrap-icons.min.css",
            "vendor/fonts/bootstrap-icons.woff",
            "vendor/fonts/bootstrap-icons.woff2"
        )
    )
    for (logical in names(manifest)) {
        expect_match(basename(manifest[[logical]]), "\\.[0-9a-f]{8}\\.[a-z0-9]+$")
        expect_true(file.exists(file.path(dist, manifest[[logical]])))
    }
    expect_true(file.exists(file.path(dist, "manifest.json")))
})

test_that("manifest hashes match the source content", {
    src <- withr::local_tempdir()
    dist <- withr::local_tempdir()
    write_fixture(src)

    manifest <- build_assets(src, dist)

    # css/app.css is compiled from SCSS, so its hash covers the generated CSS,
    # not a source file; only the copied-verbatim assets are checked here.
    for (logical in c(
        "js/app.js",
        "vendor/fonts/bootstrap-icons.woff2"
    )) {
        expect_equal(
            manifest[[logical]],
            expected_fingerprint(file.path(src, logical), logical)
        )
    }
})

test_that("bootstrap-icons font urls are rewritten to fingerprinted names", {
    src <- withr::local_tempdir()
    dist <- withr::local_tempdir()
    write_fixture(src)

    manifest <- build_assets(src, dist)

    icons_out <- file.path(dist, manifest[["vendor/bootstrap-icons.min.css"]])
    css <- readLines(icons_out, warn = FALSE) |> paste(collapse = "\n")

    woff2_name <- basename(manifest[["vendor/fonts/bootstrap-icons.woff2"]])
    woff_name <- basename(manifest[["vendor/fonts/bootstrap-icons.woff"]])
    expect_true(grepl(paste0("fonts/", woff2_name), css, fixed = TRUE))
    expect_true(grepl(paste0("fonts/", woff_name), css, fixed = TRUE))
    # Original query-string references must be gone.
    expect_false(grepl("?abc123", css, fixed = TRUE))
    expect_false(grepl("bootstrap-icons.woff2?", css, fixed = TRUE))

    # The icons CSS hash must cover the rewritten content it now serves.
    embedded_hash <- sub("^.*\\.([0-9a-f]{8})\\.css$", "\\1", basename(icons_out))
    bytes <- readBin(icons_out, "raw", file.info(icons_out)$size)
    expect_equal(embedded_hash, substr(paste0(openssl::sha256(bytes)), 1, 8))
})

test_that("build_assets is deterministic and idempotent", {
    src <- withr::local_tempdir()
    dist_a <- withr::local_tempdir()
    dist_b <- withr::local_tempdir()
    write_fixture(src)

    manifest_a <- build_assets(src, dist_a)
    manifest_b <- build_assets(src, dist_b)
    expect_equal(manifest_a, manifest_b)

    # A stray file from a previous build is wiped (dist cleaned first).
    stray <- file.path(dist_a, "stray.txt")
    writeLines("stale", stray)
    build_assets(src, dist_a)
    expect_false(file.exists(stray))
})

test_that("asset_path resolves known logicals and rejects unknown ones", {
    src <- withr::local_tempdir()
    dist <- withr::local_tempdir()
    write_fixture(src)
    manifest <- build_assets(src, dist)

    expect_equal(asset_path(manifest, "css/app.css"), manifest[["css/app.css"]])
    expect_error(asset_path(manifest, "css/missing.css"), "Unknown asset")
})
