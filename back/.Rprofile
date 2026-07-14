source("renv/activate.R")
# Immutable dated PPM snapshot (same date as shiny-base's profiles): rolling
# /latest URLs rot — exact pinned versions vanish on the next upstream bump and
# renv::restore() fails. Set AFTER renv activation, which would otherwise
# reinstate the lockfile's recorded repos. PPM serves the right per-distro
# binary from the generic dated URL via the User-Agent below (Rscript's
# default UA otherwise gets source packages).
local({
    ppm <- "https://packagemanager.posit.co/cran/2026-07-09"
    options(
        repos = c(PPM = ppm),
        HTTPUserAgent = sprintf(
            "R/%s R (%s)",
            getRversion(),
            paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
        )
    )
})
