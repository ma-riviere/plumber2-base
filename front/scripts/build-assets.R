#!/usr/bin/env Rscript
# Thin wrapper: fingerprint front/assets into front/dist. Resolves
# paths from the script location so it runs from any working directory.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
base_dir <- dirname(dirname(normalizePath(file_arg)))

source(file.path(base_dir, "R", "assets.R"))

manifest <- build_assets(
    file.path(base_dir, "assets"),
    file.path(base_dir, "dist")
)
cat(sprintf(
    "Built %d assets -> %s\n",
    length(manifest),
    file.path(base_dir, "dist")
))
