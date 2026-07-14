# Production entrypoint. The constructor sets host/port from the environment, so
# api_run() binds to them.

# Resolve paths against this file so `Rscript front/entrypoint.R` works from
# the repo root as well as from front/ (the constructor and _server.yml use
# relative paths).
local({
    args <- commandArgs(FALSE)
    this_file <- sub("^--file=", "", args[startsWith(args, "--file=")])
    setwd(dirname(normalizePath(this_file)))
})

plumber2::api("_server.yml") |> plumber2::api_run()
