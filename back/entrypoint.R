# back entry point. Thin by design: validate config, run migrations, serve.
#
# Order is a safety property. Config validation (prod assertions) and migrations
# run BEFORE the port is bound, so a bad config or a failed migration exits
# non-zero without the service ever accepting a request. Migrations run here in
# the main process (not in an api_on("start") hook) precisely so a failure aborts
# startup instead of leaving a half-migrated database serving traffic.

# Resolve paths against this file so `Rscript back/entrypoint.R` works from
# the repo root as well as from back/.
local({
    args <- commandArgs(FALSE)
    this_file <- sub("^--file=", "", args[startsWith(args, "--file=")])
    setwd(dirname(normalizePath(this_file)))
})

source("R/config.R", local = FALSE)
source("R/db.R", local = FALSE)
source("../db/migrate-lib.R", local = FALSE)

config <- get_config()

# Shared DDL first: the app migrations reference users, which lives in the
# cross-app "shared" schema (see db/migrate-lib.R). The sanity assertions catch
# a broken search_path or leftover app-local shadow tables before any DDL runs.
migration_con <- db_connect(config)
tryCatch(
    {
        assert_db_sanity(migration_con)
        run_shared_ddl(migration_con, normalizePath("../db/schema-shared.sql"))
        run_migrations(migration_con, normalizePath("../db/migrations"))
    },
    finally = DBI::dbDisconnect(migration_con)
)

# The not-found fallback and the pool/mirai daemons are wired by the constructor's
# api_on("start") hook (see constructor.R), so serving is a single call here.
plumber2::api("_server.yml") |>
    plumber2::api_run(
        host = config$host,
        port = config$port,
        block = TRUE,
        showcase = FALSE
    )
