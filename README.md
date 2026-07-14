# plumber2-base

A sandbox dual-plumber2 API demo app mirroring [shiny-base](https://github.com/ma-riviere/shiny-base).

## Features

Datasets upload, exploration, and linear model fit (async, with polling).

## Architecture

Two services, both plumber2:

- `back`: standalone JSON REST API over Postgres, Auth0 JWT + API-key auth. Live at https://plumber2-base-api.ma-riviere.com (OpenAPI docs at `/__docs__/`).
- `front`: server-rendered htmx + Bootstrap 5 app with Auth0 login, consuming the API. Live at https://plumber2-base.ma-riviere.com.

In production this app and shiny-base share the same Postgres database (`db/schema-shared.sql`). Run/test/deploy details live in `back/README.md`, `front/README.md`, `db/README.md` and `deploy/README.md`.

## Run it locally

```sh
cd deploy && docker compose up --wait
```

Production-shaped images plus a throwaway Postgres, in guest mode (no Auth0 setup). Front on :8080, back on :8081. Building the images needs a `GITHUB_PAT` with read access to the (for now) private `auth0r` package (see `deploy/README.md`).

## Local development

1. Start the dev database: `docker compose -f compose.dev.yml up -d --wait`, then apply roles/schemas once: `docker compose -f compose.dev.yml exec -T postgres psql -U admin -d apps -f - < db/dev-init.sql` (details in `db/README.md`).
2. Migrations: `Rscript db/migrate.R`. Dev seed: `Rscript db/seed-dev.R`.
3. Run the services: `Rscript back/entrypoint.R` (:8081) and `Rscript front/entrypoint.R` (:8080); `BYPASS_AUTH=true` enables guest mode without Auth0 credentials.
4. Tests: `Rscript -e 'testthat::test_dir("db/tests")'`, same for `back/tests/testthat` and `front/tests/testthat`.

renv caveat: each service's lockfile is an explicit snapshot of its `DESCRIPTION` Imports, so test-only Suggests are not recorded; after a fresh `renv::restore()`, run `renv::install()` in the service directory before running tests.

## Deployment

Push to `main` builds both images to GHCR and deploys them to the server. Manual dispatch with a previous commit SHA as `image_tag` rolls back to that exact image pair without rebuilding. Details in `deploy/README.md` and `.github/workflows/deploy.yml`.
