# front

Server-rendered htmx + Bootstrap 5 front-end consuming the back API: Home (datasets: upload, filter, rename, delete), Explore (paginated preview), Model (async fit with polling, saved models), Admin, Account (API keys), profile modal. Auth0 login (OIDC code + PKCE) with server-side sessions in Postgres; guest mode via `BYPASS_AUTH=true`.

## Run it locally

Prereqs: dev Postgres up + roles applied, plus a running back service (root `README.md`). Config comes from environment variables: copy `.Renviron.example` to `.Renviron` (`BYPASS_AUTH=true` needs no Auth0 values).

```sh
Rscript front/entrypoint.R    # binds :8080
```

Static assets are vendored under `assets/` and fingerprinted into the gitignored `dist/` by `scripts/build-assets.R`; dev startup rebuilds them, the prod image ships them prebuilt. The app stylesheet is authored as SCSS in `assets/scss/` (customization of the vendored flatly build) and compiled to `dist/css/app.css` by the same build step via the `sass` R package (no Node).

## Test

```sh
Rscript -e 'testthat::test_dir("front/tests/testthat")'    # from the repo root; needs the dev Postgres
```

Integration tests run against webfakes fakes (backend, Auth0, Management API); UI snapshot tests use testthat edition 3 (`NOT_CRAN` is set in `setup.R`). Test-only Suggests are not recorded in `renv.lock` (root README caveat).

## Deploy

Image `ghcr.io/ma-riviere/plumber2-base-front`, built by CI from `front/Dockerfile` with the REPO ROOT as build context. It starts from the paired `docker-plumber2:4.6-builder` and `:4.6-runtime` images; the builder restores the exact lockfile into `/opt/r-site-library` with `clean = TRUE`, prebuilds `dist/`, and the runtime contains no compiler toolchain. The restore needs the `github_pat` BuildKit secret for the private auth0r package. Runtime env on the server: `db.env` (sessions live in the shared app schema) + `app.env` (Auth0 client + mgmt credentials, SESSION_KEY) + `BACKEND_URL`/`APP_URL`/`BACKEND_PUBLIC_URL` - see the comments in `deploy/compose.yml` and `deploy/README.md`.
