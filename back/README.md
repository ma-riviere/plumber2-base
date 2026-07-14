# back

JSON REST API of the plumber-base app: datasets (CRUD, multipart CSV upload, paginated preview, CSV download), lm model fits as async jobs (mirai) with polling, self-managed API keys, admin endpoints, per-principal rate limiting. Auth: Auth0 access tokens (RS256, RFC 9068) or API keys; RBAC in `permissions.yaml`. OpenAPI docs at `/__docs__/`.

## Run

Prereqs: dev Postgres up + roles applied (root `README.md` steps 1-2). Config comes from environment variables: copy `.Renviron.example` to `.Renviron` and fill in values (`BYPASS_AUTH=true` skips the Auth0 ones).

```sh
Rscript back/entrypoint.R    # binds :8081; validates config and applies db/ migrations BEFORE binding
curl localhost:8081/health
```

Startup is fail-fast: prod config assertions and migrations abort the process before the port binds; `ENVIRONMENT=prod` with `BYPASS_AUTH=true` refuses to start.

## Test

```sh
Rscript -e 'testthat::test_dir("back/tests/testthat")'    # from the repo root; needs the dev Postgres
```

Auth tests run against local RSA/JWKS fixtures (no network).

## Deploy

Image `ghcr.io/ma-riviere/plumber2-base-back`, built by CI from `back/Dockerfile` with the REPO ROOT as build context. It starts from the paired `docker-plumber2:4.6-builder` and `:4.6-runtime` images; the builder restores the exact lockfile into `/opt/r-site-library` with `clean = TRUE`, and the runtime contains no compiler toolchain. The image ships `db/` for startup migrations; the restore needs the `github_pat` BuildKit secret for the private package auth0r. Runtime env on the server: `db.env` + `app.env` (AUTH0_DOMAIN, AUTH0_AUDIENCE, AUTH0_CLAIM_NAMESPACE) - see the comments in `deploy/compose.yml` and `deploy/README.md`.
