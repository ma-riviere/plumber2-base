# deploy/

`compose.yml` is the production stack, shaped for the deploy-server platform contract (pinned GHCR tags, external `edge`/`postgres` networks, `env_file: db.env` provisioned on the server, Traefik labels, no `ports:`). Production hostnames: `plumber2-base.ma-riviere.com` (front) and `plumber2-base-api.ma-riviere.com` (back), both requiring Cloudflare's origin-pull client cert (`tls.options: cloudflare-origin-pull@file`, shipped by the platform's traefik role). App secrets (Auth0 + `SESSION_KEY`) live in a server-side `/srv/apps/plumber2-base/app.env`, delivered root:deploy 0640 by deploy-server's Ansible from the gitignored repo-root `app.env` master copy (`apps[].app_env_file` in its config.yml; contents listed in the compose comments) - referenced with `required: false` so the guest-mode local overlay works without it, while the services' prod startup assertions abort loudly if it is missing. Keep the master copy current: every deploy-server Ansible run overwrites the server-side file from it. CI (`.github/workflows/deploy.yml`) builds both images to GHCR, substitutes `IMAGE_TAG` with the commit SHA, and pushes the file with `deploy-app push-compose`; healthchecks live in the service Dockerfiles and `deploy-app deploy` gates the rollout on them via `docker compose up --wait`. Manual dispatch: an empty `image_tag` rebuilds and redeploys HEAD (e.g. after a scheduled base-image refresh); a previous commit SHA rolls back to that exact image pair without rebuilding.

`compose.override.yml` turns the same stack into a local one (merged automatically): images built from the working tree, a throwaway Postgres 18 provisioned like production (`db/dev-init.sql`), guest mode (`ENVIRONMENT=dev`, `BYPASS_AUTH=true`), loopback ports 8080 (front) / 8081 (back). The committed overlay supplies a non-secret development `SESSION_KEY` because guest sessions still sign CSRF and cookie values. The committed `deploy/db.env` holds the matching local-only credentials. Both application builds consume the `github_pat` BuildKit secret from `GITHUB_PAT`; it needs read access to `ma-riviere/auth0r`.

## Local run

```sh
export GITHUB_PAT="<read-scoped token for ma-riviere/auth0r>"
cd deploy
docker compose build            # both images, from the repo root context
docker compose up --wait        # green when both services + postgres are healthy
docker compose exec back Rscript /app/db/seed-dev.R   # optional: guest + mtcars demo dataset
docker compose down -v          # teardown, wipe the throwaway volume
```

Front: http://localhost:8080 (guest login). Back: http://localhost:8081 (`/health`, `/v1/*` under bypass auth, docs at `/__docs__/`).
