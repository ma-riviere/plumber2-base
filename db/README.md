# Dev database

Local Postgres 18 for development, defined in `../compose.dev.yml`. Superuser `admin` / `admin`, shared database `apps`, exposed on host `127.0.0.1:5433` (container port 5432). Data persists in the `pgdata-dev` named volume.

## Start / stop

```sh
# from the repo root
docker compose -f compose.dev.yml up -d --wait   # start, block until healthy
docker compose -f compose.dev.yml down           # stop (keeps the volume)
docker compose -f compose.dev.yml down -v        # stop and wipe data
```

## Provision app roles and schemas

`dev-init.sql` mirrors production provisioning: it creates a login role + owned schema per app (`plumber_base`, `spike`), pins each role's `search_path` to its schema in database `apps`, revokes PUBLIC's rights on schema `public`, and provisions the cross-app `shared` schema (owned by NOLOGIN role `shared`, app-role membership, two-schema search_path `plumber_base, shared`) that holds users/datasets/models, shared with shiny-base in production. It is idempotent (safe to re-run).

```sh
# from the repo root, after the database is up
docker compose -f compose.dev.yml exec -T postgres psql -U admin -d apps -f - < db/dev-init.sql
```

## Verify

```sh
# search_path is pinned for the app role
docker compose -f compose.dev.yml exec -T postgres psql -U plumber_base -d apps -tAc 'SHOW search_path'   # -> plumber_base, shared
```

App services connect with the per-app role, not `admin` (see each service's `.Renviron.example`): `PGHOST=127.0.0.1 PGPORT=5433 PGDATABASE=apps PGUSER=plumber_base PGPASSWORD=plumber_base`.
