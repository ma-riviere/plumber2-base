# The shared builder pre-warms public packages. This service restores its locked
# delta (including private auth0r) into the same version-independent library.
FROM ghcr.io/ma-riviere/docker-plumber2:4.6-builder AS builder

COPY back/renv.lock /build/renv.lock

RUN --mount=type=secret,id=github_pat \
    GITHUB_PAT="$(cat /run/secrets/github_pat 2>/dev/null || true)" \
    Rscript \
        -e '.libPaths(c("/opt/renv-bootstrap", .libPaths()))' \
        -e 'renv::restore(lockfile = "/build/renv.lock", \
            library = Sys.getenv("R_LIBS_SITE"), \
            prompt = FALSE, clean = TRUE)'

RUN find /opt/r-site-library -depth -type d \
        \( -name help -o -name html -o -name doc -o -name tests \) -exec rm -rf {} +

FROM ghcr.io/ma-riviere/docker-plumber2:4.6-runtime

COPY --from=builder /opt/r-site-library /opt/r-site-library

# db/ ships with the service: the entrypoint applies ../db/migrations before
# binding the port (a failed migration exits non-zero and fails the rollout).
COPY db /app/db
COPY back /app/back

USER app
WORKDIR /app/back

ENV PORT=8081

# start-period covers R boot + migrations. `docker compose up --wait` and the
# deploy-server rollout gate on this check.
HEALTHCHECK --interval=30s --timeout=5s --retries=3 --start-period=45s \
    CMD curl -fsS "http://127.0.0.1:${PORT}/health" || exit 1

CMD ["Rscript", "entrypoint.R"]
