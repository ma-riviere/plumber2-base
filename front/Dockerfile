# The shared builder pre-warms public packages. This service restores its locked
# delta (including private auth0r) into the same version-independent library.
FROM ghcr.io/ma-riviere/docker-plumber2:4.6-builder AS builder

COPY front/renv.lock /build/renv.lock

RUN --mount=type=secret,id=github_pat \
    GITHUB_PAT="$(cat /run/secrets/github_pat 2>/dev/null || true)" \
    Rscript \
        -e '.libPaths(c("/opt/renv-bootstrap", .libPaths()))' \
        -e 'renv::restore(lockfile = "/build/renv.lock", \
            library = Sys.getenv("R_LIBS_SITE"), \
            prompt = FALSE, clean = TRUE)'

RUN find /opt/r-site-library -depth -type d \
        \( -name help -o -name html -o -name doc -o -name tests \) -exec rm -rf {} +

# Fingerprint assets/ into dist/ (uses openssl + yyjsonr from the library).
COPY front /app/front
RUN Rscript /app/front/scripts/build-assets.R

FROM ghcr.io/ma-riviere/docker-plumber2:4.6-runtime

COPY --from=builder /opt/r-site-library /opt/r-site-library
COPY --from=builder /app/front /app/front

# dist/ stays writable by the app user: a dev-mode (guest) container rebuilds
# the fingerprinted assets at startup; prod serves the prebuilt ones untouched.
RUN chown -R app:app /app/front/dist

USER app
WORKDIR /app/front

ENV PORT=8080

HEALTHCHECK --interval=30s --timeout=5s --retries=3 --start-period=45s \
    CMD curl -fsS "http://127.0.0.1:${PORT}/health" || exit 1

CMD ["Rscript", "entrypoint.R"]
