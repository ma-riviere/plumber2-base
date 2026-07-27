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

# Agent runtime for the dataset chatbot. Node and pi are pinned exactly (same
# rationale as the R image pins): the RPC protocol and the extension API are
# both 0.x surfaces, so an unpinned minor can change the wire format under us.
# Installed globally into /usr/local so the unprivileged app user can only read
# it. Node >= 22.19 is pi's floor.
ARG NODE_VERSION=22.19.0
ARG PI_VERSION=0.82.1

USER root
RUN set -eux; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
        amd64) node_arch="x64" ;; \
        arm64) node_arch="arm64" ;; \
        *) echo "unsupported architecture: $arch" >&2; exit 1 ;; \
    esac; \
    # .tar.gz, not .tar.xz: the runtime base image has gzip but no xz binary
    curl -fsSL "https://nodejs.org/dist/v${NODE_VERSION}/node-v${NODE_VERSION}-linux-${node_arch}.tar.gz" \
        -o /tmp/node.tar.gz; \
    tar -xzf /tmp/node.tar.gz -C /usr/local --strip-components=1 \
        --exclude=CHANGELOG.md --exclude=LICENSE --exclude=README.md; \
    rm /tmp/node.tar.gz; \
    npm install -g --no-fund --no-audit "@earendil-works/pi-coding-agent@${PI_VERSION}"; \
    npm cache clean --force; \
    pi --version

# The chatbot's `query` tool executes model-written SQL through this CLI, and the
# lockdown recipe (settings, then `.safe_mode`) is version-specific behaviour:
# pinned exactly and checksummed, like Node and pi above. The official Linux
# asset is glibc-dynamic, which matches this runtime.
ARG DUCKDB_VERSION=1.5.5
ARG DUCKDB_SHA256_AMD64=08c0ca117111fcede14239d0093792352befdc174218c344d232c13279643d05
ARG DUCKDB_SHA256_ARM64=02163197027a42149147364d31fa67cac82108517a4be43304a1cc226eaef07a

RUN set -eux; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
        amd64) duckdb_sha="${DUCKDB_SHA256_AMD64}" ;; \
        arm64) duckdb_sha="${DUCKDB_SHA256_ARM64}" ;; \
        *) echo "unsupported architecture: $arch" >&2; exit 1 ;; \
    esac; \
    curl -fsSL \
        "https://github.com/duckdb/duckdb/releases/download/v${DUCKDB_VERSION}/duckdb_cli-linux-${arch}.zip" \
        -o /tmp/duckdb.zip; \
    echo "${duckdb_sha}  /tmp/duckdb.zip" | sha256sum -c -; \
    unzip -q /tmp/duckdb.zip -d /tmp/duckdb; \
    install -m 0755 /tmp/duckdb/duckdb /usr/local/bin/duckdb; \
    rm -rf /tmp/duckdb.zip /tmp/duckdb; \
    duckdb --version

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
