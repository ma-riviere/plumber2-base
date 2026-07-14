# Database access for back.
#
# db_pool() backs request handlers: a pool is created once at server start and
# handlers check a connection out per request. db_connect() is a plain, single
# connection for the few places a pool cannot go (running migrations at startup).
#
# The app role's search_path is already pinned to its own schema in the database
# (see db/dev-init.sql), so nothing here qualifies object names. Postgres NOTICEs
# are demoted to keep logs clean, mirroring the migration runner.

# Shared, process-wide handles to the live pool and the validated config.
# Handlers read them through app_pool()/app_config(); the constructor and the
# lifecycle hooks (and tests) set them.
app_state <- new.env(parent = emptyenv())

app_pool <- function() {
    app_state$pool
}

set_app_pool <- function(pool) {
    app_state$pool <- pool
    invisible(pool)
}

app_config <- function() {
    app_state$config
}

set_app_config <- function(config) {
    app_state$config <- config
    invisible(config)
}

app_permissions <- function() {
    app_state$permissions
}

set_app_permissions <- function(permissions) {
    app_state$permissions <- permissions
    invisible(permissions)
}

# Recursively mark length-1 atomics as scalars for JSON output. Needed when a
# jsonb value parsed back into R lists is re-serialized by the endpoint's JSON
# serializer (which would box every length-1 vector as [x]).
unbox_scalars <- function(x) {
    if (is.list(x)) {
        return(lapply(x, unbox_scalars))
    }
    if (is.atomic(x) && length(x) == 1 && is.null(dim(x))) {
        return(jsonlite::unbox(x))
    }
    x
}

# Timestamps for JSON output: NA-safe (jsonlite::unbox(NA) serializes as null).
format_time_or_null <- function(time) {
    if (is.null(time) || is.na(time)) {
        return(NA_character_)
    }
    format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# TCP keepalives so an idle connection dropped by the DB, network, or a NAT
# gateway is detected and reaped rather than surfacing as a hung/broken query
# later. The pool then validates and replaces dead connections transparently.
DB_KEEPALIVE_ARGS <- list(
    keepalives = 1L,
    keepalives_idle = 60L,
    keepalives_interval = 10L,
    keepalives_count = 5L
)

db_pool <- function(config) {
    do.call(
        pool::dbPool,
        c(
            list(
                drv = RPostgres::Postgres(),
                host = config$db$host,
                port = config$db$port,
                dbname = config$db$dbname,
                user = config$db$user,
                password = config$db$password,
                options = "-c client_min_messages=warning"
            ),
            DB_KEEPALIVE_ARGS
        )
    )
}

db_connect <- function(config) {
    do.call(
        DBI::dbConnect,
        c(
            list(
                RPostgres::Postgres(),
                host = config$db$host,
                port = config$db$port,
                dbname = config$db$dbname,
                user = config$db$user,
                password = config$db$password,
                options = "-c client_min_messages=warning"
            ),
            DB_KEEPALIVE_ARGS
        )
    )
}

# Check a connection out of the pool, run fn(con), and always return it.
db_with_con <- function(pool, fn) {
    con <- pool::poolCheckout(pool)
    on.exit(pool::poolReturn(con))
    fn(con)
}

# Postgres text[] columns round-trip as raw array literal strings (RPostgres does
# not parse them into R vectors). Adapted from db/tests/helper-db.R; the values
# stored here (scope names) contain no commas/braces, but escaping is kept anyway.
pg_text_array_literal <- function(x) {
    if (length(x) == 0L) {
        return("{}")
    }
    escaped <- gsub('(["\\\\])', "\\\\\\1", x)
    sprintf("{%s}", paste0('"', escaped, '"', collapse = ","))
}

parse_pg_text_array <- function(literal) {
    inner <- sub("\\}$", "", sub("^\\{", "", as.character(literal)))
    if (!nzchar(inner)) {
        return(character(0))
    }
    parts <- strsplit(inner, ",", fixed = TRUE)[[1]]
    gsub('^"|"$', "", parts)
}

# TRUE if the database answers a trivial query. Used by GET /health; never throws
# so an unreachable database surfaces as a 503 rather than a 500.
db_healthcheck <- function(pool) {
    if (is.null(pool)) {
        return(FALSE)
    }
    tryCatch(
        {
            db_with_con(pool, function(con) DBI::dbGetQuery(con, "SELECT 1"))
            TRUE
        },
        error = function(e) FALSE
    )
}
