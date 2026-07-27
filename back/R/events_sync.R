# Auth0 Events API sync: catches bans and deletions performed OUTSIDE this app
# (typically straight from the Auth0 dashboard) and mirrors them into
# users.status, the cross-app enforcement column. This poller lives only here:
# shiny-base shares the column but not this machinery.
#
# Shape of the upstream endpoint (GET /api/v2/events, GA 2026-04, verified live
# against the tenant): it is a Server-Sent Events STREAM, not a paginated JSON
# collection. It accepts exactly three query params - `from` (opaque cursor,
# resumes AFTER it), `from_timestamp` (mutually exclusive with `from`) and a
# repeatable `event_type` filter - and emits `event: <type>` frames plus
# `event: offset-only` cursor heartbeats, each carrying `data.offset`. Omitting
# the cursor starts at the latest events, which is exactly the "do not replay
# history" behaviour wanted on first run. No tenant Event Stream needs to exist.
#
# Because the stream stays open for minutes and this is a single-threaded R
# process, each tick opens the stream, drains what is already buffered in a
# bounded window WITHOUT blocking the event loop (non-blocking connection +
# later chain), then closes it. Auth0's concurrent-connection cap (1 on the free
# tier) makes a permanently held stream a poor fit anyway.

AUTH0_EVENTS_CURSOR_KEY <- "auth0_events_cursor"
# Written on the very first cursor-less tick and used as the stream start until
# the first real offset is checkpointed: without it, a tick whose drain window
# sees no frame would restart at "latest" and silently drop everything emitted
# in between.
AUTH0_EVENTS_FROM_TIMESTAMP_KEY <- "auth0_events_from_timestamp"
EVENTS_SYNC_INTERVAL_SECONDS <- 300
# Per-tick read window and the gap between two non-blocking reads within it.
EVENTS_DRAIN_SECONDS <- 5
EVENTS_DRAIN_POLL_SECONDS <- 0.2
EVENTS_TYPES <- c("user.updated", "user.deleted")

events_state <- new.env(parent = emptyenv())

reset_events_state <- function() {
    rm(list = ls(events_state), envir = events_state)
    invisible()
}

log_events_sync <- function(fmt, ...) {
    if (!isTRUE(getOption("back.quiet_events_log"))) {
        cat(sprintf(paste0("[back] events sync: ", fmt, "\n"), ...), file = stderr())
    }
    invisible()
}

# --- cursor persistence --------------------------------------------------------

sync_state_get <- function(pool, key) {
    row <- DBI::dbGetQuery(pool, "SELECT value FROM sync_state WHERE key = $1", params = list(key))
    if (nrow(row) == 0) NULL else row$value[[1]]
}

sync_state_delete <- function(pool, key) {
    DBI::dbExecute(pool, "DELETE FROM sync_state WHERE key = $1", params = list(key))
    invisible()
}

sync_state_set <- function(pool, key, value) {
    DBI::dbExecute(
        pool,
        "INSERT INTO sync_state (key, value) VALUES ($1, $2)
         ON CONFLICT (key) DO UPDATE SET value = excluded.value, updated_at = now()",
        params = list(key, value)
    )
    invisible()
}

# --- event handling ------------------------------------------------------------

# Apply one CloudEvent envelope (the `event` member of an SSE frame's data).
# Every branch is an idempotent UPDATE keyed on auth0_sub; subs with no local
# user row are skipped silently (the tenant is shared with other apps).
apply_auth0_event <- function(pool, event) {
    object <- event$data$object
    sub <- object$user_id
    if (!is.character(sub) || length(sub) != 1L || is.na(sub) || !nzchar(sub)) {
        return(invisible(FALSE))
    }
    row <- DBI::dbGetQuery(pool, "SELECT id, status FROM users WHERE auth0_sub = $1", params = list(sub))
    if (nrow(row) == 0) {
        return(invisible(FALSE))
    }
    new_status <- if (identical(event$type, "user.deleted")) {
        "deleted"
    } else if (isTRUE(object$blocked)) {
        "banned"
    } else if (identical(row$status[[1]], "banned")) {
        # An unblock only lifts a ban: a 'deleted' row must never be resurrected.
        "active"
    } else {
        return(invisible(FALSE))
    }
    set_user_status(pool, row$id[[1]], new_status)
    log_events_sync("%s -> %s", event$type, new_status)
    invisible(TRUE)
}

# One SSE frame. The offset is checkpointed only AFTER the frame's event was
# applied (or was an intentional no-op): checkpointing first would permanently
# skip a ban whose UPDATE failed. Returns one of
#   "continue" - frame handled, keep draining
#   "stop"     - stop draining, the offsets reached so far are safe to persist
#   "abort"    - stop draining WITHOUT persisting, so the next tick retries from
#                the last known-good offset
#   "reset"    - the cursor itself is bad; drop it and restart fresh next tick
handle_events_frame <- function(pool, frame) {
    if (identical(frame$type, "error")) {
        detail <- substr(frame$data %||% "", 1L, 200L)
        log_events_sync("stream error frame: %s", detail)
        return(if (grepl("invalid_cursor", detail, fixed = TRUE)) "reset" else "stop")
    }
    payload <- parse_events_data(frame$data)
    if (is.null(payload)) {
        return("continue")
    }
    if (!is.null(payload$event)) {
        applied <- tryCatch(
            {
                apply_auth0_event(pool, payload$event)
                TRUE
            },
            error = function(e) {
                log_events_sync("handler failed, not checkpointing: %s", conditionMessage(e))
                FALSE
            }
        )
        if (!applied) {
            return("abort")
        }
    }
    if (is.character(payload$offset) && nzchar(payload$offset)) {
        events_state$offset <- payload$offset
    }
    "continue"
}

# yyjsonr echoes the offending text to stdout on a parse failure even inside
# tryCatch, and this text is remote input: capture it away from the logs.
parse_events_data <- function(data) {
    if (!is.character(data) || length(data) != 1L || !nzchar(data)) {
        return(NULL)
    }
    utils::capture.output(
        parsed <- tryCatch(
            yyjsonr::read_json_str(data, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE),
            error = function(e) NULL
        )
    )
    parsed
}

# --- stream access -------------------------------------------------------------

# `from` (opaque cursor) and `from_timestamp` are mutually exclusive upstream.
# The bearer token is the shared Management client's (auth0r caches it to
# expiry). The explicit timeouts matter: opening the stream is the one blocking
# step of the tick, so a hung Auth0 endpoint must not pin the single-threaded
# loop.
events_open_stream <- function(config, cursor, from_timestamp) {
    mgmt <- mgmt_client(config)
    req <- httr2::request(mgmt$api_url) |>
        httr2::req_url_path_append("events") |>
        httr2::req_auth_bearer_token(mgmt$access_token()) |>
        httr2::req_headers(Accept = "text/event-stream") |>
        httr2::req_url_query(event_type = EVENTS_TYPES, .multi = "explode") |>
        httr2::req_timeout(EVENTS_DRAIN_SECONDS + 10) |>
        httr2::req_options(connecttimeout = 5)
    if (!is.null(cursor)) {
        req <- httr2::req_url_query(req, from = cursor)
    } else if (!is.null(from_timestamp)) {
        req <- httr2::req_url_query(req, from_timestamp = from_timestamp)
    }
    httr2::req_perform_connection(req, blocking = FALSE)
}

# --- tick ----------------------------------------------------------------------

# Resolve where this tick starts: the checkpointed cursor if there is one, else
# the from_timestamp anchor (minted and persisted on the very first tick, so a
# drain window that sees nothing does not restart at "latest" and lose events).
events_start_position <- function(pool) {
    cursor <- sync_state_get(pool, AUTH0_EVENTS_CURSOR_KEY)
    if (!is.null(cursor)) {
        return(list(cursor = cursor, from_timestamp = NULL))
    }
    from_timestamp <- sync_state_get(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY)
    if (is.null(from_timestamp)) {
        from_timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        sync_state_set(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY, from_timestamp)
    }
    list(cursor = NULL, from_timestamp = from_timestamp)
}

# Drop both start positions so the next tick anchors on a fresh timestamp. Used
# when Auth0 rejects the cursor itself (retrying it would loop forever).
reset_events_position <- function(pool) {
    log_events_sync("cursor rejected by Auth0, restarting from a fresh timestamp")
    sync_state_delete(pool, AUTH0_EVENTS_CURSOR_KEY)
    sync_state_delete(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY)
    events_state$offset <- NULL
    events_state$persisted_offset <- NULL
    invisible()
}

# One sync pass: open the stream, drain the already-buffered frames, checkpoint,
# close. `on_done` is invoked exactly once, whatever happens, so the schedule
# chain cannot die, and every exit path closes the connection (Auth0 caps
# concurrent event connections at 1 on the free tier).
run_events_sync <- function(config, on_done = function() invisible()) {
    finish <- function(conn = NULL, checkpoint = TRUE) {
        if (!is.null(conn)) {
            try(close(conn), silent = TRUE)
        }
        if (checkpoint) {
            try(persist_events_cursor(), silent = TRUE)
        }
        on_done()
    }
    pool <- app_pool()
    if (is.null(pool) || !mgmt_configured(config)) {
        return(finish())
    }
    start <- tryCatch(
        events_start_position(pool),
        error = function(e) {
            log_events_sync("cannot resolve the start position: %s", conditionMessage(e))
            NULL
        }
    )
    if (is.null(start)) {
        return(finish())
    }
    events_state$offset <- start$cursor
    events_state$persisted_offset <- start$cursor
    conn <- tryCatch(
        events_open_stream(config, start$cursor, start$from_timestamp),
        error = function(e) {
            # HTTP 410 = the cursor aged out of Auth0's retention window.
            if (!is.null(start$cursor) && inherits(e, "httr2_http_410")) {
                try(reset_events_position(pool), silent = TRUE)
            } else {
                log_events_sync("cannot open stream: %s", conditionMessage(e))
            }
            NULL
        }
    )
    if (is.null(conn)) {
        return(finish(checkpoint = FALSE))
    }
    deadline <- Sys.time() + EVENTS_DRAIN_SECONDS
    drain <- function() {
        outcome <- tryCatch(
            {
                verdict <- drain_events_frames(pool, conn)
                if (identical(verdict, "continue") && Sys.time() < deadline && !httr2::resp_stream_is_complete(conn)) {
                    "again"
                } else {
                    verdict
                }
            },
            error = function(e) {
                log_events_sync("drain failed: %s", conditionMessage(e))
                "abort"
            }
        )
        switch(
            outcome,
            again = later::later(drain, EVENTS_DRAIN_POLL_SECONDS),
            reset = {
                try(reset_events_position(pool), silent = TRUE)
                finish(conn, checkpoint = FALSE)
            },
            abort = finish(conn, checkpoint = FALSE),
            finish(conn)
        )
    }
    drain()
    invisible()
}

# Consume every frame currently available (the connection is non-blocking, so
# resp_stream_sse returns NULL once the buffer is empty). Returns the first
# non-"continue" verdict, or "continue" when the buffer simply ran dry.
drain_events_frames <- function(pool, conn) {
    repeat {
        frame <- httr2::resp_stream_sse(conn)
        if (is.null(frame)) {
            return("continue")
        }
        verdict <- handle_events_frame(pool, frame)
        if (!identical(verdict, "continue")) {
            return(verdict)
        }
    }
}

persist_events_cursor <- function() {
    offset <- events_state$offset
    if (is.null(offset) || identical(offset, events_state$persisted_offset)) {
        return(invisible())
    }
    pool <- app_pool()
    sync_state_set(pool, AUTH0_EVENTS_CURSOR_KEY, offset)
    # The timestamp anchor is only a stand-in until a real offset exists.
    if (is.null(events_state$persisted_offset)) {
        sync_state_delete(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY)
    }
    events_state$persisted_offset <- offset
    invisible()
}

# Self-rescheduling chain, registered at startup next to schedule_maintenance.
# The next tick is armed from run_events_sync's on_done, so a slow drain cannot
# stack two overlapping streams (Auth0 caps concurrent connections).
schedule_events_sync <- function(config, interval = EVENTS_SYNC_INTERVAL_SECONDS) {
    if (!mgmt_configured(config)) {
        return(invisible())
    }
    tick <- function() {
        run_events_sync(config, on_done = function() later::later(tick, interval))
    }
    later::later(tick, 0)
    invisible()
}
