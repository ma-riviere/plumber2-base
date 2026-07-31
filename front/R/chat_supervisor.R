# The single later-tick supervisor draining every live chat subprocess.
#
# R is single-threaded and fiery's loop is the only thread there is: a blocking
# read on any subprocess would stall the whole app. So one self-rescheduling
# `later` tick (the pattern back/R/maintenance.R uses) round-robins over the
# registry with ZERO-timeout polls and a per-session byte budget, so one chatty
# subprocess cannot starve the others or the HTTP handlers. The tick stops
# rescheduling itself once the registry empties, and restarts with the next
# session.

CHAT_TICK_SECONDS <- 0.25

chat_supervisor <- new.env(parent = emptyenv())
chat_supervisor$running <- FALSE

chat_supervisor_start <- function() {
    if (isTRUE(chat_supervisor$running)) {
        return(invisible())
    }
    chat_supervisor$running <- TRUE
    tick <- function() {
        tryCatch(chat_tick(), error = function(e) NULL)
        if (length(ls(chat_registry)) == 0L) {
            chat_supervisor$running <- FALSE
            return(invisible())
        }
        later::later(tick, CHAT_TICK_SECONDS)
    }
    later::later(tick, 0)
    invisible()
}

chat_tick <- function(now = as.numeric(Sys.time())) {
    for (key in ls(chat_registry)) {
        session <- chat_registry[[key]]
        if (is.null(session)) {
            next
        }
        chat_drain(session)
        chat_retry_set_model(session, now)
        chat_enforce_deadlines(session, now)
        if (isTRUE(session$needs_cleanup)) {
            chat_cleanup(session)
        }
        # No poll for a while means the widget is gone (page left, tab closed):
        # the process would otherwise keep its model context alive for nothing.
        if (now - session$last_poll_at > CHAT_IDLE_SECONDS) {
            chat_cleanup_key(key)
        }
    }
    invisible()
}

# One non-blocking pass over a session's pipes, capped at CHAT_READ_CHUNK_BYTES
# per stream. Whatever is left stays in the pipe for the next tick.
chat_drain <- function(session) {
    process <- session$process
    if (is.null(process)) {
        return(invisible(session))
    }
    ready <- tryCatch(process$poll_io(0), error = function(e) NULL)
    if (is.null(ready)) {
        return(invisible(chat_fail(session, "The assistant connection failed")))
    }
    if (identical(ready[["output"]], "ready")) {
        chat_feed_stdout(session, chat_read_stream(process, "output"))
    }
    if (identical(ready[["error"]], "ready")) {
        chat_feed_stderr(session, chat_read_stream(process, "error"))
    }
    # Death is only an error while a turn (or the startup handshake) is pending;
    # a process reaped after cleanup is the normal path.
    if (!process$is_alive() && chat_is_active(session)) {
        chat_fail(session, "The assistant connection failed")
    }
    invisible(session)
}

chat_read_stream <- function(process, which) {
    tryCatch(
        {
            chunk <- if (identical(which, "output")) {
                process$read_output(CHAT_READ_CHUNK_BYTES)
            } else {
                process$read_error(CHAT_READ_CHUNK_BYTES)
            }
            if (is.null(chunk) || is.na(chunk)) "" else chunk
        },
        error = function(e) ""
    )
}

# Re-send a set_model whose transient failure scheduled a retry (see
# chat_send_set_model in R/chat_session.R).
chat_retry_set_model <- function(session, now) {
    if (!identical(session$status, "awaiting_model") || is.na(session$model_retry_at) || now < session$model_retry_at) {
        return(invisible(session))
    }
    session$model_retry_at <- NA_real_
    chat_send_set_model(session)
    invisible(session)
}

# Per-turn walltime: RPC abort first (pi can still emit agent_settled, which
# closes the turn cleanly), then a short grace before the kill ladder.
chat_enforce_deadlines <- function(session, now) {
    if (!identical(session$status, "streaming") || is.na(session$turn_started_at)) {
        return(invisible(session))
    }
    if (!isTRUE(session$aborting)) {
        if (now - session$turn_started_at > CHAT_TURN_TIMEOUT_SECONDS) {
            session$aborting <- TRUE
            session$abort_sent_at <- now
            chat_write_command(session, list(type = "abort"))
        }
    } else if (now - session$abort_sent_at > CHAT_ABORT_GRACE_SECONDS) {
        chat_fail(session, "The answer took too long and was stopped")
    }
    invisible(session)
}
