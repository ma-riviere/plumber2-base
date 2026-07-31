# pi RPC state machine. Pure state transitions over a chat-session environment:
# no process I/O lives here, so the whole protocol can be unit-tested by feeding
# fixture JSONL through chat_feed_stdout(). The supervisor (R/chat_supervisor.R)
# owns the reads, the writes and the kill ladder.
#
# Protocol facts this encodes (pi 0.82, `--mode rpc`):
#   - strict JSONL, LF is the ONLY record separator (a trailing \r is stripped);
#     records arrive fragmented, so partial lines are buffered.
#   - every command may carry an `id`; its `{"type":"response","command":...}`
#     echoes it back. Correlation is by id, never by arrival order.
#   - a user turn ends on `agent_settled`, NOT on `agent_end`: pi may still
#     retry, compact or drain queued follow-ups after agent_end.
#   - streaming text deltas are nested: message_update.assistantMessageEvent
#     with type "text_delta" (field `delta`). Thinking deltas show only a
#     generic activity indicator while streaming; the content accumulates into
#     the settled entry's collapsed "Thoughts" block (escaped, never markdown).

# Byte/size caps. A misbehaving or hostile subprocess must not be able to grow
# the R heap or stall the single-threaded event loop.
CHAT_MAX_LINE_BYTES <- 1048576L
CHAT_MAX_STDERR_BYTES <- 262144L
CHAT_MAX_TURN_BYTES <- 8388608L
CHAT_READ_CHUNK_BYTES <- 65536L
CHAT_MAX_REPLY_CHARS <- 20000L
CHAT_MAX_THINKING_CHARS <- 20000L
CHAT_MAX_TOOL_ARGS_CHARS <- 2000L
CHAT_MAX_TOOL_CALLS <- 24L
CHAT_MAX_TURNS <- 30L
CHAT_TRANSCRIPT_MESSAGES <- 40L

# The tool names the widget knows how to label; anything else shows a generic
# chip (the model cannot invent tools, but a pi version bump could).
CHAT_TOOL_LABELS <- list(
    query = "Querying data",
    websearch = "Searching the web",
    read = "Reading the dataset"
)

# Chat failures carry an i18n KEY (the English source string) rather than a
# rendered message, so the same condition renders in whatever language the
# request that catches it resolves.
chat_error <- function(key) {
    structure(
        class = c("fe_chat_error", "error", "condition"),
        list(message = paste0("chat: ", key), key = key, call = NULL)
    )
}

# A fresh session state. Kept as an environment so the supervisor mutates it in
# place from a later tick without reassigning into the registry.
chat_new_state <- function(key, dataset_id, user_id, lang) {
    session <- new.env(parent = emptyenv())
    session$id <- paste0("c", sodium::bin2hex(sodium::random(8)))
    session$key <- key
    session$dataset_id <- dataset_id
    session$user_id <- user_id
    session$lang <- lang
    session$status <- "starting"
    session$process <- NULL
    session$workdir <- NULL
    session$provider <- NULL
    session$model <- NULL
    session$stdout_buffer <- ""
    session$stderr_bytes <- 0L
    session$turn_bytes <- 0L
    session$command_seq <- 0L
    session$pending <- list()
    session$transcript <- list()
    session$store <- NULL
    session$store_key <- NULL
    session$replay_context <- NULL
    session$stream_text <- ""
    session$thinking_text <- ""
    session$chips <- list()
    session$activity <- NULL
    session$queued_prompt <- NULL
    session$turns <- 0L
    session$tool_calls <- 0L
    session$error <- NULL
    session$aborting <- FALSE
    session$needs_cleanup <- FALSE
    session$model_retries_left <- CHAT_MODEL_RETRIES
    session$model_retry_at <- NA_real_
    now <- as.numeric(Sys.time())
    session$created_at <- now
    session$last_poll_at <- now
    session$turn_started_at <- NA_real_
    session$abort_sent_at <- NA_real_
    session
}

chat_is_active <- function(session) {
    !is.null(session) && session$status %in% c("starting", "awaiting_model", "streaming")
}

chat_next_command_id <- function(session, command) {
    session$command_seq <- session$command_seq + 1L
    id <- sprintf("%s-%d", session$id, session$command_seq)
    session$pending[[id]] <- command
    id
}

# --- stream ingestion ----------------------------------------------------------

# Append a stdout chunk and dispatch every COMPLETE record. The remainder stays
# buffered for the next tick; an unterminated record larger than the line cap is
# treated as a protocol failure rather than buffered forever.
chat_feed_stdout <- function(session, chunk) {
    if (!nzchar(chunk)) {
        return(invisible(session))
    }
    session$turn_bytes <- session$turn_bytes + nchar(chunk, type = "bytes")
    if (session$turn_bytes > CHAT_MAX_TURN_BYTES) {
        return(invisible(chat_fail(session, "The assistant produced too much output")))
    }
    buffer <- paste0(session$stdout_buffer, chunk)
    parts <- strsplit(buffer, "\n", fixed = TRUE)[[1]]
    complete <- if (endsWith(buffer, "\n")) parts else utils::head(parts, -1L)
    session$stdout_buffer <- if (endsWith(buffer, "\n")) "" else utils::tail(parts, 1L)
    if (nchar(session$stdout_buffer, type = "bytes") > CHAT_MAX_LINE_BYTES) {
        return(invisible(chat_fail(session, "The assistant produced too much output")))
    }
    for (line in complete) {
        if (session$status %in% c("error", "dead")) {
            break
        }
        chat_feed_line(session, line)
    }
    invisible(session)
}

chat_feed_line <- function(session, line) {
    line <- sub("\r$", "", line)
    if (!nzchar(trimws(line))) {
        return(invisible(session))
    }
    # yyjsonr echoes the offending text to stdout when a parse fails; that text
    # is model output, which must never reach the logs.
    event <- NULL
    utils::capture.output(
        event <- tryCatch(
            yyjsonr::read_json_str(line, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE),
            error = function(e) NULL
        )
    )
    if (!is.list(event) || is.null(event$type)) {
        return(invisible(chat_fail(session, "The assistant connection failed")))
    }
    chat_apply_event(session, event)
}

# stderr is never parsed or shown; it is only counted, and a flood is a symptom
# of a broken subprocess (dependency errors, crash loops) that must not run on.
chat_feed_stderr <- function(session, chunk) {
    if (!nzchar(chunk)) {
        return(invisible(session))
    }
    session$stderr_bytes <- session$stderr_bytes + nchar(chunk, type = "bytes")
    if (session$stderr_bytes > CHAT_MAX_STDERR_BYTES) {
        chat_fail(session, "The assistant connection failed")
    }
    invisible(session)
}

# --- event dispatch ------------------------------------------------------------

chat_apply_event <- function(session, event) {
    switch(
        event$type,
        response = chat_apply_response(session, event),
        message_update = chat_apply_message_update(session, event),
        tool_execution_start = chat_apply_tool_start(session, event),
        tool_execution_end = chat_apply_tool_end(session, event),
        agent_settled = chat_settle_turn(session),
        extension_error = chat_fail(session, "The assistant connection failed"),
        invisible(session)
    )
}

# Responses are correlated by id. An unknown id is ignored (a late response to a
# command from a previous, already-reset session generation).
chat_apply_response <- function(session, event) {
    id <- be_scalar(event$id)
    command <- if (!is.null(id)) session$pending[[id]] else be_scalar(event$command)
    if (!is.null(id)) {
        session$pending[[id]] <- NULL
    }
    if (is.null(command)) {
        return(invisible(session))
    }
    success <- isTRUE(be_scalar(event$success))
    if (identical(command, "set_model")) {
        if (!success) {
            # Transient on a cold pi state dir (see chat_send_set_model):
            # schedule a re-send instead of failing, until the budget runs out.
            if (session$model_retries_left > 0L) {
                session$model_retries_left <- session$model_retries_left - 1L
                session$model_retry_at <- as.numeric(Sys.time()) + CHAT_MODEL_RETRY_SECONDS
                return(invisible(session))
            }
            return(invisible(chat_fail(session, "No chat model is available right now")))
        }
        session$model_retry_at <- NA_real_
        session$status <- "idle"
        if (!is.null(session$queued_prompt)) {
            prompt <- session$queued_prompt
            session$queued_prompt <- NULL
            chat_start_turn(session, prompt)
        }
        return(invisible(session))
    }
    if (identical(command, "prompt") && !success) {
        return(invisible(chat_fail(session, "The assistant could not answer")))
    }
    invisible(session)
}

chat_apply_message_update <- function(session, event) {
    inner <- event$assistantMessageEvent
    kind <- be_scalar(inner$type)
    if (is.null(kind)) {
        return(invisible(session))
    }
    if (identical(kind, "text_delta")) {
        delta <- be_scalar(inner$delta) %||% ""
        session$activity <- NULL
        if (nchar(session$stream_text) < CHAT_MAX_REPLY_CHARS) {
            session$stream_text <- paste0(session$stream_text, as.character(delta))
        }
    } else if (kind %in% c("thinking_start", "thinking_delta")) {
        # Live view shows only a generic indicator; the content accumulates for
        # the settled entry's collapsed "Thoughts" block (persisted, escaped).
        session$activity <- list(kind = "thinking")
        if (identical(kind, "thinking_delta")) {
            delta <- be_scalar(inner$delta) %||% ""
            if (nchar(session$thinking_text) < CHAT_MAX_THINKING_CHARS) {
                session$thinking_text <- paste0(session$thinking_text, as.character(delta))
            }
        }
    } else if (identical(kind, "text_start")) {
        session$activity <- NULL
    }
    invisible(session)
}

chat_apply_tool_start <- function(session, event) {
    tool <- as.character(be_scalar(event$toolName) %||% "")
    session$tool_calls <- session$tool_calls + 1L
    if (session$tool_calls > CHAT_MAX_TOOL_CALLS) {
        return(invisible(chat_fail(session, "The assistant used too many tools for one question")))
    }
    session$activity <- list(kind = "tool", tool = tool)
    call_id <- as.character(be_scalar(event$toolCallId) %||% tool)
    session$chips[[call_id]] <- list(tool = tool, done = FALSE, args_text = chat_tool_args_text(tool, event$args))
    invisible(session)
}

# A display string for a tool call's arguments: the known tools carry one
# meaningful string field; anything else (a future pi/extension bump) falls back
# to compact JSON so the chip stays honest.
chat_tool_args_text <- function(tool, args) {
    if (!is.list(args) || length(args) == 0L) {
        return(NULL)
    }
    known <- switch(tool %||% "", query = "sql", websearch = "query", read = "path", NULL)
    text <- if (!is.null(known)) be_scalar(args[[known]])
    if (is.null(text)) {
        text <- tryCatch(yyjsonr::write_json_str(args, auto_unbox = TRUE), error = function(e) NULL)
    }
    if (is.null(text) || !nzchar(text)) {
        return(NULL)
    }
    substr(as.character(text), 1L, CHAT_MAX_TOOL_ARGS_CHARS)
}

chat_apply_tool_end <- function(session, event) {
    call_id <- as.character(be_scalar(event$toolCallId) %||% "")
    if (!is.null(session$chips[[call_id]])) {
        session$chips[[call_id]]$done <- TRUE
    }
    session$activity <- NULL
    invisible(session)
}

# --- turn boundaries -----------------------------------------------------------

# Record the user message and mark the turn in flight. The command write itself
# is the supervisor's job (chat_write_command), invoked through this hook so the
# unit tests can drive turns without a subprocess.
chat_start_turn <- function(session, prompt) {
    session$status <- "streaming"
    session$stream_text <- ""
    session$thinking_text <- ""
    session$chips <- list()
    session$activity <- NULL
    session$turn_bytes <- 0L
    session$aborting <- FALSE
    session$abort_sent_at <- NA_real_
    session$turn_started_at <- as.numeric(Sys.time())
    session$turns <- session$turns + 1L
    chat_write_command(session, list(type = "prompt", message = prompt))
    invisible(session)
}

# agent_settled is the ONLY completion signal: agent_end may be followed by an
# automatic retry, a compaction retry or a queued follow-up.
chat_settle_turn <- function(session) {
    if (!identical(session$status, "streaming")) {
        return(invisible(session))
    }
    text <- session$stream_text
    if (nzchar(trimws(text))) {
        chat_push_message(session, "assistant", text)
    } else if (session$aborting) {
        chat_push_message(session, "error", NULL, error = "The answer took too long and was stopped")
    } else {
        chat_push_message(session, "error", NULL, error = "The assistant could not answer")
    }
    session$stream_text <- ""
    session$thinking_text <- ""
    session$activity <- NULL
    session$status <- "idle"
    session$aborting <- FALSE
    session$abort_sent_at <- NA_real_
    session$turn_started_at <- NA_real_
    chat_persist_transcript(session)
    invisible(session)
}

chat_push_message <- function(session, role, text, error = NULL) {
    thinking <- session$thinking_text %||% ""
    entry <- list(
        role = role,
        text = text,
        error = error,
        chips = session$chips,
        thinking = if (nzchar(thinking)) thinking
    )
    if (identical(role, "assistant")) {
        # Persisted so a restored transcript can still show which model
        # answered (the header's info icon) without spawning a session.
        entry$provider <- session$provider
        entry$model <- session$model
    }
    session$transcript <- c(session$transcript, list(entry))
    if (length(session$transcript) > CHAT_TRANSCRIPT_MESSAGES) {
        session$transcript <- utils::tail(session$transcript, CHAT_TRANSCRIPT_MESSAGES)
    }
    invisible(session)
}

# Terminal failure: the transcript keeps what was already answered, the session
# is flagged for the supervisor's kill + workdir removal. `reason` is an English
# source string, i.e. an i18n key (see assets/translations.json).
chat_fail <- function(session, reason) {
    if (identical(session$status, "error")) {
        return(invisible(session))
    }
    if (identical(session$status, "streaming") && nzchar(trimws(session$stream_text))) {
        chat_push_message(session, "assistant", session$stream_text)
    }
    session$status <- "error"
    session$error <- reason
    session$stream_text <- ""
    session$thinking_text <- ""
    session$activity <- NULL
    session$needs_cleanup <- TRUE
    chat_persist_transcript(session)
    invisible(session)
}
