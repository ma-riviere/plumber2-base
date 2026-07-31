# Chat session lifecycle: the in-process registry, the per-chat workdir, the pi
# subprocess and the one cleanup ladder every teardown path goes through.
#
# One chat session per browser session (keyed by the session's csrf_id, which is
# already per-session random and never leaves the server). The workdir holds the
# dataset the agent may look at and nothing else; the subprocess is spawned with
# a SCRUBBED environment allowlist, so the front process's Auth0/session secrets
# are never inherited.

CHAT_MAX_PROMPT_CHARS <- 2000L
CHAT_MAX_CSV_BYTES <- 8388608L
CHAT_INGEST_TIMEOUT_SECONDS <- 60
CHAT_TURN_TIMEOUT_SECONDS <- 120
CHAT_ABORT_GRACE_SECONDS <- 10
CHAT_IDLE_SECONDS <- 600
CHAT_KILL_GRACE_MS <- 300
CHAT_MODEL_RETRIES <- 8L
CHAT_MODEL_RETRY_SECONDS <- 1

# Environment variables the subprocess may inherit. Everything else (Auth0
# client secret, SESSION_KEY, PG*, ...) is dropped: processx replaces the whole
# environment rather than extending it.
CHAT_PROVIDER_ENV <- c(
    "LLAMA_BASE_URL",
    "LLAMA_API_KEY",
    "OPENROUTER_API_KEY",
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GEMINI_API_KEY",
    "GROQ_API_KEY",
    "MISTRAL_API_KEY",
    "NVIDIA_API_KEY",
    "TAVILY_API_KEY"
)

chat_registry <- new.env(parent = emptyenv())
chat_quota_state <- new.env(parent = emptyenv())

# --- registry ------------------------------------------------------------------

chat_get <- function(key) {
    if (is.null(key) || !nzchar(key)) {
        return(NULL)
    }
    chat_registry[[key]]
}

chat_live_sessions <- function() {
    as.list(chat_registry)
}

chat_drop <- function(key) {
    if (!is.null(chat_registry[[key]])) {
        rm(list = key, envir = chat_registry)
    }
    invisible()
}

# --- cleanup ladder ------------------------------------------------------------

# The single teardown path: RPC abort -> SIGTERM to the process group -> SIGKILL
# -> workdir removal. Wired into logout (destroy_auth_session), the reset POST,
# the idle sweep, the turn deadline, malformed RPC, subprocess death and dataset
# switches. Safe to call on an already-dead or half-built session.
chat_cleanup <- function(session) {
    if (is.null(session)) {
        return(invisible())
    }
    process <- session$process
    if (!is.null(process)) {
        try(
            {
                if (process$is_alive()) {
                    try(process$write_input("{\"type\":\"abort\"}\n"), silent = TRUE)
                    process$signal(tools::SIGTERM)
                    process$wait(timeout = CHAT_KILL_GRACE_MS)
                }
                if (process$is_alive()) {
                    process$kill_tree()
                }
            },
            silent = TRUE
        )
    }
    session$process <- NULL
    if (!is.null(session$workdir) && dir.exists(session$workdir)) {
        unlink(session$workdir, recursive = TRUE, force = TRUE)
    }
    session$workdir <- NULL
    if (!identical(session$status, "error")) {
        session$status <- "dead"
    }
    session$needs_cleanup <- FALSE
    invisible()
}

chat_cleanup_key <- function(key) {
    session <- chat_get(key)
    if (is.null(session)) {
        return(invisible())
    }
    chat_cleanup(session)
    chat_drop(key)
    invisible()
}

# Startup sweep: api_on("end") does not run on SIGTERM, so a redeploy leaves
# workdirs behind. Nothing else writes under the root, so it is wiped whole.
chat_sweep_workdirs <- function(config) {
    root <- config$chat$workdir_root
    if (dir.exists(root)) {
        unlink(list.files(root, full.names = TRUE), recursive = TRUE, force = TRUE)
    }
    invisible()
}

# --- quota ---------------------------------------------------------------------

# Advisory per-user daily turn budget: in-process only, so it resets on every
# redeploy. Documented as such; the real spend guards are the per-session turn
# and tool-call caps plus the provider's own limits.
chat_quota_take <- function(user_id, limit) {
    key <- sprintf("%s:%s", user_id, format(Sys.Date()))
    used <- chat_quota_state[[key]] %||% 0L
    if (used >= limit) {
        return(FALSE)
    }
    chat_quota_state[[key]] <- used + 1L
    TRUE
}

# --- persistent chat store -----------------------------------------------------

# One persistent chat per (user, dataset): the capped display transcript, kept
# in the FE Postgres datastore (state$chat_store, a storr namespace next to the
# session store - see assemble_api). It is the ONLY persistence artifact: pi
# itself stays stateless (--no-session), and a revived chat gets the previous
# visible dialogue replayed inside its first prompt instead of a session
# restore. The store handle rides on the session so the supervisor can persist
# at settlement without request context.

CHAT_STORE_NAMESPACE <- "chat_transcripts"
CHAT_REPLAY_MAX_CHARS <- 16000L
CHAT_REPLAY_THINKING_CHARS <- 2000L

chat_transcript_key <- function(user_id, dataset_id) {
    sprintf("u%s:d%d", user_id, as.integer(dataset_id))
}

# Written at every turn settlement (and terminal failure). Best-effort: a
# transient store failure loses one checkpoint, never the live conversation.
chat_persist_transcript <- function(session) {
    if (is.null(session$store)) {
        return(invisible())
    }
    tryCatch(
        session$store$set(session$store_key, session$transcript, namespace = CHAT_STORE_NAMESPACE),
        error = function(e) NULL
    )
    invisible()
}

chat_restore_transcript <- function(state, user_id, dataset_id) {
    if (is.null(state$chat_store)) {
        return(NULL)
    }
    messages <- tryCatch(
        state$chat_store$get(chat_transcript_key(user_id, dataset_id), namespace = CHAT_STORE_NAMESPACE),
        error = function(e) NULL
    )
    if (!is.list(messages) || length(messages) == 0L) {
        return(NULL)
    }
    messages
}

chat_forget <- function(state, user_id, dataset_id) {
    if (!is.null(state$chat_store)) {
        try(
            state$chat_store$del(chat_transcript_key(user_id, dataset_id), namespace = CHAT_STORE_NAMESPACE),
            silent = TRUE
        )
    }
    invisible()
}

# The revived agent's memory: the previous dialogue rendered into the first
# prompt (newest entries kept when the cap trims), including each answer's
# persisted thinking (truncated) and tool calls so the model sees HOW it got
# there, not just what it said. Error notices are display state and stay out.
chat_replay_context <- function(transcript) {
    lines <- character()
    total <- 0L
    for (entry in rev(transcript %||% list())) {
        if (!identical(entry$role, "user") && !identical(entry$role, "assistant")) {
            next
        }
        text <- entry$text %||% ""
        if (!nzchar(text)) {
            next
        }
        line <- sprintf("%s: %s", entry$role, text)
        if (identical(entry$role, "assistant")) {
            line <- paste(c(chat_replay_turn_prefix(entry), line), collapse = "\n")
        }
        total <- total + nchar(line)
        if (total > CHAT_REPLAY_MAX_CHARS) {
            break
        }
        lines <- c(line, lines)
    }
    if (length(lines) == 0L) {
        return(NULL)
    }
    paste0(
        "<previous_conversation>\n",
        paste(lines, collapse = "\n"),
        "\n</previous_conversation>\n\n",
        "The exchange above already happened in an earlier session about this dataset; continue from it."
    )
}

# The non-visible half of a past assistant turn: its thinking (truncated - the
# stored blob can be 20k chars and must not starve the replay budget) and the
# tool calls it made, one line each.
chat_replay_turn_prefix <- function(entry) {
    parts <- character()
    thinking <- entry$thinking %||% ""
    if (nzchar(thinking)) {
        parts <- c(
            parts,
            sprintf(
                "assistant (thinking): %s",
                substr(thinking, 1L, CHAT_REPLAY_THINKING_CHARS)
            )
        )
    }
    for (chip in entry$chips %||% list()) {
        args_text <- chip$args_text %||% ""
        if (nzchar(args_text)) {
            parts <- c(parts, sprintf("assistant (tool %s): %s", chip$tool %||% "?", args_text))
        }
    }
    parts
}

# What the widget shows for this dataset on a plain page render: the live
# session when it is bound to this dataset, otherwise the persisted transcript
# as an inert pseudo-session (status "restored" is never active, so it renders
# without a polling trigger), otherwise nothing.
chat_display_session <- function(state, datastore, dataset_id) {
    if (is.null(dataset_id)) {
        return(NULL)
    }
    session <- chat_get(chat_session_key(datastore))
    if (!is.null(session) && identical(as.integer(session$dataset_id), as.integer(dataset_id))) {
        return(session)
    }
    messages <- chat_restore_transcript(state, datastore$session$auth$user_id, dataset_id)
    restored <- list(status = "restored", transcript = messages %||% list())
    for (entry in rev(restored$transcript)) {
        if (identical(entry$role, "assistant") && nzchar(entry$model %||% "")) {
            restored$provider <- entry$provider
            restored$model <- entry$model
            break
        }
    }
    # No persisted answer to attribute: show what the next session would pick,
    # so the header's tooltip is available before the first question too.
    if (is.null(restored$model) && isTRUE(state$config$chat$enabled)) {
        choice <- chat_prospective_model(state$config)
        restored$provider <- choice$provider
        restored$model <- choice$model
    }
    restored
}

# --- workdir -------------------------------------------------------------------

# Fetch the dataset ONCE per chat with the user's own token (the backend stays
# the authorization boundary; pi never sees a bearer token) and describe it in
# DATASET.md so the agent knows the shape without reading the whole CSV.
chat_prepare_workdir <- function(session, state, datastore, detail) {
    root <- state$config$chat$workdir_root
    dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
    workdir <- file.path(root, session$id)
    dir.create(workdir, showWarnings = FALSE, mode = "0700")
    session$workdir <- workdir

    csv <- be_fetch_csv(state, datastore, session$dataset_id)
    if (length(csv$body) > CHAT_MAX_CSV_BYTES) {
        stop(backend_error(413L, "", "dataset too large for the assistant"))
    }
    writeBin(csv$body, file.path(workdir, "dataset.csv"))
    chat_build_duckdb(workdir, state$config$chat$duckdb_bin)
    writeLines(chat_dataset_notes(detail), file.path(workdir, "DATASET.md"), useBytes = TRUE)
    invisible(workdir)
}

# Eager, TRUSTED ingest: one CLI run turns the CSV into the database the query
# tool later opens READ-ONLY under `.safe_mode`. External access is still on
# here because the input is ours; the lockdown belongs to the per-query child
# (.pi/extensions/dataset-chat.ts). Built under a `.part` name and renamed, so a
# half-written file can never be queried.
chat_build_duckdb <- function(workdir, configured_bin) {
    part <- file.path(workdir, "data.duckdb.part")
    unlink(part, force = TRUE)
    result <- tryCatch(
        processx::run(
            chat_duckdb_bin(configured_bin),
            c(
                "-no-init",
                "-batch",
                "-bail",
                "-c",
                "CREATE TABLE dataset AS SELECT * FROM read_csv('dataset.csv'); CHECKPOINT;",
                "data.duckdb.part"
            ),
            wd = workdir,
            timeout = CHAT_INGEST_TIMEOUT_SECONDS,
            error_on_status = FALSE,
            env = c(
                HOME = workdir,
                TMPDIR = workdir,
                LANG = Sys.getenv("LANG", "C.UTF-8"),
                LC_ALL = Sys.getenv("LC_ALL", "C.UTF-8")
            )
        ),
        error = function(e) NULL
    )
    if (is.null(result) || !identical(result$status, 0L) || !file.exists(part)) {
        unlink(part, force = TRUE)
        stop(chat_error("The assistant could not prepare this dataset"))
    }
    file.rename(part, file.path(workdir, "data.duckdb"))
    invisible()
}

# An absolute path, so the query tool's child process needs no PATH in its
# scrubbed environment. An unresolvable name is passed through unchanged and
# fails loudly at spawn rather than silently picking up something else.
chat_duckdb_bin <- function(configured) {
    if (grepl("/", configured, fixed = TRUE)) {
        return(configured)
    }
    resolved <- unname(Sys.which(configured))
    if (nzchar(resolved)) resolved else configured
}

chat_dataset_notes <- function(detail) {
    summary <- detail$summary
    columns <- vapply(
        names(summary) %||% character(),
        function(name) sprintf("- `%s` (%s)", name, be_scalar(summary[[name]]$type) %||% "unknown"),
        character(1)
    )
    description <- be_scalar(detail$description)
    c(
        sprintf("# %s", be_scalar(detail$name) %||% "dataset"),
        "",
        if (!is.null(description) && nzchar(description)) c(description, ""),
        sprintf(
            paste(
                "The file `dataset.csv` in this directory has %s rows and %s columns.",
                "The same data is loaded in DuckDB as the table `dataset`, which the `query` tool reads."
            ),
            fmt_count(detail$n_rows),
            fmt_count(detail$n_cols)
        ),
        "",
        "## Columns",
        "",
        unname(columns)
    )
}

# --- subprocess ----------------------------------------------------------------

chat_asset_path <- function(state, ...) {
    file.path(state$base_dir, ".pi", ...)
}

# The scrubbed environment. HOME points at the workdir so pi writes any state
# there (and the whole tree dies with the session); PI_CODING_AGENT_DIR keeps
# its config/credential store out of the real home.
chat_spawn_env <- function(state, session) {
    config <- state$config
    vars <- c(
        PATH = Sys.getenv("PATH"),
        HOME = session$workdir,
        TMPDIR = session$workdir,
        LANG = Sys.getenv("LANG", "C.UTF-8"),
        LC_ALL = Sys.getenv("LC_ALL", "C.UTF-8"),
        PI_CODING_AGENT_DIR = file.path(session$workdir, ".pi-agent"),
        PI_SKIP_VERSION_CHECK = "1",
        PI_TELEMETRY = "0",
        CHAT_WORKDIR = session$workdir,
        CHAT_DATASET_CSV = file.path(session$workdir, "dataset.csv"),
        CHAT_DUCKDB_BIN = chat_duckdb_bin(config$chat$duckdb_bin),
        CHAT_DUCKDB_DB = file.path(session$workdir, "data.duckdb"),
        CHAT_SYSTEM_PROMPT = chat_asset_path(state, "system-prompt.md"),
        CHAT_WEBSEARCH = if (isTRUE(config$chat$websearch)) "1" else "0"
    )
    for (name in CHAT_PROVIDER_ENV) {
        value <- Sys.getenv(name)
        if (nzchar(value)) {
            vars[[name]] <- value
        }
    }
    vars
}

# Launch pi in RPC mode with NO built-in tools: the extension registers the only
# three the agent gets (a workdir-scoped `read`, `query`, `websearch`). This is
# fail-closed - if the extension fails to load, the agent has no tools at all
# rather than the built-in `bash`/`write`/`edit`. `--no-approve` keeps any
# project-local `.pi` resources untrusted; CLI `-e` extensions load before that
# check, so ours still loads.
chat_spawn <- function(session, state) {
    args <- c(
        "--mode",
        "rpc",
        "--no-session",
        "--no-approve",
        "--no-builtin-tools",
        "-e",
        chat_asset_path(state, "extensions", "dataset-chat.ts")
    )
    session$process <- processx::process$new(
        state$config$chat$pi_bin,
        args,
        stdin = "|",
        stdout = "|",
        stderr = "|",
        wd = session$workdir,
        env = chat_spawn_env(state, session),
        cleanup = TRUE
    )
    invisible(session)
}

# Serialize one RPC command as a single LF-terminated JSON record. `id` is
# assigned here so every command is correlatable with its response.
chat_write_command <- function(session, command) {
    command$id <- chat_next_command_id(session, command$type)
    line <- paste0(yyjsonr::write_json_str(command, auto_unbox = TRUE), "\n")
    session$last_command <- command
    process <- session$process
    if (is.null(process)) {
        return(invisible(session))
    }
    ok <- tryCatch(
        {
            process$write_input(line)
            TRUE
        },
        error = function(e) FALSE
    )
    if (!ok) {
        chat_fail(session, "The assistant connection failed")
    }
    invisible(session)
}

# --- session creation ----------------------------------------------------------

# pi discovers llama.cpp router models through an async catalog fetch, and every
# session starts on a cold pi state dir (fresh PI_CODING_AGENT_DIR), so a
# set_model sent right after spawn can race the fetch and fail with "Model not
# found" even though the alias is loaded. That failure is transient: it is
# retried on the supervisor tick (chat_retry_set_model) until the budget runs
# out; only exhaustion is terminal.
chat_send_set_model <- function(session) {
    chat_write_command(session, list(type = "set_model", provider = session$provider, modelId = session$model))
}

# Create the chat session for this browser session and dataset: workdir, pi
# subprocess, auto-retry ON (transient provider errors - e.g. NVIDIA free-tier
# ResourceExhausted - are retried by pi inside the turn; the 120s deadline stays
# deterministic because chat_enforce_deadlines aborts on OUR walltime clock,
# retries or not) and the awaited set_model. The first prompt is queued until
# set_model answers. When a persisted chat exists for this (user, dataset), its
# transcript is preloaded for display and rendered into a replay context that
# rides along with the FIRST prompt - that replay IS the revived agent's whole
# memory (pi runs --no-session and knows nothing across revivals).
chat_create <- function(state, datastore, key, user_id, dataset_id, lang, detail) {
    session <- chat_new_state(key, dataset_id, user_id, lang)
    session$store <- state$chat_store
    session$store_key <- chat_transcript_key(user_id, dataset_id)
    session$transcript <- chat_restore_transcript(state, user_id, dataset_id) %||% list()
    session$replay_context <- chat_replay_context(session$transcript)
    chat_prepare_workdir(session, state, datastore, detail)
    choice <- chat_choose_model(state$config)
    session$provider <- choice$provider
    session$model <- choice$model
    chat_spawn(session, state)
    session$status <- "awaiting_model"
    chat_write_command(session, list(type = "set_auto_retry", enabled = TRUE))
    chat_send_set_model(session)
    chat_registry[[key]] <- session
    chat_supervisor_start()
    session
}

# The user's turn. A session already bound to another dataset is replaced
# atomically here (a dataset SWITCH must never kill a process from a GET).
chat_submit <- function(state, datastore, key, user_id, dataset_id, lang, prompt, detail) {
    session <- chat_get(key)
    if (!is.null(session) && !identical(session$dataset_id, dataset_id)) {
        chat_cleanup_key(key)
        session <- NULL
    }
    if (!is.null(session) && session$status %in% c("dead", "error")) {
        chat_cleanup_key(key)
        session <- NULL
    }
    if (is.null(session)) {
        session <- chat_create(state, datastore, key, user_id, dataset_id, lang, detail)
    }
    session$lang <- lang
    session$last_poll_at <- as.numeric(Sys.time())
    chat_push_message(session, "user", prompt)
    message <- chat_prompt_text(prompt, lang, state$translations)
    # Consumed once: only the first prompt of a revived session carries the
    # replayed history.
    if (!is.null(session$replay_context)) {
        message <- paste0(session$replay_context, "\n\n", message)
        session$replay_context <- NULL
    }
    if (identical(session$status, "awaiting_model")) {
        session$queued_prompt <- message
    } else {
        chat_start_turn(session, message)
    }
    session
}

# --- access + error boundary ---------------------------------------------------

# Whether this session may use chat at all. Guests are refused SERVER-SIDE in
# every handler, not merely by hiding the widget; CHAT_ALLOW_GUESTS re-opens it
# only under BYPASS_AUTH (where every session is a guest), and config.R already
# refuses that combination outside dev.
chat_visible <- function(state, datastore) {
    if (!isTRUE(state$config$chat$enabled)) {
        return(FALSE)
    }
    auth <- datastore$session$auth
    !isTRUE(auth$is_guest) || isTRUE(state$config$chat$allow_guests)
}

# Refusal reason for a chat handler, or NULL when the request may proceed.
chat_denial <- function(state, datastore) {
    if (!isTRUE(state$config$chat$enabled)) {
        return("The assistant is not available right now")
    }
    auth <- datastore$session$auth
    if (isTRUE(auth$is_guest) && !isTRUE(state$config$chat$allow_guests)) {
        return("Sign in to use the dataset assistant")
    }
    NULL
}

chat_session_key <- function(datastore) {
    datastore$session$auth$csrf_id
}

chat_needs_create <- function(session, dataset_id) {
    is.null(session) ||
        !identical(session$dataset_id, dataset_id) ||
        session$status %in% c("dead", "error")
}

# Chat-specific failures (no model available, backend refusing the CSV) render
# as an in-widget notice rather than the page-level error alert.
with_chat_errors <- function(response, dataset_id, lang, translations, expr) {
    tryCatch(
        expr,
        fe_chat_error = function(e) {
            response$status <- 503L
            set_html_headers(response)
            chat_stream_html(NULL, dataset_id, lang, translations, notice = e$key)
        }
    )
}

# The UI language is a SERVER-controlled instruction sent with every prompt: a
# session-start-only instruction goes stale as soon as the user switches
# language mid-chat.
chat_prompt_text <- function(prompt, lang, translations) {
    language <- tr(if (identical(lang, "fr")) "French" else "English", "en", translations)
    sprintf("<instructions>Answer in %s.</instructions>\n\n%s", language, prompt)
}
