# RPC framing and state machine, driven entirely by fixture JSONL: no
# subprocess, no network. chat_write_command tolerates a NULL process, so turns
# can be started and settled in-process.

new_test_session <- function() {
    session <- chat_new_state("k1", 1L, 7L, "en")
    session$status <- "idle"
    session
}

# Feed a string in arbitrary-sized slices, so every record boundary lands mid-chunk.
feed_in_chunks <- function(session, text, size) {
    starts <- seq(1L, nchar(text), by = size)
    for (start in starts) {
        chat_feed_stdout(session, substr(text, start, min(start + size - 1L, nchar(text))))
    }
    session
}

turn_fixture <- function() {
    paste0(paste(readLines(test_path("fixtures", "chat-turn.jsonl")), collapse = "\n"), "\n")
}

test_that("fragmented JSONL records are reassembled and the turn settles once", {
    for (size in c(1L, 7L, 64L, 4096L)) {
        session <- new_test_session()
        chat_start_turn(session, "hello")
        feed_in_chunks(session, turn_fixture(), size)
        expect_equal(session$status, "idle", info = size)
        expect_length(session$transcript, 1L)
        expect_equal(session$transcript[[1]]$role, "assistant")
        expect_equal(session$transcript[[1]]$text, "The mean is 42.")
        expect_equal(session$transcript[[1]]$thinking, "private chain of thought")
        expect_equal(session$transcript[[1]]$chips[["call_1"]]$args_text, "SELECT avg(x) FROM dataset;")
    }
})

test_that("agent_end (with a retry in between) does not end the turn - only agent_settled does", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    lines <- readLines(test_path("fixtures", "chat-turn.jsonl"))
    settled <- grep('"agent_settled"', lines)
    chat_feed_stdout(session, paste0(paste(lines[seq_len(settled - 1L)], collapse = "\n"), "\n"))
    expect_equal(session$status, "streaming")
    expect_length(session$transcript, 0L)
    chat_feed_stdout(session, paste0(lines[[settled]], "\n"))
    expect_equal(session$status, "idle")
    expect_length(session$transcript, 1L)
})

test_that("thinking streams as a generic indicator but persists into the settled entry", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(
        session,
        '{"type":"message_update","assistantMessageEvent":{"type":"thinking_delta","delta":"<secret>"}}\n'
    )
    expect_equal(session$activity$kind, "thinking")
    expect_equal(session$stream_text, "")
    # Live view: spinner only, never the content.
    html <- chat_stream_html(session, 1L, "en", load_translations(translations_path))
    expect_false(grepl("secret", html, fixed = TRUE))
    chat_feed_stdout(
        session,
        paste0(
            '{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"Done."}}\n',
            '{"type":"agent_settled"}\n'
        )
    )
    expect_equal(session$transcript[[1]]$thinking, "<secret>")
    expect_equal(session$thinking_text, "")
    # Settled view: the thinking shows in a collapsed details block, escaped.
    html <- chat_stream_html(session, 1L, "en", load_translations(translations_path))
    expect_match(html, "chat-thinking", fixed = TRUE)
    expect_match(html, "&lt;secret&gt;", fixed = TRUE)
    expect_false(grepl("<secret>", html, fixed = TRUE))
})

test_that("a settled turn persists the transcript to the store, and it restores", {
    store <- storr::storr_environment()
    session <- new_test_session()
    session$store <- store
    session$store_key <- chat_transcript_key(7L, 1L)
    chat_push_message(session, "user", "q")
    chat_start_turn(session, "q")
    chat_feed_stdout(
        session,
        paste0(
            '{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"The answer."}}\n',
            '{"type":"agent_settled"}\n'
        )
    )
    state <- list(chat_store = store)
    restored <- chat_restore_transcript(state, 7L, 1L)
    expect_length(restored, 2L)
    expect_equal(restored[[1]]$role, "user")
    expect_equal(restored[[2]]$text, "The answer.")

    chat_forget(state, 7L, 1L)
    expect_null(chat_restore_transcript(state, 7L, 1L))
})

test_that("the replay context keeps the dialogue with thinking and tool calls, newest first under the cap", {
    expect_null(chat_replay_context(list()))
    transcript <- list(
        list(role = "user", text = "what is the mean"),
        list(
            role = "assistant",
            text = "The mean is 42.",
            thinking = "I should query the data.",
            chips = list(list(tool = "query", done = TRUE, args_text = "SELECT avg(x) FROM dataset;"))
        ),
        list(role = "error", text = NULL, error = "The assistant could not answer")
    )
    context <- chat_replay_context(transcript)
    expect_match(context, "<previous_conversation>", fixed = TRUE)
    expect_match(context, "user: what is the mean", fixed = TRUE)
    expect_match(context, "assistant (thinking): I should query the data.", fixed = TRUE)
    expect_match(context, "assistant (tool query): SELECT avg(x) FROM dataset;", fixed = TRUE)
    expect_match(context, "assistant: The mean is 42.", fixed = TRUE)
    expect_false(grepl("could not answer", context, fixed = TRUE))

    # Overflow trims from the OLDEST side.
    long <- c(
        list(list(role = "user", text = strrep("x", CHAT_REPLAY_MAX_CHARS))),
        transcript[1:2]
    )
    trimmed <- chat_replay_context(long)
    expect_false(grepl("xxxx", trimmed, fixed = TRUE))
    expect_match(trimmed, "The mean is 42.", fixed = TRUE)
})

test_that("tool executions become chips and are capped", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(
        session,
        '{"type":"tool_execution_start","toolCallId":"a","toolName":"websearch","args":{"query":"mean height"}}\n'
    )
    expect_equal(session$activity$kind, "tool")
    expect_equal(session$chips[["a"]]$tool, "websearch")
    expect_equal(session$chips[["a"]]$args_text, "mean height")
    expect_false(session$chips[["a"]]$done)
    chat_feed_stdout(session, '{"type":"tool_execution_end","toolCallId":"a","toolName":"websearch"}\n')
    expect_true(session$chips[["a"]]$done)

    for (i in seq_len(CHAT_MAX_TOOL_CALLS + 1L)) {
        chat_feed_stdout(
            session,
            sprintf('{"type":"tool_execution_start","toolCallId":"c%d","toolName":"query","args":{}}\n', i)
        )
    }
    expect_equal(session$status, "error")
    expect_equal(session$error, "The assistant used too many tools for one question")
})

test_that("responses are correlated by command id and a failed set_model is terminal", {
    session <- chat_new_state("k1", 1L, 7L, "en")
    session$status <- "awaiting_model"
    id <- chat_next_command_id(session, "set_model")
    session$queued_prompt <- "question"

    # A response for an id this session never issued is ignored.
    chat_feed_stdout(session, '{"type":"response","id":"other-99","command":"set_model","success":true}\n')
    expect_equal(session$status, "awaiting_model")

    chat_feed_stdout(session, sprintf('{"type":"response","id":"%s","command":"set_model","success":true}\n', id))
    expect_equal(session$status, "streaming")
    expect_null(session$queued_prompt)
    expect_equal(session$last_command$type, "prompt")
    expect_equal(session$last_command$message, "question")
})

test_that("a failed set_model response ends the session once retries are exhausted", {
    session <- chat_new_state("k1", 1L, 7L, "en")
    session$status <- "awaiting_model"
    session$model_retries_left <- 0L
    id <- chat_next_command_id(session, "set_model")
    chat_feed_stdout(
        session,
        sprintf('{"type":"response","id":"%s","command":"set_model","success":false,"error":"nope"}\n', id)
    )
    expect_equal(session$status, "error")
    expect_equal(session$error, "No chat model is available right now")
    expect_true(session$needs_cleanup)
})

test_that("a transiently failed set_model is re-sent by the supervisor before failing", {
    session <- chat_new_state("k1", 1L, 7L, "en")
    session$status <- "awaiting_model"
    session$provider <- "llama.cpp"
    session$model <- "small"
    session$model_retries_left <- 1L
    id <- chat_next_command_id(session, "set_model")

    # pi's cold-start "Model not found": the session survives, a retry is due.
    chat_feed_stdout(
        session,
        sprintf('{"type":"response","id":"%s","command":"set_model","success":false,"error":"nope"}\n', id)
    )
    expect_equal(session$status, "awaiting_model")
    expect_false(is.na(session$model_retry_at))

    # Before the delay elapses nothing is re-sent.
    chat_retry_set_model(session, session$model_retry_at - 0.1)
    expect_null(session$last_command)

    # Once due, the same provider/model pair goes out again under a fresh id.
    chat_retry_set_model(session, session$model_retry_at + 0.1)
    expect_true(is.na(session$model_retry_at))
    expect_equal(session$last_command$type, "set_model")
    expect_equal(session$last_command$modelId, "small")

    # A second failure exhausts the budget and is terminal.
    retry_id <- session$last_command$id
    chat_feed_stdout(
        session,
        sprintf('{"type":"response","id":"%s","command":"set_model","success":false,"error":"nope"}\n', retry_id)
    )
    expect_equal(session$status, "error")
    expect_equal(session$error, "No chat model is available right now")
})

test_that("malformed JSON is a protocol failure, not a skipped line", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(session, "{not json at all}\n")
    expect_equal(session$status, "error")
    expect_equal(session$error, "The assistant connection failed")
    expect_true(session$needs_cleanup)
})

test_that("an unterminated record cannot grow the buffer past the line cap", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(session, strrep("x", CHAT_MAX_LINE_BYTES + 1L))
    expect_equal(session$status, "error")
})

test_that("a stderr flood fails the session", {
    session <- new_test_session()
    chat_feed_stderr(session, strrep("e", 1024L))
    expect_equal(session$status, "idle")
    chat_feed_stderr(session, strrep("e", CHAT_MAX_STDERR_BYTES))
    expect_equal(session$status, "error")
    expect_equal(session$error, "The assistant connection failed")
})

test_that("a turn producing more than the total byte cap is stopped", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(session, paste0(strrep("y", CHAT_MAX_TURN_BYTES + 1L), "\n"))
    expect_equal(session$status, "error")
    expect_equal(session$error, "The assistant produced too much output")
})

test_that("an aborted turn still settles, keeping whatever text arrived", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(
        session,
        '{"type":"message_update","assistantMessageEvent":{"type":"text_delta","delta":"half"}}\n'
    )
    session$aborting <- TRUE
    chat_feed_stdout(session, '{"type":"agent_settled"}\n')
    expect_equal(session$status, "idle")
    expect_equal(session$transcript[[1]]$text, "half")
})

test_that("an aborted turn with no text settles as a timeout notice", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    session$aborting <- TRUE
    chat_feed_stdout(session, '{"type":"agent_settled"}\n')
    expect_equal(session$transcript[[1]]$role, "error")
    expect_equal(session$transcript[[1]]$error, "The answer took too long and was stopped")
})

test_that("an extension failure is terminal (the tool allowlist cannot be trusted after it)", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    chat_feed_stdout(session, '{"type":"extension_error","extensionPath":"x","event":"tool_call","error":"boom"}\n')
    expect_equal(session$status, "error")
})

test_that("the turn deadline sends abort once, then fails after the grace period", {
    session <- new_test_session()
    chat_start_turn(session, "hello")
    now <- session$turn_started_at + CHAT_TURN_TIMEOUT_SECONDS + 1
    chat_enforce_deadlines(session, now)
    expect_true(session$aborting)
    expect_equal(session$last_command$type, "abort")
    chat_enforce_deadlines(session, now + 1)
    expect_equal(session$status, "streaming")
    chat_enforce_deadlines(session, now + CHAT_ABORT_GRACE_SECONDS + 1)
    expect_equal(session$status, "error")
    expect_equal(session$error, "The answer took too long and was stopped")
})

test_that("the transcript is capped", {
    session <- new_test_session()
    for (i in seq_len(CHAT_TRANSCRIPT_MESSAGES + 5L)) {
        chat_push_message(session, "user", sprintf("m%d", i))
    }
    expect_length(session$transcript, CHAT_TRANSCRIPT_MESSAGES)
    expect_equal(session$transcript[[1]]$text, "m6")
})
