# Chat endpoints against the in-process api, with the stub `pi` executable
# standing in for the real agent (no Node, no provider key, no network).

# The registry and the daily quota are process-global: without this, one test's
# sessions and turn count leak into the next.
local_clean_registry <- function(env = parent.frame()) {
    rm(list = ls(chat_quota_state), envir = chat_quota_state)
    withr::defer(
        {
            for (key in ls(chat_registry)) {
                chat_cleanup_key(key)
            }
            rm(list = ls(chat_quota_state), envir = chat_quota_state)
        },
        envir = env
    )
}

chat_api <- function(env = parent.frame(), ...) {
    skip_if_no_duckdb()
    local_clean_registry(env = env)
    local_front_api(env = env, chat = utils::modifyList(list(enabled = TRUE, allow_guests = TRUE), list(...)))
}

form_headers <- function(session) {
    action_headers(session, Content_Type = "application/x-www-form-urlencoded")
}

send <- function(pa, session, content = "dataset=1&message=hello") {
    do_request(pa, "http://t/partials/chat/send", method = "post", headers = form_headers(session), content = content)
}

# Pump fiery's later loop (which the supervisor tick runs on) until the stub has
# answered, or the budget runs out.
pump_until <- function(condition, seconds = 10) {
    deadline <- Sys.time() + seconds
    while (Sys.time() < deadline && !isTRUE(condition())) {
        later::run_now(0.05)
    }
    isTRUE(condition())
}

test_that("the widget is absent and every handler refuses when chat is disabled", {
    pa <- chat_api(enabled = FALSE)
    session <- guest_session(pa)
    page <- do_request(pa, "http://t/explore?dataset=1", headers = list(Cookie = session$cookie))
    expect_false(grepl("chat-launcher", page$body, fixed = TRUE))

    res <- send(pa, session)
    expect_equal(res$status, 403L)
    expect_match(res$body, "not available", fixed = TRUE)
})

test_that("guests are denied server-side, not merely hidden", {
    pa <- chat_api(allow_guests = FALSE)
    session <- guest_session(pa)
    page <- do_request(pa, "http://t/explore?dataset=1", headers = list(Cookie = session$cookie))
    expect_false(grepl("chat-launcher", page$body, fixed = TRUE))

    for (res in list(
        send(pa, session),
        do_request(pa, "http://t/partials/chat/stream?chat=x&dataset=1", headers = list(Cookie = session$cookie)),
        do_request(
            pa,
            "http://t/partials/chat/reset",
            method = "post",
            headers = form_headers(session),
            content = "dataset=1"
        )
    )) {
        expect_equal(res$status, 403L)
        expect_match(res$body, "Sign in", fixed = TRUE)
    }
    expect_length(ls(chat_registry), 0L)
})

test_that("an empty or oversized question is refused before a process is spawned", {
    pa <- chat_api()
    session <- guest_session(pa)
    empty <- send(pa, session, content = "dataset=1&message=")
    expect_equal(empty$status, 422L)
    long <- send(pa, session, content = paste0("dataset=1&message=", strrep("a", CHAT_MAX_PROMPT_CHARS + 1L)))
    expect_equal(long$status, 422L)
    expect_match(long$body, "too long", fixed = TRUE)
    expect_length(ls(chat_registry), 0L)
})

test_that("a question spawns the agent, streams and settles into the transcript", {
    pa <- chat_api()
    session <- guest_session(pa)
    res <- send(pa, session)
    expect_equal(res$status, 200L)
    # The polling fragment carries the chat generation id; the form comes back
    # out-of-band so the textarea is cleared without any client-side scripting.
    expect_match(res$body, 'hx-trigger="load delay:1s"', fixed = TRUE)
    expect_match(res$body, 'hx-sync="#chat-stream:drop"', fixed = TRUE)
    expect_match(res$body, 'hx-swap-oob="outerHTML"', fixed = TRUE)
    expect_match(res$body, "hello", fixed = TRUE)
    # The header's model indicator comes back OOB once the session has chosen
    # its provider/model.
    expect_match(res$body, 'id="chat-model-info"', fixed = TRUE)
    expect_match(res$body, 'title="[stub] stub-model"', fixed = TRUE)

    key <- ls(chat_registry)
    expect_length(key, 1L)
    chat <- chat_registry[[key]]
    expect_true(pump_until(function() identical(chat$status, "idle")))
    expect_equal(chat$provider, "stub")
    expect_equal(chat$model, "stub-model")

    poll <- do_request(
        pa,
        sprintf("http://t/partials/chat/stream?chat=%s&dataset=1", chat$id),
        headers = list(Cookie = session$cookie)
    )
    expect_equal(poll$status, 200L)
    expect_match(poll$body, "Stub answer.", fixed = TRUE)
    expect_match(poll$body, "Querying data", fixed = TRUE)
    # Settled: the fragment must no longer ask for another poll.
    expect_false(grepl("hx-trigger", poll$body, fixed = TRUE))
})

test_that("a second send while a turn is in flight is refused with 409", {
    pa <- chat_api()
    session <- guest_session(pa)
    expect_equal(send(pa, session)$status, 200L)
    key <- ls(chat_registry)
    chat_registry[[key]]$status <- "streaming"
    res <- send(pa, session, content = "dataset=1&message=again")
    expect_equal(res$status, 409L)
    expect_match(res$body, "wait for the current answer", fixed = TRUE)
})

test_that("a poll for a stale generation answers 204 so it cannot repaint a newer chat", {
    pa <- chat_api()
    session <- guest_session(pa)
    send(pa, session)
    res <- do_request(
        pa,
        "http://t/partials/chat/stream?chat=c-not-this-one&dataset=1",
        headers = list(Cookie = session$cookie)
    )
    expect_equal(res$status, 204L)
    expect_equal(res$body, "")
})

test_that("New chat runs the cleanup ladder, empties the transcript and forgets the store", {
    pa <- chat_api()
    session <- guest_session(pa)
    send(pa, session)
    key <- ls(chat_registry)
    chat <- chat_registry[[key]]
    workdir <- chat$workdir
    process <- chat$process
    expect_true(dir.exists(workdir))
    expect_true(pump_until(function() identical(chat$status, "idle")))
    expect_true(chat$store$exists(chat$store_key, namespace = CHAT_STORE_NAMESPACE))

    res <- do_request(
        pa,
        "http://t/partials/chat/reset",
        method = "post",
        headers = form_headers(session),
        content = "dataset=1"
    )
    expect_equal(res$status, 200L)
    expect_false(grepl("hello", res$body, fixed = TRUE))
    expect_length(ls(chat_registry), 0L)
    expect_false(dir.exists(workdir))
    expect_false(process$is_alive())
    expect_null(chat$process)
    # The persisted transcript is gone too: the next question starts with no
    # memory.
    expect_false(chat$store$exists(chat$store_key, namespace = CHAT_STORE_NAMESPACE))
})

test_that("a settled chat is restored on the next explore render and replayed on the next send", {
    pa <- chat_api()
    session <- guest_session(pa)
    send(pa, session)
    key <- ls(chat_registry)
    chat <- chat_registry[[key]]
    expect_true(pump_until(function() identical(chat$status, "idle")))
    expect_true(chat$store$exists(chat$store_key, namespace = CHAT_STORE_NAMESPACE))

    # The live session dies (idle sweep, redeploy, logout); a plain page load
    # still shows the persisted conversation, inert - no polling trigger.
    chat_cleanup_key(key)
    page <- do_request(pa, "http://t/explore?dataset=1", headers = list(Cookie = session$cookie))
    expect_match(page$body, "hello", fixed = TRUE)
    expect_match(page$body, "Stub answer.", fixed = TRUE)
    expect_false(grepl("load delay:1s", page$body, fixed = TRUE))

    # The revived subprocess gets the previous dialogue replayed inside its
    # first prompt, and the displayed transcript continues where it ended.
    send(pa, session, content = "dataset=1&message=again")
    revived <- chat_registry[[ls(chat_registry)]]
    expect_true(pump_until(function() identical(revived$status, "idle")))
    prompt <- revived$last_command
    expect_equal(prompt$type, "prompt")
    expect_match(prompt$message, "<previous_conversation>", fixed = TRUE)
    expect_match(prompt$message, "assistant: Stub answer.", fixed = TRUE)
    expect_match(prompt$message, "again", fixed = TRUE)
    texts <- vapply(revived$transcript, function(entry) entry$text %||% "", character(1))
    expect_equal(texts, c("hello", "Stub answer.", "again", "Stub answer."))
})

test_that("logout tears the chat session down", {
    pa <- chat_api()
    session <- guest_session(pa)
    send(pa, session)
    chat <- chat_registry[[ls(chat_registry)]]
    workdir <- chat$workdir

    res <- do_request(pa, "http://t/logout", method = "post", headers = action_headers(session))
    expect_true(res$status %in% c(200L, 302L))
    expect_length(ls(chat_registry), 0L)
    expect_false(dir.exists(workdir))
})

test_that("the idle sweep reaps a session nothing polls any more", {
    pa <- chat_api()
    session <- guest_session(pa)
    send(pa, session)
    key <- ls(chat_registry)
    chat <- chat_registry[[key]]
    workdir <- chat$workdir
    chat$last_poll_at <- as.numeric(Sys.time()) - CHAT_IDLE_SECONDS - 1
    chat_tick()
    expect_length(ls(chat_registry), 0L)
    expect_false(dir.exists(workdir))
})

test_that("switching dataset replaces the session instead of killing it from a GET", {
    pa <- chat_api()
    session <- guest_session(pa)
    send(pa, session)
    first <- chat_registry[[ls(chat_registry)]]
    first_process <- first$process
    expect_true(pump_until(function() identical(first$status, "idle")))

    # A GET for the other dataset must leave the live session alone.
    do_request(pa, "http://t/partials/explore/content?dataset=2", headers = list(Cookie = session$cookie))
    expect_length(ls(chat_registry), 1L)
    expect_true(first_process$is_alive())

    send(pa, session, content = "dataset=2&message=other")
    expect_length(ls(chat_registry), 1L)
    second <- chat_registry[[ls(chat_registry)]]
    expect_false(identical(second$id, first$id))
    expect_equal(second$dataset_id, 2L)
    expect_false(first_process$is_alive())
})

test_that("the concurrent-session cap answers with a busy fragment", {
    pa <- chat_api(max_sessions = 1L)
    session <- guest_session(pa)
    send(pa, session)
    chat_registry[["someone-else"]] <- chat_new_state("someone-else", 9L, 99L, "en")
    chat_cleanup_key(ls(chat_registry)[[1]])

    res <- send(pa, session)
    expect_equal(res$status, 503L)
    expect_match(res$body, "busy", fixed = TRUE)
})

test_that("the daily quota is enforced per user", {
    pa <- chat_api(daily_turns = 1L)
    session <- guest_session(pa)
    expect_equal(send(pa, session)$status, 200L)
    chat_registry[[ls(chat_registry)]]$status <- "idle"
    res <- send(pa, session, content = "dataset=1&message=again")
    expect_equal(res$status, 429L)
    expect_match(res$body, "limit", fixed = TRUE)
})
