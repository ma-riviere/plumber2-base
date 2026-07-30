# users.status (the cross-app ban authority, shared with shiny-base): request
# enforcement, key deletion, the admin status endpoint's guard branches and the
# Auth0 events poller's handlers.

fake_ban_client <- function(fail = FALSE) {
    calls <- new.env(parent = emptyenv())
    guard <- function() {
        if (fail) {
            stop("auth0 unreachable")
        }
    }
    list(
        calls = calls,
        set_user_blocked = function(sub, blocked) {
            guard()
            calls$blocked <- c(calls$blocked, sprintf("%s=%s", sub, isTRUE(blocked)))
            invisible()
        }
    )
}

put_status <- function(ctx, headers, user_id, body) {
    do_request(
        ctx$pa,
        sprintf("http://t/v1/admin/users/%d/status", as.integer(user_id)),
        method = "put",
        headers = c(headers, list(Content_Type = "application/json")),
        content = body
    )
}

seed_user <- function(pool, sub, status = "active") {
    DBI::dbGetQuery(
        pool,
        "INSERT INTO users (auth0_sub, status, last_seen_at) VALUES ($1, $2, now()) RETURNING id",
        params = list(sub, status)
    )$id
}

# ---- enforcement --------------------------------------------------------------

test_that("a banned user's JWT is refused with 403 account_banned", {
    ctx <- auth_api()
    banned <- bearer_header(sign_access_token(ctx$fixture, roles = "user", sub = "auth0|bad"))
    expect_equal(do_request(ctx$pa, "http://t/v1/me", headers = banned)$status, 200L)

    DBI::dbExecute(ctx$pool, "UPDATE users SET status = 'banned' WHERE auth0_sub = 'auth0|bad'")
    res <- do_request(ctx$pa, "http://t/v1/me", headers = banned)

    expect_equal(res$status, 403L)
    expect_match(res$body, "account_banned")
})

test_that("a valid key whose owner is banned no longer authenticates", {
    ctx <- auth_api()
    user_id <- seed_user(ctx$pool, "auth0|keyowner")
    key <- create_api_key(ctx$pool, user_id, "k", scopes = "write:datasets")
    expect_equal(do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = key$secret))$status, 200L)

    DBI::dbExecute(ctx$pool, "UPDATE users SET status = 'banned' WHERE id = $1", params = list(user_id))

    # The key guard itself fails (fireproof's key contract answers 403), so the
    # request never reaches request_principal's status check.
    expect_null(lookup_api_key(ctx$pool, key$secret))
    expect_equal(do_request(ctx$pa, "http://t/v1/me", headers = list(X_API_Key = key$secret))$status, 403L)
})

test_that("run_maintenance deletes the keys of users banned elsewhere (shiny-base)", {
    pool <- local_migrated_pool()
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL))
    reset_rate_limits()
    withr::defer(reset_rate_limits())
    active_id <- seed_user(pool, "auth0|active")
    banned_id <- seed_user(pool, "auth0|elsewhere", status = "banned")
    create_api_key(pool, active_id, "keep")
    create_api_key(pool, banned_id, "drop")

    run_maintenance(list(request_log_retention_days = 30L))

    expect_equal(DBI::dbGetQuery(pool, "SELECT user_id FROM api_keys")$user_id, active_id)
})

# ---- PUT /v1/admin/users/{id}/status ------------------------------------------

test_that("the status endpoint gates on manage:admin:users and refuses unknown ids", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_ban_client())
    dev <- bearer_header(sign_access_token(ctx$fixture, roles = "dev", sub = "auth0|dev"))
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))

    expect_equal(put_status(ctx, dev, 1L, '{"status": "banned"}')$status, 403L)
    expect_equal(put_status(ctx, admin, 99999L, '{"status": "banned"}')$status, 404L)
})

test_that("the status endpoint refuses invalid values, guests and self-bans", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_ban_client())
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)
    root_id <- DBI::dbGetQuery(ctx$pool, "SELECT id FROM users WHERE auth0_sub = 'auth0|root'")$id
    bob_id <- seed_user(ctx$pool, "auth0|bob")
    guest_id <- DBI::dbGetQuery(
        ctx$pool,
        "INSERT INTO users (nickname, is_guest, last_seen_at) VALUES ('guest', true, now()) RETURNING id"
    )$id

    # 'deleted' is reserved for the events poller / erasure, not settable here.
    expect_equal(put_status(ctx, admin, bob_id, '{"status": "deleted"}')$status, 422L)
    expect_equal(put_status(ctx, admin, bob_id, '{"status": "nope"}')$status, 422L)
    # A non-scalar status must not reach the membership test (length-2 condition).
    expect_equal(put_status(ctx, admin, bob_id, '{"status": ["banned", "active"]}')$status, 422L)
    expect_equal(put_status(ctx, admin, bob_id, "{}")$status, 422L)
    expect_equal(put_status(ctx, admin, guest_id, '{"status": "banned"}')$status, 422L)
    expect_equal(put_status(ctx, admin, root_id, '{"status": "banned"}')$status, 409L)
    expect_equal(
        DBI::dbGetQuery(ctx$pool, "SELECT status FROM users WHERE id = $1", params = list(root_id))$status,
        "active"
    )
})

test_that("banning writes the status, deletes the keys and blocks the Auth0 user", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    client <- fake_ban_client()
    set_mgmt_client(client)
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)
    bob_id <- seed_user(ctx$pool, "auth0|bob")
    create_api_key(ctx$pool, bob_id, "bob-key")

    res <- put_status(ctx, admin, bob_id, '{"status": "banned"}')
    body <- yyjsonr::read_json_str(res$body, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE)

    expect_equal(res$status, 200L)
    expect_equal(body$status, "banned")
    expect_true(body$auth0_synced)
    expect_equal(client$calls$blocked, "auth0|bob=TRUE")
    expect_equal(
        nrow(DBI::dbGetQuery(ctx$pool, "SELECT id FROM api_keys WHERE user_id = $1", params = list(bob_id))),
        0L
    )
})

test_that("unbanning restores login but not the keys, and an Auth0 failure never rolls back", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_ban_client(fail = TRUE))
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)
    bob_id <- seed_user(ctx$pool, "auth0|bob")
    create_api_key(ctx$pool, bob_id, "bob-key")

    banned <- put_status(ctx, admin, bob_id, '{"status": "banned"}')
    banned_body <- yyjsonr::read_json_str(banned$body, arr_of_objs_to_df = FALSE, obj_of_arrs_to_df = FALSE)
    expect_equal(banned$status, 200L)
    expect_false(banned_body$auth0_synced)
    expect_equal(
        DBI::dbGetQuery(ctx$pool, "SELECT status FROM users WHERE id = $1", params = list(bob_id))$status,
        "banned"
    )

    expect_equal(put_status(ctx, admin, bob_id, '{"status": "active"}')$status, 200L)
    expect_equal(
        DBI::dbGetQuery(ctx$pool, "SELECT status FROM users WHERE id = $1", params = list(bob_id))$status,
        "active"
    )
    expect_equal(
        nrow(DBI::dbGetQuery(ctx$pool, "SELECT id FROM api_keys WHERE user_id = $1", params = list(bob_id))),
        0L
    )
})

test_that("a deleted account cannot be resurrected by an unban", {
    ctx <- auth_api()
    withr::defer(reset_mgmt_state())
    reset_mgmt_state()
    set_mgmt_client(fake_ban_client())
    admin <- bearer_header(sign_access_token(ctx$fixture, roles = "admin", sub = "auth0|root"))
    do_request(ctx$pa, "http://t/v1/me", headers = admin)
    gone_id <- seed_user(ctx$pool, "auth0|gone", status = "deleted")

    expect_equal(put_status(ctx, admin, gone_id, '{"status": "active"}')$status, 409L)
    expect_equal(put_status(ctx, admin, gone_id, '{"status": "banned"}')$status, 409L)
    expect_equal(
        DBI::dbGetQuery(ctx$pool, "SELECT status FROM users WHERE id = $1", params = list(gone_id))$status,
        "deleted"
    )
})

# ---- Auth0 events poller ------------------------------------------------------

events_frame <- function(offset, type = NULL, object = NULL) {
    payload <- list(offset = offset)
    if (!is.null(type)) {
        payload$event <- list(id = "evt_1", type = type, time = "2026-07-27T00:00:00Z", data = list(object = object))
    }
    list(
        type = type %||% "offset-only",
        data = yyjsonr::write_json_str(payload, auto_unbox = TRUE),
        id = offset
    )
}

test_that("event handlers mirror Auth0 bans and deletions onto users.status", {
    pool <- local_migrated_pool()
    reset_events_state()
    withr::defer(reset_events_state())
    bob_id <- seed_user(pool, "auth0|bob")
    create_api_key(pool, bob_id, "bob-key")
    status_of <- function(id) DBI::dbGetQuery(pool, "SELECT status FROM users WHERE id = $1", params = list(id))$status

    handle_events_frame(pool, events_frame("o1", "user.updated", list(user_id = "auth0|bob", blocked = TRUE)))
    expect_equal(status_of(bob_id), "banned")
    # The offset is checkpointed only after the event was applied.
    expect_equal(events_state$offset, "o1")
    expect_equal(nrow(DBI::dbGetQuery(pool, "SELECT id FROM api_keys WHERE user_id = $1", params = list(bob_id))), 0L)

    handle_events_frame(pool, events_frame("o2", "user.updated", list(user_id = "auth0|bob", blocked = FALSE)))
    expect_equal(status_of(bob_id), "active")

    handle_events_frame(pool, events_frame("o3", "user.deleted", list(user_id = "auth0|bob")))
    expect_equal(status_of(bob_id), "deleted")

    # An unblock must never resurrect a deleted account.
    handle_events_frame(pool, events_frame("o4", "user.updated", list(user_id = "auth0|bob", blocked = FALSE)))
    expect_equal(status_of(bob_id), "deleted")
})

test_that("frames advance the cursor; unknown subs and error frames are handled", {
    pool <- local_migrated_pool()
    reset_events_state()
    withr::defer(reset_events_state())

    expect_equal(handle_events_frame(pool, events_frame("cursor-1")), "continue")
    expect_equal(events_state$offset, "cursor-1")

    # A sub with no local user row is skipped silently (shared Auth0 tenant).
    expect_equal(
        handle_events_frame(pool, events_frame("cursor-2", "user.deleted", list(user_id = "auth0|stranger"))),
        "continue"
    )
    expect_equal(events_state$offset, "cursor-2")

    expect_equal(
        handle_events_frame(pool, list(type = "error", data = '{"error":{"code":"invalid_cursor"}}')),
        "reset"
    )
    expect_equal(handle_events_frame(pool, list(type = "error", data = '{"error":{"code":"other"}}')), "stop")
})

test_that("a failed handler aborts the drain without checkpointing its offset", {
    pool <- local_migrated_pool()
    reset_events_state()
    withr::defer(reset_events_state())
    seed_user(pool, "auth0|bob")
    expect_true(handle_events_frame(pool, events_frame("good")) == "continue")

    # A closed pool makes the handler's UPDATE throw (no need for a second
    # migrated schema: the query never reaches a table).
    broken <- dev_pool_or_skip()
    pool::poolClose(broken)
    verdict <- handle_events_frame(broken, events_frame("bad", "user.deleted", list(user_id = "auth0|bob")))

    expect_equal(verdict, "abort")
    expect_equal(events_state$offset, "good")
})

test_that("a cursor-less tick anchors on a from_timestamp so nothing is skipped", {
    pool <- local_migrated_pool()
    reset_events_state()
    withr::defer(reset_events_state())

    start <- events_start_position(pool)
    expect_null(start$cursor)
    expect_match(start$from_timestamp, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
    # A later tick that saw no frame reuses the same anchor, not "latest".
    expect_equal(events_start_position(pool)$from_timestamp, start$from_timestamp)

    reset_events_position(pool)
    expect_null(sync_state_get(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY))
})

test_that("the events cursor round-trips through sync_state", {
    pool <- local_migrated_pool()
    set_app_pool(pool)
    withr::defer(set_app_pool(NULL))
    reset_events_state()
    withr::defer(reset_events_state())

    expect_null(sync_state_get(pool, AUTH0_EVENTS_CURSOR_KEY))
    sync_state_set(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY, "2026-07-27T00:00:00Z")
    events_state$offset <- "cursor-9"
    persist_events_cursor()
    expect_equal(sync_state_get(pool, AUTH0_EVENTS_CURSOR_KEY), "cursor-9")
    # The timestamp anchor is dropped once a real offset exists.
    expect_null(sync_state_get(pool, AUTH0_EVENTS_FROM_TIMESTAMP_KEY))

    # Idempotent: re-persisting the same offset is a no-op, a new one updates.
    persist_events_cursor()
    events_state$offset <- "cursor-10"
    persist_events_cursor()
    expect_equal(sync_state_get(pool, AUTH0_EVENTS_CURSOR_KEY), "cursor-10")
})

test_that("the events sync tick is a no-op (and still chains) without a pool or mgmt config", {
    set_app_pool(NULL)
    done <- 0L
    expect_no_error(run_events_sync(
        list(auth0 = list(domain = "", mgmt_client_id = "", mgmt_client_secret = "")),
        on_done = function() done <<- done + 1L
    ))
    expect_equal(done, 1L)
})
