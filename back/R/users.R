# User provisioning for authenticated principals. The FE fills profile fields
# (email, nickname) at login; the BE only guarantees a row exists so resources
# can be owned, and keeps last_seen_at fresh.

USER_COLUMNS <- "id, auth0_sub, email, nickname, is_guest, created_at, last_seen_at"

# last_seen_at is refreshed at most once per window per sub. Every JWT request
# needs the user row, but only the first in a window needs to WRITE it: within
# the window a plain SELECT returns the row with no last_seen write, so a burst
# of reads does not amplify into an UPDATE (WAL + row lock) per request on the
# single R thread. The upsert still runs on a cache miss, so first-seen users
# are always provisioned and a user deleted out-of-band is transparently
# recreated.
user_seen_state <- new.env(parent = emptyenv())
USER_SEEN_THROTTLE_SECONDS <- 60

reset_user_seen_cache <- function() {
    rm(list = ls(user_seen_state), envir = user_seen_state)
    invisible()
}

get_or_create_user_by_sub <- function(pool, auth0_sub, throttle_seconds = USER_SEEN_THROTTLE_SECONDS) {
    now <- as.numeric(Sys.time())
    last <- user_seen_state[[auth0_sub]]
    if (!is.null(last) && now - last < throttle_seconds) {
        row <- DBI::dbGetQuery(
            pool,
            sprintf("SELECT %s FROM users WHERE auth0_sub = $1", USER_COLUMNS),
            params = list(auth0_sub)
        )
        if (nrow(row) > 0) {
            return(row)
        }
    }
    row <- DBI::dbGetQuery(
        pool,
        sprintf(
            "INSERT INTO users (auth0_sub, last_seen_at) VALUES ($1, now())
             ON CONFLICT (auth0_sub) DO UPDATE SET last_seen_at = now()
             RETURNING %s",
            USER_COLUMNS
        ),
        params = list(auth0_sub)
    )
    user_seen_state[[auth0_sub]] <- now
    row
}

get_user_by_id <- function(pool, user_id) {
    user <- DBI::dbGetQuery(
        pool,
        sprintf("SELECT %s FROM users WHERE id = $1", USER_COLUMNS),
        params = list(user_id)
    )
    if (nrow(user) == 0) NULL else user
}

# The shared guest user for bypass mode (dev only). Reuses the seeded guest row
# when present, creates one otherwise. The sentinel is `is_guest AND auth0_sub
# IS NULL`: the users table is shared with shiny-base, whose per-session guests
# also carry is_guest = true (with guest_/tmp_ subs) and get deleted by its
# cleanup — matching on bare is_guest could adopt one of those rows and lose
# everything attached to it.
get_or_create_guest <- function(pool) {
    guest <- DBI::dbGetQuery(
        pool,
        sprintf("SELECT %s FROM users WHERE is_guest AND auth0_sub IS NULL ORDER BY id LIMIT 1", USER_COLUMNS)
    )
    if (nrow(guest) > 0) {
        return(guest)
    }
    DBI::dbGetQuery(
        pool,
        sprintf(
            "INSERT INTO users (nickname, is_guest, last_seen_at) VALUES ('guest', true, now()) RETURNING %s",
            USER_COLUMNS
        )
    )
}

# Resolve the user row backing an authenticated principal (see current_principal).
principal_user <- function(pool, principal) {
    switch(
        principal$guard,
        jwt = get_or_create_user_by_sub(pool, principal$info$id),
        api_key = get_user_by_id(pool, as.integer(principal$info$id)),
        bypass = get_or_create_guest(pool),
        NULL
    )
}
