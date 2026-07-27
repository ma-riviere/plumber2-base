# API-key machinery: generation, hashing, lookup, revocation. Key format is
# pbk_<64 hex chars> (32 random bytes = 256 bits of entropy), stored as a
# SHA-256 hash. SHA-256 (not argon2id) is a deliberate, reviewer-confirmed
# choice: with 256-bit random secrets offline dictionary attacks are infeasible,
# and a slow hash would only add per-request CPU (a DoS vector). The comparison
# is constant-time.

API_KEY_PATTERN <- "^pbk_[0-9a-f]{64}$"
API_KEY_PREFIX_CHARS <- 8L

generate_api_key <- function() {
    paste0("pbk_", sodium::bin2hex(openssl::rand_bytes(32)))
}

api_key_prefix <- function(secret) {
    substr(secret, 1L, API_KEY_PREFIX_CHARS)
}

hash_api_key <- function(secret) {
    as.raw(openssl::sha256(charToRaw(secret)))
}

# Compares every byte regardless of where a mismatch occurs.
constant_time_equal <- function(a, b) {
    if (length(a) != length(b)) {
        return(FALSE)
    }
    sum(bitwXor(as.integer(a), as.integer(b))) == 0L
}

# Issue a key. `scopes` must already be bounded by the caller (endpoint policy,
# Phase 4). The full secret is returned ONCE here and never stored.
create_api_key <- function(pool, user_id, name, scopes = character(), expires_at = NULL) {
    secret <- generate_api_key()
    row <- DBI::dbGetQuery(
        pool,
        "INSERT INTO api_keys (user_id, name, key_prefix, key_hash, scopes, expires_at)
         VALUES ($1, $2, $3, $4, $5::text[], $6) RETURNING id, created_at",
        params = list(
            user_id,
            name,
            api_key_prefix(secret),
            list(hash_api_key(secret)),
            pg_text_array_literal(scopes),
            expires_at %||% NA
        )
    )
    list(
        id = row$id,
        secret = secret,
        key_prefix = api_key_prefix(secret),
        scopes = scopes,
        created_at = row$created_at
    )
}

# Resolve a presented secret to its active key record, or NULL. Lookup is by
# prefix (indexed); every candidate's hash is compared in constant time. The
# users join drops keys of a banned/deleted owner, so the guard itself fails
# (401) instead of authenticating a principal request_principal would then 403.
lookup_api_key <- function(pool, secret) {
    if (is.null(pool) || !is.character(secret) || length(secret) != 1 || !grepl(API_KEY_PATTERN, secret)) {
        return(NULL)
    }
    candidates <- tryCatch(
        DBI::dbGetQuery(
            pool,
            "SELECT k.id, k.user_id, k.name, k.key_hash, k.scopes FROM api_keys k
             JOIN users u ON u.id = k.user_id
             WHERE k.key_prefix = $1 AND k.revoked_at IS NULL
               AND (k.expires_at IS NULL OR k.expires_at > now())
               AND u.status = 'active'",
            params = list(api_key_prefix(secret))
        ),
        error = function(e) NULL
    )
    if (is.null(candidates) || nrow(candidates) == 0) {
        return(NULL)
    }
    presented_hash <- hash_api_key(secret)
    for (i in seq_len(nrow(candidates))) {
        if (constant_time_equal(candidates$key_hash[[i]], presented_hash)) {
            return(list(
                id = candidates$id[[i]],
                user_id = candidates$user_id[[i]],
                name = candidates$name[[i]],
                scopes = parse_pg_text_array(candidates$scopes[[i]])
            ))
        }
    }
    NULL
}

# Mark last_used_at; fire-and-forget (an error here must never fail the request).
# Throttled by an in-process cache: a hot key is written at most once per window,
# so a burst of authenticated requests does not turn every read into a blocking
# UPDATE on the single R thread (write amplification / row churn). The precision
# of last_used_at only needs to be "roughly now", so a per-minute write is ample.
key_touch_state <- new.env(parent = emptyenv())
KEY_TOUCH_THROTTLE_SECONDS <- 60

reset_key_touch_cache <- function() {
    rm(list = ls(key_touch_state), envir = key_touch_state)
    invisible()
}

touch_api_key <- function(pool, key_id, throttle_seconds = KEY_TOUCH_THROTTLE_SECONDS) {
    now <- as.numeric(Sys.time())
    cache_key <- as.character(key_id)
    last <- key_touch_state[[cache_key]]
    if (!is.null(last) && now - last < throttle_seconds) {
        return(invisible())
    }
    key_touch_state[[cache_key]] <- now
    try(
        DBI::dbExecute(pool, "UPDATE api_keys SET last_used_at = now() WHERE id = $1", params = list(key_id)),
        silent = TRUE
    )
    invisible()
}

# Drop every key of a user (ban / account deletion). A HARD delete, not a
# revoke: request_log.api_key_id is a bare bigint with no foreign key, so the
# usage history survives. Returns the number of rows removed.
delete_all_user_keys <- function(pool, user_id) {
    DBI::dbExecute(pool, "DELETE FROM api_keys WHERE user_id = $1", params = list(user_id))
}

# Revoke a key. Scoped to the owning user; returns TRUE if a live key was revoked.
revoke_api_key <- function(pool, user_id, key_id) {
    updated <- DBI::dbExecute(
        pool,
        "UPDATE api_keys SET revoked_at = now() WHERE id = $1 AND user_id = $2 AND revoked_at IS NULL",
        params = list(key_id, user_id)
    )
    updated > 0
}
