-- Small key/value store for cross-restart sync positions. Currently holds the
-- Auth0 Events API cursor ('auth0_events_cursor') so a redeploy resumes the
-- stream where it stopped instead of replaying or skipping events.
CREATE TABLE sync_state (
    key        text PRIMARY KEY,
    value      text NOT NULL,
    updated_at timestamptz NOT NULL DEFAULT now()
);
