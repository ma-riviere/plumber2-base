-- Request log for admin stats and per-key usage. No foreign keys: log rows must
-- survive deletion of the referenced user or key.
CREATE TABLE request_log (
    id          bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ts          timestamptz NOT NULL DEFAULT now(),
    service     text NOT NULL,
    method      text NOT NULL,
    path        text NOT NULL,
    status      integer NOT NULL,
    user_id     bigint,
    api_key_id  bigint,
    duration_ms integer
);

CREATE INDEX request_log_ts_idx ON request_log (ts);
