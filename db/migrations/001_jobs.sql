-- Async jobs (e.g. model fitting). gen_random_uuid() is built into Postgres core (>= 13).
-- REFERENCES users stays unqualified so scratch-schema tests keep working; in
-- prod/dev it resolves to shared.users because assert_db_sanity() guarantees no
-- app-local users table exists before migrations run.
CREATE TABLE jobs (
    id         uuid PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id    bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    kind       text NOT NULL,
    status     text NOT NULL DEFAULT 'pending' CHECK (status IN ('pending', 'running', 'done', 'error')),
    payload    jsonb,
    result     jsonb,
    error      text,
    created_at timestamptz NOT NULL DEFAULT now(),
    updated_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX jobs_user_id_status_idx ON jobs (user_id, status);
CREATE INDEX jobs_status_idx ON jobs (status);
