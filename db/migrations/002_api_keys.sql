-- Self-managed API keys. key_hash is the SHA-256 of the full secret; key_prefix the first chars for lookup.
CREATE TABLE api_keys (
    id           bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    user_id      bigint NOT NULL REFERENCES users (id) ON DELETE CASCADE,
    name         text NOT NULL,
    key_prefix   text NOT NULL,
    key_hash     bytea NOT NULL,
    scopes       text[] NOT NULL DEFAULT '{}',
    last_used_at timestamptz,
    expires_at   timestamptz,
    revoked_at   timestamptz,
    created_at   timestamptz NOT NULL DEFAULT now(),
    UNIQUE (user_id, name)
);

CREATE INDEX api_keys_key_prefix_idx ON api_keys (key_prefix);
