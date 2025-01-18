DROP TABLE IF EXISTS survey.users CASCADE;

CREATE TABLE survey.users (
    user_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    user_uid UUID,
    user_email TEXT UNIQUE NOT NULL,
    user_name TEXT,
    user_role TEXT,
    tenant_id UUID,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
