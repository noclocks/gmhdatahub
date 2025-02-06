DROP TABLE IF EXISTS survey.users CASCADE;

CREATE TABLE survey.users (
    user_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_email TEXT UNIQUE NOT NULL,
    user_name TEXT,
    user_role TEXT,
    tenant_id UUID DEFAULT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
