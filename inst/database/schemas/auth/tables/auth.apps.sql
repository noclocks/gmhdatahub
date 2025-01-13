-- Apps: Represents applications tied to tenants
CREATE TABLE auth.apps (
    app_id SERIAL PRIMARY KEY,
    tenant_id INT REFERENCES auth.tenants(tenant_id) ON DELETE CASCADE,
    name TEXT NOT NULL,
    api_key TEXT UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
