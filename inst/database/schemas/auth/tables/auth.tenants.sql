-- Tenants: Represents organizations using the system
CREATE TABLE auth.tenants (
    tenant_id SERIAL PRIMARY KEY,
    tenant_name TEXT NOT NULL,
    tenant_domain TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
