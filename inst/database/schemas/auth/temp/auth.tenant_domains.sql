CREATE TABLE auth.tenant_domains (
  domain_id SERIAL PRIMARY KEY,
  tenant_id INT REFERENCES auth.tenants(tenant_id),
  tenant_domain TEXT NOT NULL,
  created_by INT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
