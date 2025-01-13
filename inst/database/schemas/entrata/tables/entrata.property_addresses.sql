CREATE TABLE entrata.property_addresses (
  property_id TEXT NOT NULL REFERENCES entrata.properties(property_id),
  address_type TEXT NOT NULL DEFAULT 'Primary' CHECK (address_type IN ('Primary', 'Mailing', 'Billing', 'Other')),
  address TEXT,
  street TEXT,
  city TEXT,
  state VARCHAR(2),
  postal_code VARCHAR(10),
  country VARCHAR(2) DEFAULT 'US',
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (property_id, address_type)
);
