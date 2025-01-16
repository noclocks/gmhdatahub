DROP TABLE IF EXISTS gmh.properties CASCADE;

CREATE TABLE gmh.properties (
  property_id          INTEGER PRIMARY KEY, -- No autoincrement here, will leverage entrata property_id
  property_name        TEXT NOT NULL,
  property_type        TEXT NOT NULL DEFAULT 'Apartment',
  property_status      TEXT NOT NULL DEFAULT 'Active',
  property_description TEXT,
  property_website     TEXT,
  portfolio_id         INTEGER NOT NULL REFERENCES gmh.portfolios(portfolio_id),
  location_id          INTEGER NOT NULL REFERENCES gmh.locations(location_id),
  created_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.properties IS 'Properties represent individual apartment communities.';
COMMENT ON COLUMN gmh.properties.property_id IS 'Unique identifier for the property.';
COMMENT ON COLUMN gmh.properties.property_name IS 'Name of the property.';
COMMENT ON COLUMN gmh.properties.property_type IS 'Type of property (Apartment, Office, Retail, etc.).';
COMMENT ON COLUMN gmh.properties.property_status IS 'Status of the property (Active, Inactive, etc.).';
COMMENT ON COLUMN gmh.properties.property_description IS 'Description of the property.';
COMMENT ON COLUMN gmh.properties.property_url IS 'URL for the property.';
COMMENT ON COLUMN gmh.properties.portfolio_id IS 'Portfolio ID for the property.';
COMMENT ON COLUMN gmh.properties.location_id IS 'Location ID for the property.';
COMMENT ON COLUMN gmh.properties.created_at IS 'Timestamp when the property was created.';
COMMENT ON COLUMN gmh.properties.updated_at IS 'Timestamp when the property was last updated.';
