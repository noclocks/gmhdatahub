DROP TABLE IF EXISTS gmh.properties CASCADE;

CREATE TABLE gmh.properties (
  property_id           INTEGER PRIMARY KEY, --using entrata property ids here
  property_name         TEXT NOT NULL,
  parent_property_id    INTEGER REFERENCES gmh.properties(property_id),
  property_type         TEXT NOT NULL DEFAULT 'Student',
  property_status       TEXT NOT NULL DEFAULT 'Active',
  property_website      TEXT,
  property_email        TEXT,
  property_phone_number TEXT,
  property_address      TEXT,
  property_image_url    TEXT,
  property_description  TEXT,
  portfolio_id          INTEGER NOT NULL REFERENCES gmh.portfolios(portfolio_id),
  partner_id            INTEGER NOT NULL REFERENCES gmh.partners(partner_id),
  created_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.properties IS 'Properties represent individual apartment communities.';
COMMENT ON COLUMN gmh.properties.property_id IS 'Unique identifier for the property.';
COMMENT ON COLUMN gmh.properties.property_name IS 'Name of the property.';
COMMENT ON COLUMN gmh.properties.parent_property_id IS 'Reference to the parent property, if applicable.';
COMMENT ON COLUMN gmh.properties.property_type IS 'Type of property (e.g. Apartment, Condo, etc.).';
COMMENT ON COLUMN gmh.properties.property_status IS 'Status of the property (e.g. Active, Inactive, etc.).';
COMMENT ON COLUMN gmh.properties.property_website IS 'Website for the property.';
COMMENT ON COLUMN gmh.properties.property_email IS 'Email address for the property.';
COMMENT ON COLUMN gmh.properties.property_phone_number IS 'Phone number for the property.';
COMMENT ON COLUMN gmh.properties.property_address IS 'Address for the property.';
COMMENT ON COLUMN gmh.properties.property_image_url IS 'URL for the property image.';
COMMENT ON COLUMN gmh.properties.property_description IS 'Description of the property.';
COMMENT ON COLUMN gmh.properties.portfolio_id IS 'Reference to the portfolio that the property belongs to.';
COMMENT ON COLUMN gmh.properties.partner_id IS 'Reference to the partner that the property belongs to.';
COMMENT ON COLUMN gmh.properties.created_at IS 'Timestamp when the property was created.';
COMMENT ON COLUMN gmh.properties.updated_at IS 'Timestamp when the property was last updated.';
