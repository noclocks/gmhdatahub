DROP TABLE IF EXISTS survey.properties;

CREATE TABLE IF NOT EXISTS survey.properties(
  property_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  property_name TEXT NOT NULL UNIQUE,
  property_type TEXT NOT NULL,
  property_description TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.properties IS 'Properties: Represents properties to be surveyed';
COMMENT ON COLUMN survey.properties.property_id IS 'Primary Key';
COMMENT ON COLUMN survey.properties.property_name IS 'Property Name';
COMMENT ON COLUMN survey.properties.property_type IS 'Property Type';
COMMENT ON COLUMN survey.properties.property_description IS 'Property Description';
COMMENT ON COLUMN survey.properties.created_at IS 'Created At';

