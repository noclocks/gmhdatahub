CREATE TABLE entrata.property_units (
  unit_id INTEGER NOT NULL PRIMARY KEY,
  property_id INTEGER NOT NULL REFERENCES entrata.properties(property_id),
  unit_type_id INTEGER NOT NULL REFERENCES entrata.unit_types(unit_type_id),
  unit_number TEXT NOT NULL,
  is_available BOOLEAN NOT NULL DEFAULT TRUE,
  current_rent DECIMAL(10, 2),
  market_rent DECIMAL(10, 2),
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
