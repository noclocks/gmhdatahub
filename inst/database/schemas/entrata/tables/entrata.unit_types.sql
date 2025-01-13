CREATE TABLE entrata.unit_types (
  unit_type_id INTEGER NOT NULL PRIMARY KEY,
  property_id INTEGER NOT NULL REFERENCES entrata.properties(property_id),
  floorplan_id INTEGER NOT NULL REFERENCES entrata.floorplans(floorplan_id),
  unit_type_name TEXT NOT NULL,
  unit_type_descrition TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
