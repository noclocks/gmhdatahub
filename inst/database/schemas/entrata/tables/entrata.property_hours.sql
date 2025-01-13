CREATE TABLE entrata.property_hours (
  property_id TEXT NOT NULL REFERENCES entrata.properties(property_id),
  day_of_week TEXT,
  office_hours TEXT,
  pool_hours TEXT
);
