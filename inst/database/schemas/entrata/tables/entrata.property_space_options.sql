CREATE TABLE entrata.property_space_options (
  property_id TEXT NOT NULL REFERENCES entrata.properties(property_id),
  space_option_id INT NOT NULL,
  space_option_name TEXT NOT NULL,
  PRIMARY KEY (property_id, space_option_id)
);
