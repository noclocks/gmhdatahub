CREATE TABLE entrata.property_phones (
  property_id TEXT NOT NULL REFERENCES entrata.properties(property_id),
  phone_number TEXT NOT NULL,
  phone_number_type TEXT,
  phone_number_intl TEXT,
  phone_number_link TEXT,
  phone_number_link_html TEXT,
  PRIMARY KEY (property_id, phone_number_type)
);
