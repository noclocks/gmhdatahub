DROP TABLE IF EXISTS survey.properties;

CREATE TABLE IF NOT EXISTS survey.properties(
  property_id INTEGER PRIMARY KEY,
  property_name TEXT NOT NULL UNIQUE,
  property_type TEXT,
  property_website TEXT,
  property_address TEXT,
  property_phone TEXT,
  property_email TEXT,
  property_image_url TEXT,
  property_description TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
