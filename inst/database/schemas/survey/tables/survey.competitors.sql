DROP TABLE IF EXISTS survey.competitors CASCADE;

CREATE TABLE IF NOT EXISTS survey.competitors (
  competitor_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  property_id INTEGER NOT NULL REFERENCES survey.properties(property_id),
  competitor_name TEXT NOT NULL,
  competitor_website TEXT,
  competitor_image_url TEXT,
  competitor_description TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
