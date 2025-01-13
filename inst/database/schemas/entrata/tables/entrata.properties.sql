DROP TABLE IS EXISTS entrata.properties CASCADE;

CREATE TABLE IF NOT EXISTS entrata.properties (
  "property_id" INTEGER PRIMARY KEY,
  "marketing_name" TEXT UNIQUE NOT NULL,
  "property_type" TEXT NOT NULL CHECK (property_type IN ('Student', 'Apartment')),
  "website" TEXT,
  "address" TEXT,
  "email" TEXT,
  "is_disabled" BOOLEAN DEFAULT FALSE,
  "is_featured_property" BOOLEAN DEFAULT FALSE,
  "parent_property_id" INTEGER REFERENCES entrata.properties(property_id),
  "year_built" INTEGER,
  "short_description" TEXT,
  "long_description" TEXT,
  "created_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  "updated_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
