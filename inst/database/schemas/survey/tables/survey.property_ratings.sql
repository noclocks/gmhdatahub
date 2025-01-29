DROP TABLE IF EXISTS survey.property_ratings CASCADE;

CREATE TABLE survey.property_ratings (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  property_rating NUMERIC(2, 1) NOT NULL DEFAULT 0 CHECK (property_rating >= 0 AND property_rating <= 5),
  common_area_rating NUMERIC(2, 1) NOT NULL DEFAULT 0 CHECK (common_area_rating >= 0 AND common_area_rating <= 5),
  average_unit_rating NUMERIC(2, 1) NOT NULL DEFAULT 0 CHECK (average_unit_rating >= 0 AND average_unit_rating <= 5),
  PRIMARY KEY (property_name),
  CONSTRAINT chk_property_or_competitor CHECK (
      (property_id IS NOT NULL AND competitor_id IS NULL) OR
      (property_id IS NULL AND competitor_id IS NOT NULL)
  )
);
