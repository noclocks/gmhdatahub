DROP TABLE IF EXISTS survey.unit_amenities_rates_premiums CASCADE;

CREATE TABLE survey.unit_amenities_rates_premiums (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  amenity_id INTEGER REFERENCES survey.amenities(amenity_id),
  amenity_name TEXT NOT NULL,
  amenity_value NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id),
  PRIMARY KEY (property_name, amenity_id),
    CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);

