DROP TABLE IF EXISTS survey.utilities CASCADE;

CREATE TABLE survey.utilities (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  utility_name TEXT NOT NULL,
  utility_category TEXT NOT NULL,
  utility_per TEXT CHECK (utility_per IN ('Unit', 'Bed')),
  utility_available BOOLEAN NOT NULL,
  utility_included BOOLEAN NOT NULL,
  utility_capped BOOLEAN NOT NULL,
  utility_allowance NUMERIC(10,2) NOT NULL,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id),
  PRIMARY KEY (property_name, utility_name),
    CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);
