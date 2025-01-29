DROP TABLE IF EXISTS survey.amenities CASCADE;

CREATE TABLE survey.amenities (
  amenity_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  amenity_type TEXT NOT NULL CHECK (amenity_type IN ('Property', 'Unit')),
  amenity_category TEXT,
  amenity_name TEXT NOT NULL,
  amenity_icon TEXT,
  amenity_description TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id)
);

