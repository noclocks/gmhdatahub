DROP TABLE IF EXISTS survey.amenities CASCADE;

CREATE TABLE survey.amenities (
  amenity_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  amenity_name TEXT UNIQUE NOT NULL,
  amenity_type TEXT NOT NULL CHECK (amenity_type IN ('property', 'unit')),
  amenity_category TEXT CHECK (amenity_category IN ('general', 'tv', 'furniture', 'other')),
  amenity_description TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id)
);

COMMENT ON TABLE survey.amenities IS 'Amenities: Represents amenities available at a property or unit';
COMMENT ON COLUMN survey.amenities.amenity_id IS 'Amenity ID';
COMMENT ON COLUMN survey.amenities.amenity_name IS 'Amenity Name';
COMMENT ON COLUMN survey.amenities.amenity_type IS 'Amenity Type: Property or Unit';
COMMENT ON COLUMN survey.amenities.amenity_category IS 'Amenity Category';
COMMENT ON COLUMN survey.amenities.amenity_description IS 'Amenity Description';
COMMENT ON COLUMN survey.amenities.created_at IS 'Created At';
COMMENT ON COLUMN survey.amenities.updated_at IS 'Updated At';
COMMENT ON COLUMN survey.amenities.created_by IS 'Created By';
COMMENT ON COLUMN survey.amenities.updated_by IS 'Updated By';
