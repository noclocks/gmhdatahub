DROP TABLE IF EXISTS survey.floorplans CASCADE;

CREATE TABLE IF NOT EXISTS survey.floorplans (
    floorplan_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER REFERENCES survey.properties(property_id),
    floorplan_type TEXT NOT NULL CHECK (floorplan_type IN ('Studio', '1 Bedroom', '2 Bedroom', '3 Bedroom', '4 Bedroom', '5 Bedroom', '6 Bedroom')),
    floorplan_name TEXT NOT NULL,
    floorplan_description TEXT,
    number_of_bedrooms INTEGER,
    number_of_bathrooms INTEGER,
    square_footage INTEGER,
    image_url TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (property_id, floorplan_name)
);

