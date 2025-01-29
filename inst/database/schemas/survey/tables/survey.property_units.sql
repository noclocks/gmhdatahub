DROP TABLE IF EXISTS survey.property_units CASCADE;

CREATE TABLE survey.units (
    unit_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    property_name TEXT NOT NULL,
    unit_name TEXT NOT NULL,
    unit_code TEXT,
    unit_description TEXT,
    unit_rating NUMERIC(2,1) CHECK (unit_rating >= 0 AND unit_rating <= 5),
    unit_levels INTEGER,
    square_footage INT,
    number_bedrooms INT,
    number_bathrooms INT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
