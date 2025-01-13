DROP TABLE IF EXISTS gmh.universities;

CREATE TABLE gmh.universities (
    university_id       INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    university_name     TEXT NOT NULL,
    university_address  TEXT NOT NULL,
    university_url      TEXT,
    property_id         INT NOT NULL REFERENCES gmh.properties(property_id),
    location_id         INT REFERENCES gmh.locations(location_id),
    created_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.universities IS 'Universities represent local colleges or universities.';
COMMENT ON COLUMN gmh.universities.university_id IS 'Unique identifier for the university.';
COMMENT ON COLUMN gmh.universities.university_name IS 'Name of the university.';
COMMENT ON COLUMN gmh.universities.university_address IS 'Address of the university.';
COMMENT ON COLUMN gmh.universities.university_url IS 'URL for the university.';
COMMENT ON COLUMN gmh.universities.property_id IS 'Property ID for the university.';
COMMENT ON COLUMN gmh.universities.location_id IS 'Location ID for the university.';
COMMENT ON COLUMN gmh.universities.created_at IS 'Timestamp when the university was created.';
COMMENT ON COLUMN gmh.universities.updated_at IS 'Timestamp when the university was last updated.';
