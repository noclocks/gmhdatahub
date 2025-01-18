DROP TABLE IF EXISTS gmh.universities;

CREATE TABLE gmh.universities (
    university_id        INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    university_name      TEXT NOT NULL,
    university_website   TEXT,
    university_address   TEXT,
    university_image_url TEXT,
    property_id          INTEGER NOT NULL REFERENCES gmh.properties(property_id),
    created_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.universities IS 'Universities represent local colleges or universities.';
COMMENT ON COLUMN gmh.universities.university_id IS 'Unique identifier for the university.';
COMMENT ON COLUMN gmh.universities.university_name IS 'Name of the university.';
COMMENT ON COLUMN gmh.universities.university_website IS 'Website URL for the university.';
COMMENT ON COLUMN gmh.universities.university_address IS 'Physical address of the university.';
COMMENT ON COLUMN gmh.universities.university_image_url IS 'URL for the university logo or image.';
COMMENT ON COLUMN gmh.universities.property_id IS 'Property ID that the university is associated with.';
COMMENT ON COLUMN gmh.universities.created_at IS 'Date and time when the university was created.';
COMMENT ON COLUMN gmh.universities.updated_at IS 'Date and time when the university was last updated.';
