-- !preview conn=db_connect()

DROP TABLE IF EXISTS gmh.competitors;

CREATE TABLE gmh.competitors (
    competitor_id   INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    competitor_name TEXT NOT NULL,
    competitor_url  TEXT,
    property_id     INTEGER NOT NULL REFERENCES gmh.properties(property_id),
    location_id     INTEGER REFERENCES gmh.locations(location_id),
    created_at      TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at      TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.competitors IS 'Competitors represent other apartment communities in the area.';
COMMENT ON COLUMN gmh.competitors.competitor_id IS 'Unique identifier for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_name IS 'Name of the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_url IS 'URL for the competitor.';
COMMENT ON COLUMN gmh.competitors.property_id IS 'Property ID for the competitor.';
COMMENT ON COLUMN gmh.competitors.location_id IS 'Location ID for the competitor.';
COMMENT ON COLUMN gmh.competitors.created_at IS 'Timestamp when the competitor was created.';
COMMENT ON COLUMN gmh.competitors.updated_at IS 'Timestamp when the competitor was last updated.';
