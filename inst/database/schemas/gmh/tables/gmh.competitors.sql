-- !preview conn=gmhdatahub::db_connect_and_checkout()

DROP TABLE IF EXISTS gmh.competitors CASCADE;

CREATE TABLE gmh.competitors (
    competitor_id        INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    competitor_name      TEXT NOT NULL UNIQUE,
    competitor_address   TEXT,
    competitor_website   TEXT,
    competitor_image_url TEXT,
    property_id          INTEGER NOT NULL REFERENCES gmh.properties(property_id),
    created_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.competitors IS 'Competitors represent other apartment communities in the area.';
COMMENT ON COLUMN gmh.competitors.competitor_id IS 'Unique identifier for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_name IS 'Name of the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_address IS 'Address for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_website IS 'Website for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_image_url IS 'URL for an image of the competitor.';
COMMENT ON COLUMN gmh.competitors.property_id IS 'The property that the competitor is associated with.';
COMMENT ON COLUMN gmh.competitors.created_at IS 'Timestamp when the competitor was created.';
COMMENT ON COLUMN gmh.competitors.updated_at IS 'Timestamp when the competitor was last updated.';
