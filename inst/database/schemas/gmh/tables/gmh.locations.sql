/* gmh.location table */

DROP TABLE IF EXISTS gmh.locations CASCADE;

CREATE TABLE IF NOT EXISTS gmh.locations (
    location_id         INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    location_name       TEXT NOT NULL,
    entity_type         TEXT NOT NULL CHECK (location_type IN ('property', 'competitor', 'university')),
    entity_id           INTEGER NOT NULL,
    address             TEXT NOT NULL,
    city                TEXT NOT NULL,
    state               TEXT NOT NULL,
    postal_code         TEXT NOT NULL,
    country             TEXT NOT NULL DEFAULT 'USA',
    latitude            DECIMAL(9,6),
    longitude           DECIMAL(9,6),
    geom                GEOMETRY(Point, 4326) GENERATED ALWAYS AS (ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)) STORED,
    phone_number        TEXT,
    email               TEXT,
    website             TEXT,
    image_url           TEXT,
    rating              DECIMAL(2,1) CHECK (rating >= 0 AND rating <= 5),
    gmaps_url           TEXT,
    gmaps_place_id      TEXT,
    gmaps_rating        DECIMAL(2,1) CHECK (gmaps_rating >= 0 AND gmaps_rating <= 5),
    gmaps_reviews_count INTEGER,
    map_layer           TEXT,
    map_marker_icon     TEXT,
    map_marker_color    TEXT,
    map_popup_html      TEXT,
    is_active           BOOLEAN NOT NULL DEFAULT TRUE,
    created_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE (entity_type, entity_id)
);

COMMENT ON TABLE gmh.locations IS 'Locations consolidated into a central table for all address/coordinate data.';
COMMENT ON COLUMN gmh.locations.location_id IS 'Unique identifier for the location.';
COMMENT ON COLUMN gmh.locations.location_name IS 'Name of the location.';
COMMENT ON COLUMN gmh.locations.address IS 'Street address of the location.';
COMMENT ON COLUMN gmh.locations.city IS 'City of the location.';
COMMENT ON COLUMN gmh.locations.state IS 'State of the location.';
COMMENT ON COLUMN gmh.locations.postal_code IS 'Postal code of the location.';
COMMENT ON COLUMN gmh.locations.country IS 'Country of the location.';
COMMENT ON COLUMN gmh.locations.latitude IS 'Latitude of the location.';
COMMENT ON COLUMN gmh.locations.longitude IS 'Longitude of the location.';
COMMENT ON COLUMN gmh.locations.geom IS 'PostGIS geometry point for the location.';
COMMENT ON COLUMN gmh.locations.phone_number IS 'Phone number of the location.';
COMMENT ON COLUMN gmh.locations.email IS 'Email address of the location.';
COMMENT ON COLUMN gmh.locations.website IS 'Website URL of the location.';
COMMENT ON COLUMN gmh.locations.image_url IS 'Image URL of the location.';
COMMENT ON COLUMN gmh.locations.rating IS 'Rating of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_url IS 'Google Maps URL of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_place_id IS 'Google Maps Place ID of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_rating IS 'Google Maps rating of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_reviews_count IS 'Google Maps reviews count of the location.';
COMMENT ON COLUMN gmh.locations.map_layer IS 'Map layer of the location.';
COMMENT ON COLUMN gmh.locations.map_marker_icon IS 'Map marker icon of the location.';
COMMENT ON COLUMN gmh.locations.map_marker_color IS 'Map marker color of the location.';
COMMENT ON COLUMN gmh.locations.map_popup_html IS 'Map popup HTML of the location.';
COMMENT ON COLUMN gmh.locations.is_active IS 'Is the location active?';
COMMENT ON COLUMN gmh.locations.created_at IS 'Timestamp when the location was created.';
COMMENT ON COLUMN gmh.locations.updated_at IS 'Timestamp when the location was last updated.';
