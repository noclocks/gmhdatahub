CREATE SCHEMA IF NOT EXISTS gmh;

CREATE TABLE IF NOT EXISTS gmh.segments (
  segment_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  segment_name TEXT NOT NULL UNIQUE,
  segment_description TEXT,
  segment_url TEXT,
  segment_logo_url TEXT,
  segment_banner_url TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.segments IS 'Segments represent high-level GMH business units or divisions.';
COMMENT ON COLUMN gmh.segments.segment_id IS 'Unique identifier for the segment.';
COMMENT ON COLUMN gmh.segments.segment_name IS 'Name of the segment.';
COMMENT ON COLUMN gmh.segments.segment_description IS 'Description of the segment.';
COMMENT ON COLUMN gmh.segments.segment_url IS 'URL for the segment.';
COMMENT ON COLUMN gmh.segments.segment_logo_url IS 'Logo URL for the segment.';
COMMENT ON COLUMN gmh.segments.segment_banner_url IS 'Banner URL for the segment.';
COMMENT ON COLUMN gmh.segments.created_at IS 'Timestamp when the segment was created.';

CREATE TABLE gmh.partners (
    partner_id INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    partner_name        TEXT NOT NULL,
    partner_type        TEXT NOT NULL DEFAULT 'Equity Partner',  -- or 'Owner'
    partner_description TEXT DEFAULT 'No Description',
    partner_url         TEXT,
    created_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.partners IS 'Partners represent external (or internal) investment or ownership entities.';
COMMENT ON COLUMN gmh.partners.partner_id IS 'Unique identifier for the partner.';
COMMENT ON COLUMN gmh.partners.partner_name IS 'Name of the partner.';
COMMENT ON COLUMN gmh.partners.partner_type IS 'Type of partner (Equity Partner or Owner).';
COMMENT ON COLUMN gmh.partners.partner_description IS 'Description of the partner.';
COMMENT ON COLUMN gmh.partners.partner_url IS 'URL for the partner.';
COMMENT ON COLUMN gmh.partners.created_at IS 'Timestamp when the partner was created.';
COMMENT ON COLUMN gmh.partners.updated_at IS 'Timestamp when the partner was last updated.';

CREATE TABLE gmh.portfolios (
  portfolio_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  portfolio_name        TEXT NOT NULL UNIQUE,
  portfolio_type        TEXT NOT NULL DEFAULT 'Equity Partner',  -- or 'Owner'
  portfolio_description TEXT DEFAULT 'No Description',
  segment_id            INT NOT NULL REFERENCES gmh.segments(segment_id),
  partner_id            INT NOT NULL REFERENCES gmh.partners(partner_id),
  created_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.portfolios IS 'Portfolios represent a collection of properties owned or managed by a partner.';
COMMENT ON COLUMN gmh.portfolios.portfolio_id IS 'Unique identifier for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_name IS 'Name of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_type IS 'Type of portfolio (Equity Partner or Owner).';
COMMENT ON COLUMN gmh.portfolios.portfolio_description IS 'Description of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.segment_id IS 'Segment ID for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.partner_id IS 'Partner ID for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.created_at IS 'Timestamp when the portfolio was created.';
COMMENT ON COLUMN gmh.portfolios.updated_at IS 'Timestamp when the portfolio was last updated.';

/*
If a property is wholly owned by GMH (no external partner), you could either have a “GMH” row in partners or allow partner_id to be nullable with specific logic.
*/

CREATE TABLE IF NOT EXISTS gmh.locations (
    location_id   INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    location_name TEXT NOT NULL,
    address       TEXT NOT NULL,
    city          TEXT NOT NULL,
    state         TEXT NOT NULL,
    postal_code   TEXT NOT NULL,
    country       TEXT NOT NULL DEFAULT 'USA',
    latitude      DECIMAL(9,6),
    longitude     DECIMAL(9,6),
    geom          GEOMETRY(Point, 4326) GENERATED ALWAYS AS (
                     ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)
                   ) STORED,
    phone_number  TEXT,
    email         TEXT,
    website       TEXT,
    image_url     TEXT,
    rating        DECIMAL(2,1) CHECK (rating >= 0 AND rating <= 5),
    gmaps_url     TEXT,
    gmaps_place_id TEXT,
    gmaps_rating  DECIMAL(2,1) CHECK (gmaps_rating >= 0 AND gmaps_rating <= 5),
    gmaps_reviews_count INT,
    gmaps_place_types TEXT[],
    map_layer     TEXT,
    map_marker_icon TEXT,
    map_marker_color TEXT,
    map_popup_html TEXT,
    is_active     BOOLEAN NOT NULL DEFAULT TRUE,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
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
COMMENT ON COLUMN gmh.locations.gmaps_place_types IS 'Google Maps place types of the location.';
COMMENT ON COLUMN gmh.locations.map_layer IS 'Map layer of the location.';
COMMENT ON COLUMN gmh.locations.map_marker_icon IS 'Map marker icon of the location.';
COMMENT ON COLUMN gmh.locations.map_marker_color IS 'Map marker color of the location.';
COMMENT ON COLUMN gmh.locations.map_popup_html IS 'Map popup HTML of the location.';
COMMENT ON COLUMN gmh.locations.is_active IS 'Is the location active?';
COMMENT ON COLUMN gmh.locations.created_at IS 'Timestamp when the location was created.';
COMMENT ON COLUMN gmh.locations.updated_at IS 'Timestamp when the location was last updated.';

CREATE TABLE gmh.properties (
  property_id          INTEGER PRIMARY KEY, -- No autoincrement here, will leverage entrata property_id
  property_name        TEXT NOT NULL,
  property_type        TEXT NOT NULL DEFAULT 'Apartment',
  property_status      TEXT NOT NULL DEFAULT 'Active',
  property_description TEXT,
  property_url         TEXT,
  is_active            BOOLEAN DEFAULT TRUE,
  portfolio_id         INT NOT NULL REFERENCES gmh.portfolios(portfolio_id),
  location_id          INT NOT NULL REFERENCES gmh.locations(location_id),
  created_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.properties IS 'Properties represent individual apartment communities.';
COMMENT ON COLUMN gmh.properties.property_id IS 'Unique identifier for the property.';
COMMENT ON COLUMN gmh.properties.property_name IS 'Name of the property.';
COMMENT ON COLUMN gmh.properties.property_type IS 'Type of property (Apartment, Office, Retail, etc.).';
COMMENT ON COLUMN gmh.properties.property_status IS 'Status of the property (Active, Inactive, etc.).';
COMMENT ON COLUMN gmh.properties.property_description IS 'Description of the property.';
COMMENT ON COLUMN gmh.properties.property_url IS 'URL for the property.';
COMMENT ON COLUMN gmh.properties.is_active IS 'Is the property active?';
COMMENT ON COLUMN gmh.properties.portfolio_id IS 'Portfolio ID for the property.';
COMMENT ON COLUMN gmh.properties.location_id IS 'Location ID for the property.';
COMMENT ON COLUMN gmh.properties.created_at IS 'Timestamp when the property was created.';
COMMENT ON COLUMN gmh.properties.updated_at IS 'Timestamp when the property was last updated.';

CREATE TABLE gmh.competitors (
    competitor_id INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
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

CREATE TABLE gmh.universities (
    university_id INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
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

CREATE TABLE gmh.model_beds (
    property_id      INTEGER PRIMARY KEY REFERENCES gmh.properties(property_id) ON DELETE CASCADE,
    model_bed_count  INT DEFAULT 0,
    notes            TEXT,
    updated_at       TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.model_beds IS 'Model beds represent the number of beds representing models for each property.';
COMMENT ON COLUMN gmh.model_beds.property_id IS 'Property ID for the model beds.';
COMMENT ON COLUMN gmh.model_beds.model_bed_count IS 'Number of model beds for the property.';
COMMENT ON COLUMN gmh.model_beds.notes IS 'Notes for the model beds.';
COMMENT ON COLUMN gmh.model_beds.updated_at IS 'Timestamp when the model beds were last updated.';

/*
CREATE TABLE gmh.lease_term_structures (
  lease_term_structure_id INTEGER PRIMARY KEY,
  lease_term_structure_name TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE gmh.lease_terms (
  lease_term_id INTEGER PRIMARY KEY,
  property_id INTEGER REFERENCES gmh.properties(property_id),
  lease_term_structure_id INTEGER REFERENCES gmh.lease_term_structures(lease_term_structure_id),
  lease_term_name TEXT NOT NULL,
  lease_term_description TEXT,
  lease_term_web_visible BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE gmh.portfolio_goals (
  goal_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  portfolio_id INTEGER REFERENCES gmh.portfolios(portfolio_id),
  portfolio_leases_goal INTEGER,
  portfolio_pre_lease_pct_goal DECIMAL(3,2),
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE gmh.property_goals (
  goal_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  property_id INTEGER REFERENCES gmh.properties(property_id),
  lease_term_id INTEGER REFERENCES gmh.lease_terms(lease_term_id),
  goal_type TEXT,
  goal_amount DECIMAL (10,2),
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE gmh.property_budgets (
  budget_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  property_id INTEGER REFERENCES gmh.properties(property_id),
  lease_term_id INTEGER REFERENCES gmh.lease_terms(lease_term_id),
  budget_type TEXT,
  budget_amount DECIMAL(10,2),
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

*/
