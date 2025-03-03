-- Schema
DROP SCHEMA IF EXISTS gmh CASCADE;
CREATE SCHEMA IF NOT EXISTS gmh;
COMMENT ON SCHEMA gmh IS 'GMH Schema housing GMH Communities Data';

-- Segments
DROP TABLE IF EXISTS gmh.segments CASCADE;
CREATE TABLE IF NOT EXISTS gmh.segments (
  segment_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  segment_name TEXT NOT NULL UNIQUE,
  segment_website TEXT UNIQUE DEFAULT NULL,
  segment_description TEXT DEFAULT 'No Description',
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.segments IS 'Segments represent high-level GMH business units or divisions.';
COMMENT ON COLUMN gmh.segments.segment_id IS 'Unique identifier for the segment.';
COMMENT ON COLUMN gmh.segments.segment_name IS 'Name of the segment.';
COMMENT ON COLUMN gmh.segments.segment_website IS 'Website URL for the segment.';
COMMENT ON COLUMN gmh.segments.segment_description IS 'Description of the segment.';
COMMENT ON COLUMN gmh.segments.created_at IS 'Timestamp when the segment was created.';
COMMENT ON COLUMN gmh.segments.updated_at IS 'Timestamp when the segment was last updated.';

-- Partners
DROP TABLE IF EXISTS gmh.partners CASCADE;
CREATE TABLE IF NOT EXISTS gmh.partners (
    partner_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    partner_name        TEXT NOT NULL,
    partner_type        TEXT NOT NULL DEFAULT 'Equity Partner' CHECK (partner_type IN ('Equity Partner', 'Owner')),
    partner_website     TEXT,
    partner_description TEXT DEFAULT 'No Description',
    created_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.partners IS 'Partners represent external (or internal) investment or ownership entities.';
COMMENT ON COLUMN gmh.partners.partner_id IS 'Unique identifier for the partner.';
COMMENT ON COLUMN gmh.partners.partner_name IS 'Name of the partner.';
COMMENT ON COLUMN gmh.partners.partner_type IS 'Type of partner (Equity Partner or Owner).';
COMMENT ON COLUMN gmh.partners.partner_website IS 'Website URL for the partner.';
COMMENT ON COLUMN gmh.partners.partner_description IS 'Description of the partner.';
COMMENT ON COLUMN gmh.partners.created_at IS 'Timestamp when the partner was created.';
COMMENT ON COLUMN gmh.partners.updated_at IS 'Timestamp when the partner was last updated.';

-- Properties
DROP TABLE IF EXISTS gmh.properties CASCADE;
CREATE TABLE gmh.properties (
  property_id          INTEGER PRIMARY KEY, -- No autoincrement here, will leverage entrata property_id
  property_name        TEXT NOT NULL,
  property_type        TEXT NOT NULL DEFAULT 'Apartment',
  property_status      TEXT NOT NULL DEFAULT 'Active',
  property_website     TEXT,
  property_phone       TEXT,
  property_email       TEXT,
  property_address     TEXT,
  property_description TEXT,
  parent_property_id   INTEGER REFERENCES gmh.properties(property_id),
  partner_id           INTEGER NOT NULL REFERENCES gmh.partners(partner_id),
  segment_id           INTEGER NOT NULL REFERENCES gmh.segments(segment_id),
  created_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at           TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.properties IS 'Properties represent individual apartment communities.';
COMMENT ON COLUMN gmh.properties.property_id IS 'Unique identifier for the property.';
COMMENT ON COLUMN gmh.properties.property_name IS 'Name of the property.';
COMMENT ON COLUMN gmh.properties.property_type IS 'Type of property (Apartment, Office, Retail, etc.).';
COMMENT ON COLUMN gmh.properties.property_status IS 'Status of the property (Active, Inactive, etc.).';
COMMENT ON COLUMN gmh.properties.property_website IS 'Website for the property.';
COMMENT ON COLUMN gmh.properties.property_phone IS 'Phone number for the property.';
COMMENT ON COLUMN gmh.properties.property_email IS 'Email address for the property.';
COMMENT ON COLUMN gmh.properties.property_address IS 'Address for the property.';
COMMENT ON COLUMN gmh.properties.property_description IS 'Description of the property.';
COMMENT ON COLUMN gmh.properties.parent_property_id IS 'Parent Property ID for hierarchical relationships.';
COMMENT ON COLUMN gmh.properties.partner_id IS 'Associated Investment Partner ID for the property.';
COMMENT ON COLUMN gmh.properties.segment_id IS 'Segment ID that the property belongs to.';
COMMENT ON COLUMN gmh.properties.created_at IS 'Timestamp when the property was created.';
COMMENT ON COLUMN gmh.properties.updated_at IS 'Timestamp when the property was last updated.';

-- Competitors
DROP TABLE IF EXISTS gmh.competitors;
CREATE TABLE gmh.competitors (
    competitor_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    competitor_name        TEXT NOT NULL,
    competitor_website     TEXT,
    competitor_address     TEXT,
    competitor_description TEXT
    property_id            INTEGER NOT NULL REFERENCES gmh.properties(property_id),
    created_at             TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at             TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.competitors IS 'Competitors represent other apartment communities in the area.';
COMMENT ON COLUMN gmh.competitors.competitor_id IS 'Unique identifier for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_name IS 'Name of the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_website IS 'Website for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_address IS 'Address for the competitor.';
COMMENT ON COLUMN gmh.competitors.competitor_description IS 'Description of the competitor.';
COMMENT ON COLUMN gmh.competitors.property_id IS 'The property that the competitor is associated with.';
COMMENT ON COLUMN gmh.competitors.created_at IS 'Timestamp when the competitor was created.';
COMMENT ON COLUMN gmh.competitors.updated_at IS 'Timestamp when the competitor was last updated.';

-- Universities
DROP TABLE IF EXISTS gmh.universities;
CREATE TABLE gmh.universities (
    university_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    university_name        TEXT NOT NULL,
    university_website     TEXT,
    university_address     TEXT,
    university_description TEXT,
    property_id            INTEGER NOT NULL REFERENCES gmh.properties(property_id),
    created_at             TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at             TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.universities IS 'Universities represent local colleges or universities.';
COMMENT ON COLUMN gmh.universities.university_id IS 'Unique identifier for the university.';
COMMENT ON COLUMN gmh.universities.university_name IS 'Name of the university.';
COMMENT ON COLUMN gmh.universities.university_website IS 'Website URL for the university.';
COMMENT ON COLUMN gmh.universities.university_address IS 'Physical address of the university.';
COMMENT ON COLUMN gmh.universities.university_description IS 'Description of the university.';
COMMENT ON COLUMN gmh.universities.property_id IS 'Property ID that the university is associated with.';
COMMENT ON COLUMN gmh.universities.created_at IS 'Date and time when the university was created.';
COMMENT ON COLUMN gmh.universities.updated_at IS 'Date and time when the university was last updated.';

-- Locations
DROP TABLE IF EXISTS gmh.locations CASCADE;
CREATE TABLE IF NOT EXISTS gmh.locations (
    location_id         INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    entity_type         TEXT NOT NULL CHECK (entity_type IN ('property', 'competitor', 'university')),
    entity_id           INTEGER NOT NULL,
    location_name       TEXT NOT NULL,
    location_address             TEXT NOT NULL,
    location_street              TEXT NOT NULL,
    location_city                TEXT NOT NULL,
    location_state               TEXT NOT NULL,
    location_postal_code         TEXT NOT NULL,
    location_country             TEXT NOT NULL DEFAULT 'USA',
    location_phone        TEXT,
    location_email       TEXT,
    location_website     TEXT,
    latitude            DECIMAL(9,6),
    longitude           DECIMAL(9,6),
    gmaps_place_id      TEXT UNIQUE,
    gmaps_url           TEXT UNIQUE,
    gmaps_rating        DECIMAL(2,1) CHECK (gmaps_rating >= 0 AND gmaps_rating <= 5),
    gmaps_reviews_count INTEGER CHECK (gmaps_reviews_count >= 0),
    map_layer           TEXT NOT NULL CHECK (map_layer IN ('properties', 'competitors', 'universities')),
    map_marker_icon     TEXT,
    map_marker_color    TEXT,
    map_popup_html      TEXT,
    created_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at          TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

COMMENT ON TABLE gmh.locations IS 'Locations consolidated into a central table for all address/coordinate data.';
COMMENT ON COLUMN gmh.locations.location_id IS 'Unique identifier for the location.';
COMMENT ON COLUMN gmh.locations.entity_type IS 'Type of entity the location is associated with.';
COMMENT ON COLUMN gmh.locations.entity_id IS 'Unique identifier of the entity the location is associated with.';
COMMENT ON COLUMN gmh.locations.location_name IS 'Name of the location.';
COMMENT ON COLUMN gmh.locations.location_address IS 'Full address of the location.';
COMMENT ON COLUMN gmh.locations.location_street IS 'Street address of the location.';
COMMENT ON COLUMN gmh.locations.location_city IS 'City where the location is situated.';
COMMENT ON COLUMN gmh.locations.location_state IS 'State where the location is situated.';
COMMENT ON COLUMN gmh.locations.location_postal_code IS 'Postal code of the location.';
COMMENT ON COLUMN gmh.locations.location_country IS 'Country where the location is situated.';
COMMENT ON COLUMN gmh.locations.location_phone IS 'Phone number of the location.';
COMMENT ON COLUMN gmh.locations.location_email IS 'Email address of the location.';
COMMENT ON COLUMN gmh.locations.location_website IS 'Website URL of the location.';
COMMENT ON COLUMN gmh.locations.latitude IS 'Latitude coordinate of the location.';
COMMENT ON COLUMN gmh.locations.longitude IS 'Longitude coordinate of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_place_id IS 'Google Maps Place ID of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_url IS 'Google Maps URL of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_rating IS 'Google Maps rating of the location.';
COMMENT ON COLUMN gmh.locations.gmaps_reviews_count IS 'Number of Google Maps reviews for the location.';
COMMENT ON COLUMN gmh.locations.map_layer IS 'Map layer of the location.';
COMMENT ON COLUMN gmh.locations.map_marker_icon IS 'Map marker icon of the location.';
COMMENT ON COLUMN gmh.locations.map_marker_color IS 'Map marker color of the location.';
COMMENT ON COLUMN gmh.locations.map_popup_html IS 'HTML content for the map popup.';
COMMENT ON COLUMN gmh.locations.created_at IS 'Timestamp when the location was created.';
COMMENT ON COLUMN gmh.locations.updated_at IS 'Timestamp when the location was last updated.';

-- assets
DROP TABLE IF EXISTS gmh.assets CASCADE;
CREATE TABLE IF NOT EXISTS gmh.assets (
  asset_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  entity_type TEXT NOT NULL CHECK (entity_type IN ('property', 'competitor', 'university', 'segment', 'partner', 'other')),
  entity_id           INTEGER NOT NULL,
  asset_type TEXT,
  asset_url TEXT,
  asset_alt_text TEXT,
  asset_href TEXT,
  asset_description TEXT,
  asset_content_type TEXT,
  asset_color TEXT,
  asset_size TEXT,
  gcs_bucket TEXT,
  gcs_path TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Model Beds
DROP TABLE IF EXISTS gmh.model_beds CASCADE;

CREATE TABLE gmh.model_beds (
    property_id      INTEGER PRIMARY KEY REFERENCES gmh.properties(property_id) ON DELETE CASCADE,
    model_bed_count  INT DEFAULT 0,
    model_bed_notes            TEXT,
    created_at       TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
    updated_at       TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

-- Leasing Calendar
DROP TABLE IF EXISTS gmh.leasing_calendar CASCADE;

CREATE TABLE gmh.leasing_calendar (
  date_key DATE PRIMARY KEY,
  calendar_year INTEGER NOT NULL,
  leasing_year INTEGER NOT NULL,
  pre_lease_year INTEGER NOT NULL,
  fiscal_year INTEGER NOT NULL,
  academic_year TEXT NOT NULL,
  calendar_week_number INTEGER NOT NULL,
  leasing_season_start_date DATE NOT NULL,
  leasing_season_end_date DATE NOT NULL,
  pre_lease_season_start_date DATE NOT NULL,
  leasing_week_start_date DATE NOT NULL,
  leasing_week_end_date DATE NOT NULL,
  weekly_period_start_date DATE NOT NULL,
  weekly_period_end_date DATE NOT NULL,
  leasing_week_number INTEGER NOT NULL,
  leasing_weeks_left_to_lease INTEGER NOT NULL,
  entrata_formatted_date TEXT NOT NULL,
  http_date TEXT NOT NULL,
  utc_date TEXT NOT NULL,
  is_current_leasing_season BOOLEAN NOT NULL,
  is_weekend BOOLEAN NOT NULL,
  is_holiday BOOLEAN NOT NULL,
  day_of_week TEXT NOT NULL,
  day_of_month INTEGER NOT NULL,
  day_of_year NUMERIC NOT NULL,
  month_of_year TEXT NOT NULL,
  quarter_of_year INTEGER NOT NULL
);

CREATE INDEX idx_leasing_calendar_date_key ON gmh.leasing_calendar(date_key);
CREATE INDEX idx_leasing_calendar_leasing_season_start_date ON gmh.leasing_calendar(leasing_season_start_date);
CREATE INDEX idx_leasing_calendar_leasing_season_end_date ON gmh.leasing_calendar(leasing_season_end_date);
CREATE INDEX idx_leasing_calendar_pre_lease_season_start_date ON gmh.leasing_calendar(pre_lease_season_start_date);
CREATE INDEX idx_leasing_calendar_leasing_week_start_date ON gmh.leasing_calendar(leasing_week_start_date);
CREATE INDEX idx_leasing_calendar_leasing_week_end_date ON gmh.leasing_calendar(leasing_week_end_date);
CREATE INDEX idx_leasing_calendar_weekly_period_start_date ON gmh.leasing_calendar(weekly_period_start_date);
CREATE INDEX idx_leasing_calendar_weekly_period_end_date ON gmh.leasing_calendar(weekly_period_end_date);

COMMENT ON TABLE gmh.leasing_calendar IS 'Leasing Calendar';
COMMENT ON COLUMN gmh.leasing_calendar.date_key IS 'Date Key';
COMMENT ON COLUMN gmh.leasing_calendar.calendar_year IS 'Calendar Year';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_year IS 'Leasing Year';
COMMENT ON COLUMN gmh.leasing_calendar.pre_lease_year IS 'Pre-Lease Year';
COMMENT ON COLUMN gmh.leasing_calendar.fiscal_year IS 'Fiscal Year';
COMMENT ON COLUMN gmh.leasing_calendar.academic_year IS 'Academic Year';
COMMENT ON COLUMN gmh.leasing_calendar.calendar_week_number IS 'Calendar Week Number';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_season_start_date IS 'Leasing Season Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_season_end_date IS 'Leasing Season End Date';
COMMENT ON COLUMN gmh.leasing_calendar.pre_lease_season_start_date IS 'Pre-Lease Season Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_week_start_date IS 'Leasing Week Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_week_end_date IS 'Leasing Week End Date';
COMMENT ON COLUMN gmh.leasing_calendar.weekly_period_start_date IS 'Weekly Period Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.weekly_period_end_date IS 'Weekly Period End Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_week_number IS 'Leasing Week Number';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_weeks_left_to_lease IS 'Leasing Weeks Left to Lease';
COMMENT ON COLUMN gmh.leasing_calendar.entrata_formatted_date IS 'Entrata Formatted Date';
COMMENT ON COLUMN gmh.leasing_calendar.http_date IS 'HTTP Date';
COMMENT ON COLUMN gmh.leasing_calendar.utc_date IS 'UTC Date';
COMMENT ON COLUMN gmh.leasing_calendar.is_current_leasing_season IS 'Is Current Leasing Season';
COMMENT ON COLUMN gmh.leasing_calendar.is_weekend IS 'Is Weekend';
COMMENT ON COLUMN gmh.leasing_calendar.is_holiday IS 'Is Holiday';
COMMENT ON COLUMN gmh.leasing_calendar.day_of_week IS 'Day of Week';
COMMENT ON COLUMN gmh.leasing_calendar.day_of_month IS 'Day of Month';
COMMENT ON COLUMN gmh.leasing_calendar.day_of_year IS 'Day of Year';
COMMENT ON COLUMN gmh.leasing_calendar.month_of_year IS 'Month of Year';
COMMENT ON COLUMN gmh.leasing_calendar.quarter_of_year IS 'Quarter of Year';

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
