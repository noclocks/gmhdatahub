/* Market Survey Schema */

DROP SCHEMA IF EXISTS survey CASCADE;

CREATE SCHEMA survey;

-- Users Table
CREATE TABLE survey.users (
    user_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    user_email TEXT UNIQUE NOT NULL,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.users IS 'Survey Users';
COMMENT ON COLUMN survey.users.user_id IS 'Unique identifier for the user';
COMMENT ON COLUMN survey.users.user_email IS 'Email address for the user';
COMMENT ON COLUMN survey.users.created_at IS 'Date and time the user was created';
COMMENT ON COLUMN survey.users.updated_at IS 'Date and time the user was last updated';

-- Properties Table
CREATE TABLE survey.properties (
    property_id INTEGER PRIMARY KEY,
    property_name TEXT NOT NULL UNIQUE,
    property_type TEXT,
    property_website TEXT,
    property_address TEXT,
    property_phone TEXT,
    property_email TEXT,
    property_image_url TEXT,
    property_description TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.properties IS 'Survey Properties';
COMMENT ON COLUMN survey.properties.property_id IS 'Unique identifier for the property';
COMMENT ON COLUMN survey.properties.property_name IS 'Name of the property';
COMMENT ON COLUMN survey.properties.property_type IS 'Type of property (Student, Conventional, Affordable, Innovative)';
COMMENT ON COLUMN survey.properties.property_website IS 'Website URL for the property';
COMMENT ON COLUMN survey.properties.property_address IS 'Address of the property';
COMMENT ON COLUMN survey.properties.property_phone IS 'Phone number for the property';
COMMENT ON COLUMN survey.properties.property_email IS 'Email address for the property';
COMMENT ON COLUMN survey.properties.property_image_url IS 'URL for an image of the property';
COMMENT ON COLUMN survey.properties.property_description IS 'Description of the property';
COMMENT ON COLUMN survey.properties.created_at IS 'Date and time the property was created';
COMMENT ON COLUMN survey.properties.updated_at IS 'Date and time the property was last updated';

-- Competitors Table
CREATE TABLE survey.competitors (
    competitor_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER NOT NULL REFERENCES survey.properties(property_id),
    competitor_name TEXT NOT NULL,
    competitor_website TEXT,
    competitor_address TEXT,
    competitor_image_url TEXT,
    competitor_description TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.competitors IS 'Survey Competitors';
COMMENT ON COLUMN survey.competitors.competitor_id IS 'Unique identifier for the competitor';
COMMENT ON COLUMN survey.competitors.property_id IS 'Identifier for the property associated with the competitor';
COMMENT ON COLUMN survey.competitors.competitor_name IS 'Name of the competitor';
COMMENT ON COLUMN survey.competitors.competitor_website IS 'Website URL for the competitor';
COMMENT ON COLUMN survey.competitors.competitor_address IS 'Address of the competitor';
COMMENT ON COLUMN survey.competitors.competitor_image_url IS 'URL for an image of the competitor';
COMMENT ON COLUMN survey.competitors.competitor_description IS 'Description of the competitor';
COMMENT ON COLUMN survey.competitors.created_at IS 'Date and time the competitor was created';
COMMENT ON COLUMN survey.competitors.updated_at IS 'Date and time the competitor was last updated';

-- Leasing Weeks Table
CREATE TABLE survey.leasing_weeks (
    leasing_week_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    leasing_week_start_date DATE NOT NULL,
    leasing_week_end_date DATE NOT NULL GENERATED ALWAYS AS (leasing_week_start_date + INTERVAL '6 days') STORED,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_leasing_week_start_date CHECK (leasing_week_start_date < leasing_week_end_date)
);

COMMENT ON TABLE survey.leasing_weeks IS 'Leasing Weeks';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_id IS 'Unique identifier for the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_start_date IS 'Start date of the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_end_date IS 'End date of the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.created_at IS 'Date and time the leasing week was created';

-- Surveys Table
CREATE TABLE survey.surveys (
    survey_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER REFERENCES survey.properties(property_id) ON DELETE CASCADE,
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
    user_id UUID NOT NULL REFERENCES survey.users(user_id) ON DELETE SET NULL,
    survey_date DATE NOT NULL DEFAULT CURRENT_DATE,
    survey_status TEXT DEFAULT 'Initialized' NOT NULL CHECK (survey_status IN ('Initialized', 'Draft', 'Submitted', 'Approved', 'Complete')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.surveys IS 'Market Surveys';
COMMENT ON COLUMN survey.surveys.survey_id IS 'Unique identifier for the survey';
COMMENT ON COLUMN survey.surveys.property_id IS 'Identifier for the property associated with the survey';
COMMENT ON COLUMN survey.surveys.competitor_id IS 'Identifier for the competitor associated with the survey';
COMMENT ON COLUMN survey.surveys.leasing_week_id IS 'Identifier for the leasing week associated with the survey';
COMMENT ON COLUMN survey.surveys.user_id IS 'Identifier for the user who created the survey';
COMMENT ON COLUMN survey.surveys.survey_date IS 'Date of the survey';
COMMENT ON COLUMN survey.surveys.survey_status IS 'Status of the survey (Initialized, Draft, Submitted, Approved, Complete)';
COMMENT ON COLUMN survey.surveys.created_at IS 'Date and time the survey was created';
COMMENT ON COLUMN survey.surveys.updated_at IS 'Date and time the survey was last updated';

-- Sections Table
CREATE TABLE survey.sections (
    section_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    section_name TEXT NOT NULL,
    section_description TEXT,
    updated_weekly BOOLEAN NOT NULL,
    is_required BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.sections IS 'Survey Sections';
COMMENT ON COLUMN survey.sections.section_id IS 'Unique identifier for the section';
COMMENT ON COLUMN survey.sections.section_name IS 'Name of the section';
COMMENT ON COLUMN survey.sections.section_description IS 'Description of the section';
COMMENT ON COLUMN survey.sections.updated_weekly IS 'Flag indicating if the section is updated weekly';
COMMENT ON COLUMN survey.sections.is_required IS 'Flag indicating if the section is required';
COMMENT ON COLUMN survey.sections.created_at IS 'Date and time the section was created';
COMMENT ON COLUMN survey.sections.updated_at IS 'Date and time the section was last updated';

-- Survey Sections Table
CREATE TABLE survey.survey_sections (
    survey_id INTEGER NOT NULL REFERENCES survey.surveys(survey_id),
    section_id INTEGER NOT NULL REFERENCES survey.sections(section_id),
    survey_section_status TEXT NOT NULL CHECK (survey_section_status IN ('Not Started', 'In Progress', 'Completed')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (survey_id, section_id)
);

COMMENT ON TABLE survey.survey_sections IS 'Survey Sections';
COMMENT ON COLUMN survey.survey_sections.survey_id IS 'Identifier for the survey associated with the section';
COMMENT ON COLUMN survey.survey_sections.section_id IS 'Identifier for the section associated with the survey';
COMMENT ON COLUMN survey.survey_sections.survey_section_status IS 'Status of the survey section (Not Started, In Progress, Completed)';
COMMENT ON COLUMN survey.survey_sections.created_at IS 'Date and time the survey section was created';
COMMENT ON COLUMN survey.survey_sections.updated_at IS 'Date and time the survey section was last updated';

-- Property Summary Table
DROP TABLE IF EXISTS survey.property_summary;

CREATE TABLE survey.property_summary (
    survey_id INTEGER NOT NULL REFERENCES survey.surveys(survey_id),
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    property_name TEXT NOT NULL,
    property_website TEXT,
    property_address TEXT,
    property_phone TEXT,
    property_developer TEXT,
    property_manager TEXT,
    property_owner TEXT,
    property_type TEXT CHECK (property_type IN ('Student', 'Conventional', 'Affordable', 'Innovative')),
    property_rating NUMERIC(2, 1) CHECK (property_rating BETWEEN 1 AND 5),
    property_status TEXT CHECK (property_status IN ('New Construction', 'Operational', 'Undergoing Renovation')),
    comp_status TEXT CHECK (comp_status IN ('Subject Property', 'Tier 1', 'Tier 2')),
    year_built INTEGER,
    most_recent_sale DATE,
    distance_from_campus NUMERIC(3, 1),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by UUID REFERENCES survey.users(user_id),
    updated_by UUID REFERENCES survey.users(user_id),
    PRIMARY KEY (survey_id, property_name),
    CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);

COMMENT ON TABLE survey.property_summary IS 'Property Summary';
COMMENT ON COLUMN survey.property_summary.survey_id IS 'Identifier for the survey associated with the property summary';
COMMENT ON COLUMN survey.property_summary.property_id IS 'Identifier for the property associated with the property summary';
COMMENT ON COLUMN survey.property_summary.competitor_id IS 'Identifier for the competitor associated with the property summary';
COMMENT ON COLUMN survey.property_summary.property_name IS 'Name of the property';
COMMENT ON COLUMN survey.property_summary.property_website IS 'Website URL for the property';
COMMENT ON COLUMN survey.property_summary.property_address IS 'Address of the property';
COMMENT ON COLUMN survey.property_summary.property_phone IS 'Phone number for the property';
COMMENT ON COLUMN survey.property_summary.property_developer IS 'Developer of the property';
COMMENT ON COLUMN survey.property_summary.property_manager IS 'Manager of the property';
COMMENT ON COLUMN survey.property_summary.property_owner IS 'Owner of the property';
COMMENT ON COLUMN survey.property_summary.property_type IS 'Type of property (Student, Conventional, Affordable, Innovative)';
COMMENT ON COLUMN survey.property_summary.property_rating IS 'Rating of the property (1-5)';
COMMENT ON COLUMN survey.property_summary.property_status IS 'Status of the property (New Construction, Operational, Undergoing Renovation)';
COMMENT ON COLUMN survey.property_summary.comp_status IS 'Competitor status (Subject Property, Tier 1, Tier 2)';
COMMENT ON COLUMN survey.property_summary.year_built IS 'Year the property was built';
COMMENT ON COLUMN survey.property_summary.most_recent_sale IS 'Date of the most recent sale';
COMMENT ON COLUMN survey.property_summary.distance_from_campus IS 'Distance from campus in miles';
COMMENT ON COLUMN survey.property_summary.created_at IS 'Date and time the property summary was created';
COMMENT ON COLUMN survey.property_summary.updated_at IS 'Date and time the property summary was last updated';

-- Leasing Summary Table
DROP TABLE IF EXISTS survey.leasing_summary;

CREATE TABLE survey.leasing_summary (
    survey_id INTEGER NOT NULL REFERENCES survey.surveys(survey_id),
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
	leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
    property_name TEXT NOT NULL,
	reporting_cycle TEXT CHECK (reporting_cycle IN ('Monday-Sunday', 'Saturday-Friday', 'Sunday-Saturday')) DEFAULT 'Monday-Sunday',
    lease_launch_date DATE,
    renewal_launch_date DATE,
    current_occupancy NUMERIC(6, 3),
    prior_year_occupancy NUMERIC(6, 3),
    current_pre_lease NUMERIC(6, 3),
    last_year_pre_lease NUMERIC(6, 3),
    total_renewals INTEGER,
    total_new_leases INTEGER,
    weekly_leases INTEGER,
    weekly_traffic INTEGER,
    current_incentive TEXT DEFAULT 'None',
    incentive_amount NUMERIC(10, 2),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by UUID REFERENCES survey.users(user_id),
    updated_by UUID REFERENCES survey.users(user_id),
    PRIMARY KEY (survey_id, leasing_week_id, property_name),
    CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);

COMMENT ON TABLE survey.leasing_summary IS 'Leasing Summary';
COMMENT ON COLUMN survey.leasing_summary.survey_id IS 'Identifier for the survey associated with the leasing summary';
COMMENT ON COLUMN survey.leasing_summary.property_id IS 'Identifier for the property associated with the leasing summary';
COMMENT ON COLUMN survey.leasing_summary.competitor_id IS 'Identifier for the competitor associated with the leasing summary';
COMMENT ON COLUMN survey.leasing_summary.leasing_week_id IS 'Identifier for the leasing week associated with the leasing summary';
COMMENT ON COLUMN survey.leasing_summary.property_name IS 'Name of the property';
COMMENT ON COLUMN survey.leasing_summary.reporting_cycle IS 'Reporting cycle for the leasing summary';
COMMENT ON COLUMN survey.leasing_summary.lease_launch_date IS 'Date of the lease launch';
COMMENT ON COLUMN survey.leasing_summary.renewal_launch_date IS 'Date of the renewal launch';
COMMENT ON COLUMN survey.leasing_summary.current_occupancy IS 'Current occupancy percentage';
COMMENT ON COLUMN survey.leasing_summary.prior_year_occupancy IS 'Prior year occupancy percentage';
COMMENT ON COLUMN survey.leasing_summary.current_pre_lease IS 'Current pre-lease percentage';
COMMENT ON COLUMN survey.leasing_summary.last_year_pre_lease IS 'Last year pre-lease percentage';
COMMENT ON COLUMN survey.leasing_summary.total_renewals IS 'Total number of renewals';
COMMENT ON COLUMN survey.leasing_summary.total_new_leases IS 'Total number of new leases';
COMMENT ON COLUMN survey.leasing_summary.weekly_leases IS 'Number of weekly leases';
COMMENT ON COLUMN survey.leasing_summary.weekly_traffic IS 'Number of weekly traffic';
COMMENT ON COLUMN survey.leasing_summary.current_incentive IS 'Current incentive offered';
COMMENT ON COLUMN survey.leasing_summary.incentive_amount IS 'Amount of the incentive';
COMMENT ON COLUMN survey.leasing_summary.created_at IS 'Date and time the leasing summary was created';
COMMENT ON COLUMN survey.leasing_summary.updated_at IS 'Date and time the leasing summary was last updated';
