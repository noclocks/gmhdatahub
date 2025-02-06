DROP TABLE IF EXISTS survey.property_summary CASCADE;

CREATE TABLE survey.property_summary (
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    property_name TEXT NOT NULL,
    property_website TEXT,
    property_address TEXT,
    property_email TEXT,
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
    property_image_url TEXT,
    property_description TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by UUID REFERENCES survey.users(user_id),
    updated_by UUID REFERENCES survey.users(user_id),
    PRIMARY KEY (property_name),
    CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);
