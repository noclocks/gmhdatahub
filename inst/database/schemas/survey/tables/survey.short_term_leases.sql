DROP TABLE IF EXISTS survey.short_term_leases CASCADE;

CREATE TABLE survey.short_term_leases (
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
    property_name TEXT NOT NULL,
    term_months INTEGER NOT NULL CHECK (term_months > 0),
    is_available BOOLEAN NOT NULL DEFAULT FALSE,
    premium NUMERIC(10, 2) NOT NULL DEFAULT 0.00,
    quantity INTEGER NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by UUID REFERENCES survey.users(user_id),
    updated_by UUID REFERENCES survey.users(user_id),
    PRIMARY KEY (leasing_week_id, property_name, term_months),
    CONSTRAINT chk_property_or_competitor CHECK (
        (property_id IS NOT NULL AND competitor_id IS NULL) OR
        (property_id IS NULL AND competitor_id IS NOT NULL)
    )
);
