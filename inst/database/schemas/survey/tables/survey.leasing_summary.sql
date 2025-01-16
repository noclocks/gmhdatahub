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
