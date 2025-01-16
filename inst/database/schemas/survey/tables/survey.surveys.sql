DROP TABLE IF EXISTS survey.surveys CASCADE;

CREATE TABLE IF NOT EXISTS survey.surveys (
    survey_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER REFERENCES survey.properties(property_id),
    competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
    leasing_week_id INTEGER REFERENCES survey.leasing_weeks(leasing_week_id),
    user_id INTEGER REFERENCES survey.users(user_id),
    survey_date DATE NOT NULL DEFAULT CURRENT_DATE,
    survey_status TEXT, --NOT NULL CHECK (status IN ('Initialized', 'Draft', 'Submitted', 'Approved', 'Complete')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
