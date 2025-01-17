DROP TABLE IF EXISTS survey.surveys CASCADE;

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
