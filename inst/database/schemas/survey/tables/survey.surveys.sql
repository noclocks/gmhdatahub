DROP TABLE IF EXISTS survey.surveys CASCADE;

CREATE TABLE IF NOT EXISTS survey.surveys (
    survey_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER NOT NULL REFERENCES survey.properties(property_id) ON DELETE CASCADE,
    leasing_week DATE NOT NULL REFERENCES survey.leasing_weeks(leasing_week_date) ON DELETE CASCADE,
    user_id UUID, --NOT NULL REFERENCES auth.users(user_id) ON DELETE CASCADE,
    user_email TEXT,
    property_name TEXT,
    survey_date DATE NOT NULL DEFAULT CURRENT_DATE,
    survey_status TEXT, --NOT NULL CHECK (status IN ('Initialized', 'Draft', 'Submitted', 'Approved', 'Complete')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.surveys IS 'Surveys: Represents surveys taken by users';
COMMENT ON COLUMN survey.surveys.survey_id IS 'Primary key for surveys';
COMMENT ON COLUMN survey.surveys.property_id IS 'Foreign key to properties table';
COMMENT ON COLUMN survey.surveys.leasing_week IS 'Foreign key to leasing_weeks table';
COMMENT ON COLUMN survey.surveys.user_id IS 'Foreign key to users table';
COMMENT ON COLUMN survey.surveys.user_email IS 'Email of user taking survey';
COMMENT ON COLUMN survey.surveys.property_name IS 'Name of property';
COMMENT ON COLUMN survey.surveys.survey_date IS 'Date survey was taken';
COMMENT ON COLUMN survey.surveys.survey_status IS 'Status of survey';
COMMENT ON COLUMN survey.surveys.created_at IS 'Timestamp when survey was created';
COMMENT ON COLUMN survey.surveys.updated_at IS 'Timestamp when survey was last updated';
