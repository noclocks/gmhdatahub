CREATE TABLE survey.surveys (
    survey_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    user_id UUID, --REFERENCES auth.users(userid),
    user_email TEXT,
    property_id INTEGER, --REFERENCES mkt.properties(property_id) ON DELETE CASCADE,
    property_name TEXT,
    leasing_week_date DATE NOT NULL,
    survey_date DATE NOT NULL,
    survey_status TEXT, --NOT NULL CHECK (status IN ('Draft', 'Submitted', 'Approved')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
