DROP TABLE IF EXISTS survey.leasing_weeks;

CREATE TABLE survey.leasing_weeks (
    leasing_week_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    leasing_week_number INTEGER NOT NULL,
    leasing_week_start_date DATE NOT NULL,
    leasing_week_end_date DATE NOT NULL GENERATED ALWAYS AS (leasing_week_start_date + INTERVAL '6 days') STORED,
    leasing_week_year INTEGER NOT NULL GENERATED ALWAYS AS (EXTRACT(YEAR FROM leasing_week_start_date)) STORED,
    is_current BOOLEAN GENERATED ALWAYS AS (leasing_week_start_date <= CURRENT_DATE AND leasing_week_end_date >= CURRENT_DATE) STORED,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_leasing_week_start_date_before_end_date CHECK (leasing_week_start_date < leasing_week_end_date)
);

COMMENT ON TABLE survey.leasing_weeks IS 'Leasing weeks for the survey system';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_id IS 'Primary key for leasing weeks';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_number IS 'The week number of the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_start_date IS 'The start date of the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_end_date IS 'The end date of the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.leasing_week_year IS 'The year of the leasing week';
COMMENT ON COLUMN survey.leasing_weeks.is_current IS 'Whether the leasing week is the current week';
COMMENT ON COLUMN survey.leasing_weeks.created_at IS 'The timestamp when the leasing week was created';
