DROP TABLE IF EXISTS survey.leasing_weeks;

CREATE TABLE survey.leasing_weeks (
    leasing_week_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    leasing_week_start_date DATE NOT NULL,
    leasing_week_end_date DATE NOT NULL GENERATED ALWAYS AS (leasing_week_start_date + INTERVAL '6 days') STORED,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT chk_leasing_week_start_date_before_end_date CHECK (leasing_week_start_date < leasing_week_end_date)
);
