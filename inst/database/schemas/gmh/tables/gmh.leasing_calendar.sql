DROP TABLE IF EXISTS gmh.leasing_calendar CASCADE;

CREATE TABLE gmh.leasing_calendar (
  date_key DATE PRIMARY KEY,
  calendar_year INTEGER NOT NULL,
  leasing_year INTEGER NOT NULL,
  pre_lease_year INTEGER NOT NULL,
  fiscal_year INTEGER NOT NULL,
  academic_year TEXT NOT NULL,
  calendar_week_number INTEGER NOT NULL,
  leasing_season_start_date DATE NOT NULL,
  leasing_season_end_date DATE NOT NULL,
  pre_lease_season_start_date DATE NOT NULL,
  leasing_week_start_date DATE NOT NULL,
  leasing_week_end_date DATE NOT NULL,
  weekly_period_start_date DATE NOT NULL,
  weekly_period_end_date DATE NOT NULL,
  leasing_week_number INTEGER NOT NULL,
  leasing_weeks_left_to_lease INTEGER NOT NULL,
  entrata_formatted_date TEXT NOT NULL,
  http_date TEXT NOT NULL,
  utc_date TEXT NOT NULL,
  is_current_leasing_season BOOLEAN NOT NULL,
  is_weekend BOOLEAN NOT NULL,
  is_holiday BOOLEAN NOT NULL,
  day_of_week TEXT NOT NULL,
  day_of_month INTEGER NOT NULL,
  day_of_year NUMERIC NOT NULL,
  month_of_year TEXT NOT NULL,
  quarter_of_year INTEGER NOT NULL
);

CREATE INDEX idx_leasing_calendar_date_key ON gmh.leasing_calendar(date_key);

CREATE INDEX idx_leasing_calendar_leasing_season_start_date ON gmh.leasing_calendar(leasing_season_start_date);

CREATE INDEX idx_leasing_calendar_leasing_season_end_date ON gmh.leasing_calendar(leasing_season_end_date);

CREATE INDEX idx_leasing_calendar_pre_lease_season_start_date ON gmh.leasing_calendar(pre_lease_season_start_date);

CREATE INDEX idx_leasing_calendar_leasing_week_start_date ON gmh.leasing_calendar(leasing_week_start_date);

CREATE INDEX idx_leasing_calendar_leasing_week_end_date ON gmh.leasing_calendar(leasing_week_end_date);

CREATE INDEX idx_leasing_calendar_weekly_period_start_date ON gmh.leasing_calendar(weekly_period_start_date);

CREATE INDEX idx_leasing_calendar_weekly_period_end_date ON gmh.leasing_calendar(weekly_period_end_date);
