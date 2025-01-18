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

COMMENT ON TABLE gmh.leasing_calendar IS 'Leasing Calendar';
COMMENT ON COLUMN gmh.leasing_calendar.date_key IS 'Date Key';
COMMENT ON COLUMN gmh.leasing_calendar.calendar_year IS 'Calendar Year';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_year IS 'Leasing Year';
COMMENT ON COLUMN gmh.leasing_calendar.pre_lease_year IS 'Pre-Lease Year';
COMMENT ON COLUMN gmh.leasing_calendar.fiscal_year IS 'Fiscal Year';
COMMENT ON COLUMN gmh.leasing_calendar.academic_year IS 'Academic Year';
COMMENT ON COLUMN gmh.leasing_calendar.calendar_week_number IS 'Calendar Week Number';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_season_start_date IS 'Leasing Season Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_season_end_date IS 'Leasing Season End Date';
COMMENT ON COLUMN gmh.leasing_calendar.pre_lease_season_start_date IS 'Pre-Lease Season Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_week_start_date IS 'Leasing Week Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_week_end_date IS 'Leasing Week End Date';
COMMENT ON COLUMN gmh.leasing_calendar.weekly_period_start_date IS 'Weekly Period Start Date';
COMMENT ON COLUMN gmh.leasing_calendar.weekly_period_end_date IS 'Weekly Period End Date';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_week_number IS 'Leasing Week Number';
COMMENT ON COLUMN gmh.leasing_calendar.leasing_weeks_left_to_lease IS 'Leasing Weeks Left to Lease';
COMMENT ON COLUMN gmh.leasing_calendar.entrata_formatted_date IS 'Entrata Formatted Date';
COMMENT ON COLUMN gmh.leasing_calendar.http_date IS 'HTTP Date';
COMMENT ON COLUMN gmh.leasing_calendar.utc_date IS 'UTC Date';
COMMENT ON COLUMN gmh.leasing_calendar.is_current_leasing_season IS 'Is Current Leasing Season';
COMMENT ON COLUMN gmh.leasing_calendar.is_weekend IS 'Is Weekend';
COMMENT ON COLUMN gmh.leasing_calendar.is_holiday IS 'Is Holiday';
COMMENT ON COLUMN gmh.leasing_calendar.day_of_week IS 'Day of Week';
COMMENT ON COLUMN gmh.leasing_calendar.day_of_month IS 'Day of Month';
COMMENT ON COLUMN gmh.leasing_calendar.day_of_year IS 'Day of Year';
COMMENT ON COLUMN gmh.leasing_calendar.month_of_year IS 'Month of Year';
COMMENT ON COLUMN gmh.leasing_calendar.quarter_of_year IS 'Quarter of Year';
