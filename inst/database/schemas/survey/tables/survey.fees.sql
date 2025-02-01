DROP TABLE IF EXISTS survey.fees;

CREATE TABLE survey.fees (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
  fee_name TEXT NOT NULL,
  fee_amount NUMERIC NOT NULL,
  fee_frequency TEXT NOT NULL CHECK (fee_frequency IN ('One Time', 'Monthly', 'Quarterly', 'Annual'))
);
