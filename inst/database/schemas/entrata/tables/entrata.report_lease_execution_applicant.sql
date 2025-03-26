DROP TABLE IF EXISTS entrata.report_lease_execution_applicant;

CREATE TABLE IF NOT EXISTS entrata.report_lease_execution_applicant (
  report_date DATE NOT NULL DEFAULT CURRENT_DATE,
  property_id INTEGER NOT NULL REFERENCES entrata.properties (property_id),
  weekly_new INTEGER,
  weekly_renewal INTEGER,
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (report_date, property_id)
);

