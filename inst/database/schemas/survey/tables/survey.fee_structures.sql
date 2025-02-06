DROP TABLE IF EXISTS survey.fee_structures;

CREATE TABLE survey.fee_structures (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
  fee_structure TEXT NOT NULL CHECK (fee_structure IN ('Both Fees Waived', 'Both Fees Due', 'App Due - Admin Waived', 'Admin Due - App Waived'))
);
