DROP TABLE IF EXISTS survey.fees;

CREATE TABLE survey.fees (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  leasing_week_id INTEGER NOT NULL REFERENCES survey.leasing_weeks(leasing_week_id),
  fee_name TEXT NOT NULL,
  fee_amount NUMERIC NOT NULL,
  fee_frequency TEXT NOT NULL CHECK (fee_frequency IN ('One Time', 'Monthly', 'Quarterly', 'Annual')),
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id),
  PRIMARY KEY (leasing_week_id, property_name, fee_name),
  CONSTRAINT chk_property_or_competitor CHECK (
    (property_id IS NOT NULL AND competitor_id IS NULL) OR
    (property_id IS NULL AND competitor_id IS NOT NULL)
  )
);
