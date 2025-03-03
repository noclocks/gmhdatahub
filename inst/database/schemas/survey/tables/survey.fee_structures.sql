DROP TABLE IF EXISTS survey.fee_structures;

CREATE TABLE survey.fee_structures (
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT NOT NULL,
  fee_structure TEXT NOT NULL DEFAULT 'Both Fees Waived' CHECK (fee_structure IN ('Both Fees Waived', 'Both Fees Due', 'App Due - Admin Waived', 'Admin Due - App Waived')),
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id),
  PRIMARY KEY (property_name, fee_structure),
  CONSTRAINT chk_property_or_competitor CHECK (
    (property_id IS NOT NULL AND competitor_id IS NULL) OR
    (property_id IS NULL AND competitor_id IS NOT NULL)
  )
);
