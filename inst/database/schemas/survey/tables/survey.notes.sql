DROP TABLE IF EXISTS survey.notes CASCADE;

CREATE TABLE survey.notes (
  note_id INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  property_id INTEGER REFERENCES survey.properties(property_id),
  competitor_id INTEGER REFERENCES survey.competitors(competitor_id),
  property_name TEXT,
  leasing_week_id INTEGER REFERENCES survey.leasing_weeks(leasing_week_id),
  note_type TEXT DEFAULT 'General',
  note_actionable BOOLEAN DEFAULT FALSE,
  note_status TEXT DEFAULT 'Pending',
  note_tags TEXT[] DEFAULT '{}',
  note_content TEXT NOT NULL,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  created_by UUID REFERENCES survey.users(user_id),
  updated_by UUID REFERENCES survey.users(user_id),
  CONSTRAINT chk_property_or_competitor CHECK (
    (property_id IS NOT NULL AND competitor_id IS NULL) OR
    (property_id IS NULL AND competitor_id IS NOT NULL)
  )
);

