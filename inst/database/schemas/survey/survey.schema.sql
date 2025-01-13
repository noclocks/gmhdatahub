/* Market Survey Notes Table */

DROP TABLE IF EXISTS survey.notes;

CREATE TABLE IF NOT EXISTS survey.notes (
    note_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER NOT NULL REFERENCES survey.properties(property_id),
    note_type VARCHAR(50) NOT NULL,
    note_content TEXT NOT NULL,
    note_comments TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    created_by UUID NOT NULL REFERENCES auth.users(user_id),
);
