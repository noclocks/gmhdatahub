DROP TABLE IF EXISTS survey.sections CASCADE;

CREATE TABLE survey.sections (
    section_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    section_name TEXT NOT NULL,
    section_description TEXT,
    updated_weekly BOOLEAN NOT NULL,
    is_required BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

