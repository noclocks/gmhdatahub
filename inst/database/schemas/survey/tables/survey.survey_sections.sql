CREATE TABLE survey.survey_sections (
    survey_id INTEGER NOT NULL REFERENCES survey.surveys(survey_id),
    section_id INTEGER NOT NULL REFERENCES survey.sections(section_id),
    survey_section_status TEXT NOT NULL CHECK (status IN ('Not Started', 'In Progress', 'Completed')),
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (survey_id, section_id)
);
