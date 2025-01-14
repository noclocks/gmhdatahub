CREATE TABLE survey.sections (
    section_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    section_name TEXT NOT NULL,
    section_description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);


CREATE TABLE survey.survey_sections (
    section_id INTEGER NOT NULL REFERENCES survey.sections(section_id),
    survey_id INTEGER NOT NULL REFERENCES survey.surveys(survey_id),
    status TEXT NOT NULL CHECK (status IN ('Complete', 'Incomplete')),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (section_id, survey_id)
);
