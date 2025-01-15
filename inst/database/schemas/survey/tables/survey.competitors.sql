DROP TABLE IF EXISTS survey.competitors CASCADE;

CREATE TABLE IF NOT EXISTS survey.competitors (
    competitor_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    property_id INTEGER NOT NULL REFERENCES survey.properties(property_id),
    competitor_name TEXT NOT NULL,
    competitor_description TEXT,
    created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE survey.competitors IS 'Competitors: Represents competitors of a property';
COMMENT ON COLUMN survey.competitors.competitor_id IS 'Unique Identifier for the individual competitors';
COMMENT ON COLUMN survey.competitors.property_id IS 'The associated property for the competitor';
COMMENT ON COLUMN survey.competitors.competitor_name IS 'The name of the competitor';
COMMENT ON COLUMN survey.competitors.competitor_description IS 'A description of the competitor';
COMMENT ON COLUMN survey.competitors.created_at IS 'The date and time the competitor was created';
COMMENT ON COLUMN survey.competitors.updated_at IS 'The date and time the competitor was last updated';
