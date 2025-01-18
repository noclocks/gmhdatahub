DROP TABLE IF EXISTS gmh.model_beds CASCADE;

CREATE TABLE gmh.model_beds (
    property_id      INTEGER PRIMARY KEY REFERENCES gmh.properties(property_id) ON DELETE CASCADE,
    model_bed_count  INT DEFAULT 0,
    notes            TEXT,
    updated_at       TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.model_beds IS 'Model beds represent the number of beds representing models for each property.';
COMMENT ON COLUMN gmh.model_beds.property_id IS 'Property ID for the model beds.';
COMMENT ON COLUMN gmh.model_beds.model_bed_count IS 'Number of model beds for the property.';
COMMENT ON COLUMN gmh.model_beds.notes IS 'Notes for the model beds.';
COMMENT ON COLUMN gmh.model_beds.updated_at IS 'Timestamp when the model beds were last updated.';
