DROP TABLE IF EXISTS gmh.partners CASCADE;

CREATE TABLE IF NOT EXISTS gmh.partners (
    partner_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    partner_name        TEXT NOT NULL,
    partner_type        TEXT NOT NULL DEFAULT 'Equity Partner',  -- or 'Owner'
    partner_description TEXT DEFAULT 'No Description',
    partner_url         TEXT,
    created_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
    updated_at          TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.partners IS 'Partners represent external (or internal) investment or ownership entities.';
COMMENT ON COLUMN gmh.partners.partner_id IS 'Unique identifier for the partner.';
COMMENT ON COLUMN gmh.partners.partner_name IS 'Name of the partner.';
COMMENT ON COLUMN gmh.partners.partner_type IS 'Type of partner (Equity Partner or Owner).';
COMMENT ON COLUMN gmh.partners.partner_description IS 'Description of the partner.';
COMMENT ON COLUMN gmh.partners.partner_url IS 'URL for the partner.';
COMMENT ON COLUMN gmh.partners.created_at IS 'Timestamp when the partner was created.';
COMMENT ON COLUMN gmh.partners.updated_at IS 'Timestamp when the partner was last updated.';
