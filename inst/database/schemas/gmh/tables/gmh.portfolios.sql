DROP TABLE IF EXISTS gmh.portfolios CASCADE;

CREATE TABLE IF NOT EXISTS gmh.portfolios (
  portfolio_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  portfolio_name        TEXT NOT NULL UNIQUE,
  portfolio_type        TEXT NOT NULL DEFAULT 'Equity Partner',  -- or 'Owner'
  portfolio_description TEXT DEFAULT 'No Description',
  segment_id            INT NOT NULL REFERENCES gmh.segments(segment_id),
  partner_id            INT NOT NULL REFERENCES gmh.partners(partner_id),
  created_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.portfolios IS 'Portfolios represent a collection of properties owned or managed by a partner.';
COMMENT ON COLUMN gmh.portfolios.portfolio_id IS 'Unique identifier for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_name IS 'Name of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_type IS 'Type of portfolio (Equity Partner or Owner).';
COMMENT ON COLUMN gmh.portfolios.portfolio_description IS 'Description of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.segment_id IS 'Segment ID for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.partner_id IS 'Partner ID for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.created_at IS 'Timestamp when the portfolio was created.';
COMMENT ON COLUMN gmh.portfolios.updated_at IS 'Timestamp when the portfolio was last updated.';
