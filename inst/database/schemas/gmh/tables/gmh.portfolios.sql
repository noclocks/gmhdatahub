DROP TABLE IF EXISTS gmh.portfolios CASCADE;

CREATE TABLE IF NOT EXISTS gmh.portfolios (
  portfolio_id          INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  portfolio_name        TEXT NOT NULL UNIQUE,
  portfolio_full_name   TEXT NOT NULL UNIQUE,
  portfolio_type_id     INTEGER NOT NULL REFERENCES gmh.portfolio_types(portfolio_type_id),
  portfolio_description TEXT DEFAULT 'No Description',
  portfolio_status      TEXT DEFAULT 'Active',
  portfolio_website     TEXT,
  portfolio_logo_url    TEXT,
  portfolio_icon_url    TEXT,
  partner_id            INTEGER NOT NULL REFERENCES gmh.partners(partner_id),
  created_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
  updated_at            TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE gmh.portfolios IS 'Portfolios represent a collection of properties owned or managed by a partner.';
COMMENT ON COLUMN gmh.portfolios.portfolio_id IS 'Unique identifier for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_name IS 'Name of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_type_id IS 'Foreign key referencing the portfolio type.';
COMMENT ON COLUMN gmh.portfolios.portfolio_description IS 'Description of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_status IS 'Status of the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_website IS 'URL for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_logo_url IS 'Logo URL for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.portfolio_icon_url IS 'Icon URL for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.partner_id IS 'Partner ID for the portfolio.';
COMMENT ON COLUMN gmh.portfolios.created_at IS 'Timestamp when the portfolio was created.';
COMMENT ON COLUMN gmh.portfolios.updated_at IS 'Timestamp when the portfolio was last updated.';
