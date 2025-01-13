CREATE TABLE entrata.property_post_months (
  property_id TEXT NOT NULL REFERENCES entrata.properties(property_id),
  ar_post_month DATE,  -- Accounts Receivable
  ap_post_month DATE,  -- Accounts Payable
  gl_post_month DATE   -- General Ledger
);
