CREATE TABLE entrata.property_lease_term_windows (
  property_id TEXT NOT NULL REFERENCES entrata.properties(property_id),
  lease_term_id INT NOT NULL,
  lease_term_window_id INT NOT NULL,
  lease_term_name TEXT,
  lease_term_months INT,
  lease_term_is_prospect BOOLEAN,
  lease_term_is_renewal BOOLEAN,
  lease_term_window_start_date DATE,
  lease_term_window_end_date DATE,
  PRIMARY KEY (property_id, lease_term_id, lease_term_window_id)
);
