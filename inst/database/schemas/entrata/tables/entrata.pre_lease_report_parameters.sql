DROP TABLE IF EXISTS entrata.pre_lease_report_parameters;

CREATE TABLE entrata.pre_lease_report_parameters (
  request_id TEXT NOT NULL,
  report_date DATE NOT NULL DEFAULT CURRENT_DATE,
  report_name TEXT NOT NULL DEFAULT 'pre_lease',
  report_version TEXT NOT NULL DEFAULT '3.2',
  report_queue_id TEXT NOT NULL,
  property_ids TEXT NOT NULL DEFAULT 'ALL',
  period_date TEXT,
  summarize_by TEXT NOT NULL DEFAULT 'Property',
  group_by TEXT NOT NULL DEFAULT 'None',
  consider_pre_leased_on TEXT NOT NULL DEFAULT 'Lease Partially Completed (332)',
  charge_code_detail TEXT NOT NULL DEFAULT 'TRUE',
  space_options TEXT NOT NULL DEFAULT 'Do Not Show',
  additional_units_shown TEXT NOT NULL DEFAULT 'Available',
  combine_unit_spaces_with_same_lease TEXT NOT NULL DEFAULT 'FALSE',
  consolidate_by TEXT NOT NULL DEFAULT 'None',
  arrange_by_property TEXT NOT NULL DEFAULT 'FALSE',
  subtotals TEXT NOT NULL DEFAULT 'Summary and Details',
  yoy TEXT NOT NULL DEFAULT 'TRUE',
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,

  PRIMARY KEY (request_id, report_date, report_queue_id)
);
