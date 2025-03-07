CREATE OR REPLACE VIEW gmh.pre_lease_per_property_unit_summary_tbl AS
WITH base_summary AS (
  SELECT
    s.report_date,
    s.property_id,
    s.property_name,
    s.unit_type,
    s.units AS total_beds,
    s.excluded_unit_count,
    s.rentable_unit_count,
    s.occupied_count AS current_occupied,
    s.occupied_count::NUMERIC / NULLIF(s.rentable_unit_count, 0) AS current_occupancy,
    (s.approved_new_count + s.partially_completed_new_count + s.completed_new_count)
      AS current_total_new,
    (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count)
      AS current_total_renewals,
    ((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
     (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))
      AS current_total_leases,
    (((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
      (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))::NUMERIC
      / NULLIF(s.rentable_unit_count, 0)) AS current_preleased_percent,
    (s.approved_new_count_prior + s.partially_completed_new_count_prior + s.completed_new_count_prior)
      AS prior_total_new,
    (s.approved_renewal_count_prior + s.partially_completed_renewal_count_prior + s.completed_renewal_count_prior)
      AS prior_total_renewals,
    (s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior)
      AS prior_total_leases,
    ((s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior)::NUMERIC
      / NULLIF(s.rentable_unit_count, 0)) AS prior_preleased_percent,
    (((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
      (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count)) -
     (s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior))
      AS yoy_variance_count,
    ((((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
       (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))::NUMERIC
      / NULLIF(s.rentable_unit_count, 0)) -
     ((s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior)::NUMERIC
      / NULLIF(s.rentable_unit_count, 0))) AS yoy_variance_percent,
    (s.rentable_unit_count -
     ((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
      (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count)))
      AS beds_left
  FROM entrata.pre_lease_summary_by_property_unit s
),
base_rent_avg AS (
  SELECT
    report_date,
    property_id,
    unit_type,
    AVG(scheduled_rent) AS avg_scheduled_charges
  FROM entrata.pre_lease_details_by_property_unit
  WHERE charge_code = 'Base Rent'
  GROUP BY report_date, property_id, unit_type
)

SELECT
  bs.report_date,
  bs.property_id,
  bs.property_name,
  bs.unit_type,
  bs.total_beds,
  bs.excluded_unit_count,
  bs.rentable_unit_count,
  bs.current_occupied,
  bs.current_occupancy,
  bs.current_total_new,
  bs.current_total_renewals,
  bs.current_total_leases,
  bs.current_preleased_percent,
  bs.prior_total_new,
  bs.prior_total_renewals,
  bs.prior_total_leases,
  bs.prior_preleased_percent,
  bs.yoy_variance_count,
  bs.yoy_variance_percent,
  bs.beds_left,
  COALESCE(br.avg_scheduled_charges, 0) AS avg_scheduled_charges,
  c.leasing_weeks_left_to_lease AS weeks_left_to_lease
FROM base_summary bs
LEFT JOIN base_rent_avg br
  ON bs.report_date = br.report_date
  AND bs.property_id = br.property_id
  AND bs.unit_type = br.unit_type
LEFT JOIN gmh.leasing_calendar c
  ON bs.report_date = c.date_key;
