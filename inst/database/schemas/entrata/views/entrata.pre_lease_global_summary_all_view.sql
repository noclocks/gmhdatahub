DROP VIEW IF EXISTS entrata.pre_lease_global_summary_all_view CASCADE;

CREATE OR REPLACE VIEW entrata.pre_lease_global_summary_all_view AS
WITH base AS (
  SELECT
    s.report_date,
    CAST(s.property_id AS integer) AS property_id,
    s.property_name,
    p.partner_name AS investment_partner,
    s.available_unit_count AS total_beds,
    COALESCE(mb.model_bed_count, 0) AS model_beds,
    s.occupied_unit_count AS current_occupied,
    s.occupied_unit_count::NUMERIC / NULLIF(s.available_unit_count, 0) AS current_occupancy,
    (s.approved_new_count + s.partially_completed_new_count + s.completed_new_count)
      AS current_total_new,
    (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count)
      AS current_total_renewals,
    ((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
     (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))
      AS current_total_leases,
    (((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
      (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))::numeric
      / s.available_unit_count) AS current_preleased_percent,
    s.preleased_percent AS current_preleased_percent_original,
    (s.approved_new_count_prior + s.partially_completed_new_count_prior + s.completed_new_count_prior)
      AS prior_total_new,
    (s.approved_renewal_count_prior + s.partially_completed_renewal_count_prior + s.completed_renewal_count_prior)
      AS prior_total_renewals,
    (s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior)
      AS prior_total_leases,
    ((s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior)::numeric
      / s.available_unit_count) AS prior_preleased_percent,
    (((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
      (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))
      - (s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior))
      AS yoy_variance_count,
    ((((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
       (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))::numeric
       / s.available_unit_count)
       - ((s.approved_count_prior + s.partially_completed_count_prior + s.completed_count_prior)::numeric
         / s.available_unit_count))
      AS yoy_variance_percent,
    w.weekly_new,
    w.weekly_renewal,
	COALESCE(w.weekly_new, 0) + COALESCE(w.weekly_renewal, 0) AS weekly_total,
    (COALESCE(w.weekly_new, 0) + COALESCE(w.weekly_renewal, 0))::numeric / s.available_unit_count
      AS weekly_percent_gained,
    s.available_unit_count -
      ((s.approved_new_count + s.partially_completed_new_count + s.completed_new_count) +
       (s.approved_renewal_count + s.partially_completed_renewal_count + s.completed_renewal_count))
      AS beds_left,
    c.leasing_weeks_left_to_lease AS weeks_left_to_lease
  FROM entrata.report_pre_lease_summary_by_property s
  LEFT JOIN gmh.leasing_calendar c
    ON s.report_date = c.date_key
  LEFT JOIN gmh.model_beds mb ON s.property_id = mb.property_id
  LEFT JOIN entrata.report_lease_execution_applicant w ON s.property_id = w.property_id AND s.report_date = w.report_date
  LEFT JOIN gmh.properties pr
    ON s.property_id = pr.property_id
  LEFT JOIN gmh.partners p
    ON pr.partner_id = p.partner_id
)
SELECT
  report_date,
  property_id,
  property_name,
  investment_partner,
  total_beds,
  model_beds,
  current_occupied,
  current_occupancy,
  current_total_new,
  current_total_renewals,
  current_total_leases,
  current_preleased_percent,
  prior_total_new,
  prior_total_renewals,
  prior_total_leases,
  prior_preleased_percent,
  yoy_variance_count,
  yoy_variance_percent,
  weekly_new,
  weekly_renewal,
  weekly_total,
  weekly_percent_gained,
  weeks_left_to_lease,
  beds_left,
  beds_left * 0.9 / weeks_left_to_lease AS vel_90,
  beds_left * 0.95 / weeks_left_to_lease AS vel_95,
  beds_left * 1.0 / weeks_left_to_lease AS vel_100
FROM base;
