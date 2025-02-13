CREATE OR REPLACE VIEW gmh.pre_lease_property_summary AS
WITH unit_summary AS (
  SELECT
    property_id,
    property_name,
    unit_type,
    SUM(excluded_unit_count) AS excluded_units,
    SUM(rentable_unit_count) AS rentable_units,
    AVG(avg_scheduled_rent) AS avg_scheduled_charges,
    SUM(occupied_count) AS occupied_current,
    SUM(approved_new_count_prior + partially_completed_new_count_prior + completed_new_count_prior) AS new_lease_2023,
    SUM(approved_new_count + partially_completed_new_count + completed_new_count) AS new_lease_2024,
    SUM(approved_renewal_count_prior + partially_completed_renewal_count_prior + completed_renewal_count_prior) AS renewal_2023,
    SUM(approved_renewal_count + partially_completed_renewal_count + completed_renewal_count) AS renewal_2024,
    SUM(approved_count_prior + partially_completed_count_prior + completed_count_prior) AS total_2023,
    SUM(approved_count + partially_completed_count + completed_count) AS total_2024,
    SUM(rentable_unit_count) AS total_units,
    SUM(avg_sqft * rentable_unit_count) AS total_sqft,
    SUM(avg_advertised_rate * rentable_unit_count) AS total_advertised_rate,
    SUM(avg_scheduled_rent * rentable_unit_count) AS total_scheduled_charges
  FROM entrata.pre_lease_summary_by_unit
  GROUP BY property_id, property_name, unit_type
)
SELECT
  property_id,
  property_name,
  unit_type,
  excluded_units,
  rentable_units,
  avg_scheduled_charges,
  occupied_current,
  new_lease_2023,
  new_lease_2024,
  renewal_2023,
  renewal_2024,
  total_2023,
  total_2024,
  ROUND(CAST(total_2023 AS NUMERIC) / NULLIF(rentable_units, 0) * 100, 2) AS percent_2023,
  ROUND(CAST(total_2024 AS NUMERIC) / NULLIF(rentable_units, 0) * 100, 2) AS percent_2024,
  ROUND(CAST(total_2024 AS NUMERIC) / NULLIF(rentable_units, 0) * 100 -
        CAST(total_2023 AS NUMERIC) / NULLIF(rentable_units, 0) * 100, 2) AS variance,
  rentable_units - total_2024 AS projected_availability,
  total_sqft,
  total_advertised_rate,
  total_scheduled_charges,
  renewal_2024 AS renewals_count,
  total_2024 AS completed_or_approved_count
FROM unit_summary

UNION ALL

SELECT
  property_id,
  property_name,
  'Total/Average' AS unit_type,
  SUM(excluded_units) AS excluded_units,
  SUM(rentable_units) AS rentable_units,
  AVG(avg_scheduled_charges) AS avg_scheduled_charges,
  SUM(occupied_current) AS occupied_current,
  SUM(new_lease_2023) AS new_lease_2023,
  SUM(new_lease_2024) AS new_lease_2024,
  SUM(renewal_2023) AS renewal_2023,
  SUM(renewal_2024) AS renewal_2024,
  SUM(total_2023) AS total_2023,
  SUM(total_2024) AS total_2024,
  ROUND(CAST(SUM(total_2023) AS NUMERIC) / NULLIF(SUM(rentable_units), 0) * 100, 2) AS percent_2023,
  ROUND(CAST(SUM(total_2024) AS NUMERIC) / NULLIF(SUM(rentable_units), 0) * 100, 2) AS percent_2024,
  ROUND(CAST(SUM(total_2024) AS NUMERIC) / NULLIF(SUM(rentable_units), 0) * 100 -
        CAST(SUM(total_2023) AS NUMERIC) / NULLIF(SUM(rentable_units), 0) * 100, 2) AS variance,
  SUM(rentable_units) - SUM(total_2024) AS projected_availability,
  SUM(total_sqft) AS total_sqft,
  SUM(total_advertised_rate) AS total_advertised_rate,
  SUM(total_scheduled_charges) AS total_scheduled_charges,
  SUM(renewal_2024) AS renewals_count,
  SUM(total_2024) AS completed_or_approved_count
FROM unit_summary
GROUP BY property_id, property_name

ORDER BY property_id, CASE WHEN unit_type = 'Total/Average' THEN 1 ELSE 0 END, unit_type;
