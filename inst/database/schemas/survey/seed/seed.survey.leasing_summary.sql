INSERT INTO survey.leasing_summary (survey_id, property_id, leasing_week_id, property_name, reporting_cycle, lease_launch_date, renewal_launch_date, current_occupancy, prior_year_occupancy, current_pre_lease, last_year_pre_lease, total_renewals, total_new_leases, weekly_leases, weekly_traffic, current_incentive, incentive_amount, created_by)
VALUES
    (1, 739085, 2, '1047 Commonwealth Avenue', 'Saturday-Friday', '2023-11-27', '2023-10-23', 0.995, 0.994, .388, .400, 51, 20, 0, 58, 'None', 0.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'));
