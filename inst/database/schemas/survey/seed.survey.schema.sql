INSERT INTO survey.users (user_email)
VALUES
    ('jimmy.briggs@noclocks.dev'),
    ('patrick.howard@noclocks.dev');


INSERT INTO survey.properties (property_id, property_name, property_type, property_website, property_address, property_phone, property_email, property_image_url, property_description)
VALUES
    (739085, '1047 Commonwealth Avenue', 'Student', 'https://www.1047commonwealth.com/', '1047 Commonwealth Ave, Boston, MA, 02215', '+16173510288', 'info@1047commonwealth.com', 'https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg', '1047 Commonwealth Avenue is a student community featuring studio apartments, averaging 340 SF, in Boston, MA. The property features a single-level parking garage, five floors of residential space and 2,890 square feet of retail occupied by the recently opened Caffé Nero.. Exceptional service welcomes you home, along with an ideal location within walking distance to shopping, dining and entertainment options.');


INSERT INTO survey.competitors (property_id, competitor_name, competitor_website, competitor_address, competitor_image_url, competitor_description)
VALUES
    (739085, 'Bower', 'https://bowerboston.com/', '771 Beacon St Apartment 775, Boston, MA, 02215', 'https://images1.apartments.com/i2/GrryY6jRKond7gyq7pWAsu8Mj8i7OIh_GRtZ4tM_CLc/111/bower-boston-ma-building-photo.jpg', 'Bower Boston'),
    (739085, 'Van Ness', 'https://www.vannessproperties.com/', '1335 Boylston St, Boston, MA, 02215', 'https://irp.cdn-website.com/62e359ee/dms3rep/multi/megwIx4yR5SWCXL2Uhkz_ALL+DA+REEDS.v2.0000000.jpg', 'Ven Ness'),
    (739085, '1330 Boylston', 'https://www.1330boylston.com/', '1330 Boylston St, Boston, MA, 02215', 'https://storage.googleapis.com/gmh-images/competitors/1330-boylston/boylston.jpg', '1330 Boylston');

INSERT INTO survey.leasing_weeks (leasing_week_start_date)
VALUES
    ('2024-12-30'),
    ('2025-01-06'),
    ('2025-01-13');

INSERT INTO survey.surveys (property_id, competitor_id, leasing_week_id, user_id, survey_date, survey_status)
VALUES
    (739085, NULL, 2, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), '2025-01-10', 'Initialized');

INSERT INTO survey.sections (section_name, section_description, updated_weekly, is_required)
VALUES
    ('Property Summary', 'Details about the property.', FALSE, TRUE),
    ('Leasing Summary', 'Leasing statistics and trends.', TRUE, TRUE);

INSERT INTO survey.survey_sections (survey_id, section_id, survey_section_status)
VALUES
    (1, 1, 'Completed'),
    (1, 2, 'In Progress');

INSERT INTO survey.property_summary (survey_id, property_id, property_name, property_website, property_address, property_phone, property_developer, property_manager, property_owner, property_type, property_rating, property_status, comp_status, year_built, most_recent_sale, distance_from_campus, created_by)
VALUES
    (1, 739085, '1047 Commonwealth Avenue', 'https://www.1047commonwealth.com/', '1047 Commonwealth Ave, Boston, MA, 02215', '+16173510288', 'BPDA', 'GMH Communities', 'AGC + GMH Communities', 'Student', 2.5, 'Operational', 'Subject Property', 2017, '2019-01-01', 2.5, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'));

INSERT INTO survey.leasing_summary (survey_id, property_id, leasing_week_id, property_name, reporting_cycle, lease_launch_date, renewal_launch_date, current_occupancy, prior_year_occupancy, current_pre_lease, last_year_pre_lease, total_renewals, total_new_leases, weekly_leases, weekly_traffic, current_incentive, incentive_amount, created_by)
VALUES
    (1, 739085, 2, '1047 Commonwealth Avenue', 'Saturday-Friday', '2023-11-27', '2023-10-23', 0.995, 0.994, .388, .400, 51, 20, 0, 58, 'None', 0.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'));
