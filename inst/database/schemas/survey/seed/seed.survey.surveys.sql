INSERT INTO survey.surveys (property_id, competitor_id, leasing_week_id, user_id, survey_date, survey_status)
VALUES
    (739085, NULL, 2, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), '2025-01-10', 'Complete'),
    (NULL, 1, 2, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), '2025-01-10', 'Initialized'),
    (NULL, 2, 2, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), '2025-01-10', 'Initialized'),
    (NULL, 3, 2, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), '2025-01-10', 'Initialized');
