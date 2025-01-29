INSERT INTO survey.parking (property_id, property_name, parking_type, is_required, is_included, amount, created_by, updated_by)
VALUES
    (739085, '1047 Commonwealth Avenue', 'Surface', FALSE, FALSE, 0.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev')),
    (739085, '1047 Commonwealth Avenue', 'Reserved', FALSE, FALSE, 275.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev')),
    (739085, '1047 Commonwealth Avenue', 'Covered', FALSE, FALSE, 0.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev')),
    (739085, '1047 Commonwealth Avenue', 'Garage', FALSE, FALSE, 175.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev')),
    (739085, '1047 Commonwealth Avenue', 'Other', FALSE, FALSE, 0.00, (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'), (SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'))
