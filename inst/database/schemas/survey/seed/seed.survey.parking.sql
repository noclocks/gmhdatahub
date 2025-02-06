WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.parking (property_id, property_name, parking_type, is_required, is_included, amount, created_by, updated_by)
SELECT
    property_id,
    property_name,
    parking_type,
    is_required,
    is_included,
    amount,
    (SELECT user_id FROM user_id),
    (SELECT user_id FROM user_id)
FROM (
  VALUES
    (739085, '1047 Commonwealth Avenue', 'Surface', FALSE, FALSE, 0.00),
    (739085, '1047 Commonwealth Avenue', 'Reserved', FALSE, FALSE, 275.00),
    (739085, '1047 Commonwealth Avenue', 'Covered', FALSE, FALSE, 0.00),
    (739085, '1047 Commonwealth Avenue', 'Garage', FALSE, FALSE, 175.00),
    (739085, '1047 Commonwealth Avenue', 'Other', FALSE, FALSE, 0.00);
) AS v(property_id, property_name, parking_type, is_required, is_included, amount);

WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.parking (competitor_id, property_name, parking_type, is_required, is_included, amount, created_by, updated_by)
SELECT
    competitor_id,
    property_name,
    parking_type,
    is_required,
    is_included,
    amount,
    (SELECT user_id FROM user_id),
    (SELECT user_id FROM user_id)
FROM (
    VALUES
    (1, 'Bower', 'Surface', FALSE, FALSE, 0.00),
    (1, 'Bower', 'Reserved', FALSE, FALSE, 0.00),
    (1, 'Bower', 'Covered', FALSE, FALSE, 0.00),
    (1, 'Bower', 'Garage', FALSE, FALSE, 0.00),
    (1, 'Bower', 'Other', FALSE, FALSE, 0.00),
    (2, 'Van Ness', 'Surface', FALSE, FALSE, 0.00),
    (2, 'Van Ness', 'Reserved', FALSE, FALSE, 0.00),
    (2, 'Van Ness', 'Covered', FALSE, FALSE, 0.00),
    (2, 'Van Ness', 'Garage', FALSE, FALSE, 0.00),
    (2, 'Van Ness', 'Other', FALSE, FALSE, 0.00),
    (3, '1330 Boylston', 'Surface', FALSE, FALSE, 0.00),
    (3, '1330 Boylston', 'Reserved', FALSE, FALSE, 0.00),
    (3, '1330 Boylston', 'Covered', FALSE, FALSE, 0.00),
    (3, '1330 Boylston', 'Garage', FALSE, FALSE, 405.00),
    (3, '1330 Boylston', 'Other', FALSE, FALSE, 0.00)
) AS v(competitor_id, property_name, parking_type, is_required, is_included, amount);
