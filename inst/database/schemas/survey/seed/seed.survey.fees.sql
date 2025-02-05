WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.fees (
  property_id,
  leasing_week_id,
  property_name,
  fee_name,
  fee_amount,
  fee_frequency,
  created_by,
  updated_by
)
SELECT
  property_id,
  leasing_week_id,
  property_name,
  fee_name,
  fee_amount,
  fee_frequency,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (739085, 6, '1047 Commonwealth Avenue', 'Application Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Administration Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Utility Set Up Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Utility Deposit', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Amenity Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Common Area Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Smart Home Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Restoration Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Security Deposit', 1000, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Pet Fee', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Pet Deposit', 0, 'Monthly'),
    (739085, 6, '1047 Commonwealth Avenue', 'Pet Rent', 0, 'Monthly')
) AS v(
  property_id,
  leasing_week_id,
  property_name,
  fee_name,
  fee_amount,
  fee_frequency
);

WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.fees (
  competitor_id,
  leasing_week_id,
  property_name,
  fee_name,
  fee_amount,
  fee_frequency,
  created_by,
  updated_by
)
SELECT
  competitor_id,
  leasing_week_id,
  property_name,
  fee_name,
  fee_amount,
  fee_frequency,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (1, 6, 'Bower', 'Application Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Administration Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Utility Set Up Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Utility Deposit', 0, 'Monthly'),
    (1, 6, 'Bower', 'Amenity Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Common Area Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Smart Home Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Restoration Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Security Deposit', 500, 'Monthly'),
    (1, 6, 'Bower', 'Pet Fee', 0, 'Monthly'),
    (1, 6, 'Bower', 'Pet Deposit', 0, 'Monthly'),
    (1, 6, 'Bower', 'Pet Rent', 35, 'Monthly'),
    (2, 6, 'Van Ness', 'Application Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Administration Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Utility Set Up Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Utility Deposit', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Amenity Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Common Area Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Smart Home Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Restoration Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Security Deposit', 500, 'Monthly'),
    (2, 6, 'Van Ness', 'Pet Fee', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Pet Deposit', 0, 'Monthly'),
    (2, 6, 'Van Ness', 'Pet Rent - Dog', 100, 'Monthly'),
    (2, 6, 'Van Ness', 'Pet Rent - Cat', 50, 'Monthly'),
    (3, 6, '1330 Boylston', 'Application Fee', 250, 'Monthly'),
    (3, 6, '1330 Boylston', 'Administration Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Utility Set Up Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Utility Deposit', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Amenity Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Common Area Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Smart Home Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Restoration Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Security Deposit', 1000, 'Monthly'),
    (3, 6, '1330 Boylston', 'Pet Fee', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Pet Deposit', 0, 'Monthly'),
    (3, 6, '1330 Boylston', 'Pet Rent - Dog', 100, 'Monthly'),
    (3, 6, '1330 Boylston', 'Pet Rent - Cat', 50, 'Monthly')
) AS v(
  competitor_id,
  leasing_week_id,
  property_name,
  fee_name,
  fee_amount,
  fee_frequency
);

