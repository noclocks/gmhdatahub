WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.short_term_leases (
	property_id,
	leasing_week_id,
	property_name,
	term_months,
	is_available,
	premium,
	quantity,
	created_by,
	updated_by
)
SELECT
  property_id,
	leasing_week_id,
	property_name,
	term_months,
	is_available,
	premium,
	quantity,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (739085, 6, '1047 Commonwealth Avenue', 5, false, 0.00, 0),
    (739085, 6, '1047 Commonwealth Avenue', 10, false, 0.00, 0)
) AS v(
  property_id,
	leasing_week_id,
	property_name,
	term_months,
	is_available,
	premium,
	quantity
);

WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.short_term_leases (
	competitor_id,
	leasing_week_id,
	property_name,
	term_months,
	is_available,
	premium,
	quantity,
	created_by,
	updated_by
)
SELECT
  competitor_id,
	leasing_week_id,
	property_name,
	term_months,
	is_available,
	premium,
	quantity,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (1, 6, 'Bower', 5, false, 0.00, 0),
    (1, 6, 'Bower', 10, false, 0.00, 0),
    (2, 6, 'Van Ness', 5, false, 0.00, 0),
    (2, 6, 'Van Ness', 10, false, 0.00, 0),
    (3, 6, '1330 Boylston', 5, false, 0.00, 0),
    (3, 6, '1330 Boylston', 10, false, 0.00, 0)
) AS v(
  competitor_id,
	leasing_week_id,
	property_name,
	term_months,
	is_available,
	premium,
	quantity
);
