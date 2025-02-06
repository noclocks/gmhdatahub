WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.utilities (
	property_id,
	property_name,
	utility_name,
	utility_category,
	utility_per,
	utility_available,
	utility_included,
	utility_capped,
	utility_allowance,
	created_by,
	updated_by
)
SELECT
  property_id,
	property_name,
	utility_name,
	utility_category,
	utility_per,
	utility_available,
	utility_included,
	utility_capped,
	utility_allowance,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (739085, '1047 Commonwealth Avenue', 'Electricity', 'Core', 'Unit', true, false, false, 25),
    (739085, '1047 Commonwealth Avenue', 'Gas', 'Core', 'Bed', true, false, false, 0),
    (739085, '1047 Commonwealth Avenue', 'Water', 'Core', 'Bed', true, false, false, 0),
    (739085, '1047 Commonwealth Avenue', 'Cable/Internet', 'Other', 'Bed', false, false, false, 0),
    (739085, '1047 Commonwealth Avenue', 'Trash Service', 'Other', 'Bed', true, false, false, 0),
    (739085, '1047 Commonwealth Avenue', 'Valet Trash', 'Other', 'Bed', true, false, false, 0),
    (739085, '1047 Commonwealth Avenue', 'Recycling', 'Other', 'Bed', true, false, false, 0)
) AS v(
  property_id,
	property_name,
	utility_name,
	utility_category,
	utility_per,
	utility_available,
	utility_included,
	utility_capped,
	utility_allowance
);
