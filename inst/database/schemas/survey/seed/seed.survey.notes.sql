WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.notes (
  property_id,
  leasing_week_id,
  property_name,
  note_type,
  note_actionable,
  note_status,
  note_tags,
  note_content,
  created_by,
  updated_by
)
SELECT
  property_id,
  leasing_week_id,
  property_name,
  note_type,
  note_actionable,
  note_status,
  note_tags,
  note_content,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (739085, 6, '1047 Commonwealth Avenue', 'Leasing Specials', false, 'Complete', ARRAY['Specials', 'Discounts'], 'New Lease Launch 11/27')
) AS v(
  property_id,
  leasing_week_id,
  property_name,
  note_type,
  note_actionable,
  note_status,
  note_tags,
  note_content
);

WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.notes (
  competitor_id,
  leasing_week_id,
  property_name,
  note_type,
  note_actionable,
  note_status,
  note_tags,
  note_content,
  created_by,
  updated_by
)
SELECT
  competitor_id,
  leasing_week_id,
  property_name,
  note_type,
  note_actionable,
  note_status,
  note_tags,
  note_content,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (1, 6, 'Bower', 'Leasing Specials', false, 'Complete', ARRAY['Specials', 'Discounts'], '$1000 off first month for a studio if you move in January 2024'),
    (3, 6, '1330 Boylston', 'Leasing Specials', false, 'Complete', ARRAY['Specials', 'Discounts'], '$250 resident refferal program')
) AS v(
  competitor_id,
  leasing_week_id,
  property_name,
  note_type,
  note_actionable,
  note_status,
  note_tags,
  note_content
);
