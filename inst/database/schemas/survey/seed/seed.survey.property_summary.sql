WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.property_summary (
  property_id,
  property_name,
  property_website,
  property_address,
  property_email,
  property_phone,
  property_developer,
  property_manager,
  property_owner,
  property_type,
  property_rating,
  property_status,
  comp_status,
  year_built,
  most_recent_sale,
  distance_from_campus,
  property_image_url,
  property_description,
  created_by,
  updated_by
)
SELECT
  property_id,
  property_name,
  property_website,
  property_address,
  property_email,
  property_phone,
  property_developer,
  property_manager,
  property_owner,
  property_type,
  property_rating,
  property_status,
  comp_status,
  year_built,
  most_recent_sale,
  distance_from_campus,
  property_image_url,
  property_description,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    (739085, '1047 Commonwealth Avenue', 'https://www.1047commonwealth.com/', '1047 Commonwealth Ave, Boston, MA, 02215', 'info@1047commonwealth.com', '+16173510288', 'BPDA', 'GMH Communities', 'AGC + GMH Communities', 'Student', 2.5, 'Operational', 'Subject Property', 2017, TO_DATE('2019-01-01', 'YYYY-MM-DD'), 2.5, 'https://images1.forrent.com/i2/0ksd7Uwm-74efUhB-UiU1cEyVv3NVOiCCVuF4oQ7cZI/117/image.jpg', '1047 Commonwealth Avenue is a student community featuring studio apartments, averaging 340 SF, in Boston, MA. The property features a single-level parking garage, five floors of residential space and 2,890 square feet of retail occupied by the recently opened Caffe Nero.. Exceptional service welcomes you home, along with an ideal location within walking distance to shopping, dining and entertainment options.')
) AS v(
  property_id,
  property_name,
  property_website,
  property_address,
  property_email,
  property_phone,
  property_developer,
  property_manager,
  property_owner,
  property_type,
  property_rating,
  property_status,
  comp_status,
  year_built,
  most_recent_sale,
  distance_from_campus,
  property_image_url,
  property_description
);

WITH user_id AS (
    SELECT user_id FROM survey.users WHERE user_email = 'jimmy.briggs@noclocks.dev'
)

INSERT INTO survey.property_summary (
  competitor_id,
  property_name,
  property_website,
  property_address,
  property_email,
  property_phone,
  property_developer,
  property_manager,
  property_owner,
  property_type,
  property_rating,
  property_status,
  comp_status,
  year_built,
  most_recent_sale,
  distance_from_campus,
  property_image_url,
  property_description,
  created_by,
  updated_by
)
VALUES
-- Property: Bower
(1, 'Bower', 'https://bowerboston.com/',
 '771 Beacon St Apartment 775, Boston, MA, 02215', NULL, '+6173419700',
 'The Green Cities Company', 'Greystar', 'Greystar',
 'Conventional', 5.0, 'Operational', 'Tier 2',
 2020, NULL, 0.2,
 'https://images1.apartments.com/i2/GrryY6jRKond7gyq7pWAsu8Mj8i7OIh_GRtZ4tM_CLc/111/bower-boston-ma-building-photo.jpg',
 NULL, (SELECT user_id FROM user_id), (SELECT user_id FROM user_id)),
 -- Property: Van Ness
(2, 'Van Ness', 'https://www.thevanness.com/',
 '1335 Boylston St, Boston, MA, 02215', NULL, '+16174241335',
 'Samuels And Associates', 'Samuels And Associates', 'Samuels And Associates',
 'Conventional', 4.0, 'Operational', 'Tier 2',
 2015, NULL, 1.0,
 'https://www.thevanness.com/wp-content/uploads/2022/02/VanNess-Background3.jpeg',
 NULL, (SELECT user_id FROM user_id), (SELECT user_id FROM user_id)),
-- Property: 1330 Boylston
(3, '1330 Boylston', 'https://1330boylston.com/',
 '1330 Boylston St, Boston, MA, 02215', NULL, '+16172671330',
 'Samuels and Associates', 'Samuels and Associates', 'Samuels and Associates',
 'Conventional', 5.0, 'Operational', 'Tier 2',
 2008, NULL, 1.0,
 'https://www.1330boylston.com/wp-content/uploads/2024/02/1330-Boylston-Apartments_Rooftop-Pool_web.jpg',
 NULL, (SELECT user_id FROM user_id), (SELECT user_id FROM user_id));



  c.competitor_id,
  c.competitor_name AS property_name,
  c.competitor_website AS property_website,
  c.competitor_address AS property_address,
  c.competitor_email AS property_email,

  property_phone,
  property_developer,
  property_manager,
  property_owner,
  property_type,
  property_rating,
  property_status,
  comp_status,
  year_built,
  most_recent_sale,
  distance_from_campus,
  property_image_url,
  property_description,
  (SELECT user_id FROM user_id),
  (SELECT user_id FROM user_id)
FROM (
  VALUES
    ()
) AS v(
  property_id,
  property_name,
  property_website,
  property_address,
  property_email,
  property_phone,
  property_developer,
  property_manager,
  property_owner,
  property_type,
  property_rating,
  property_status,
  comp_status,
  year_built,
  most_recent_sale,
  distance_from_campus,
  property_image_url,
  property_description
);
