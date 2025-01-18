prep_competitors <- tibble::tribble(
  ~competitor_id, ~competitor_name, ~competitor_website, ~competitor_address, ~competitor_image_url, ~property_id,
  1L, "1330 Boylston", "https://www.1330boylston.com/", "1330 Boylston St, Boston, MA, 02215", "https://storage.googleapis.com/gmh-images/competitors/1330-boylston/boylston.jpg", 739085L,
  2L, "Van Ness", "https://www.vannessproperties.com/", "1335 Boylston St, Boston, MA, 02215", "https://irp.cdn-website.com/62e359ee/dms3rep/multi/megwIx4yR5SWCXL2Uhkz_ALL+DA+REEDS.v2.0000000.jpg", 739085L,
  3L, "Bower", "https://bowerboston.com/", "771 Beacon St Apartment 775, Boston, MA, 02215", "https://images1.apartments.com/i2/GrryY6jRKond7gyq7pWAsu8Mj8i7OIh_GRtZ4tM_CLc/111/bower-boston-ma-building-photo.jpg", 739085L
)

readr::write_csv(prep_competitors, "data-raw/data/working/gmh/gmh_competitors.csv")
