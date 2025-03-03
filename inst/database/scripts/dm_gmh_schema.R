
#  ------------------------------------------------------------------------
#
# Title : GMH Schema Data Model
#    By : Jimmy Briggs
#  Date : 2025-02-26
#
#  ------------------------------------------------------------------------

pkgload::load_all()

library(dm)
library(pool)
library(DBI)
library(connections)
library(dplyr)
library(dbplyr)
library(readr)
library(tibble)
library(vctrs)

# database connection ---------------------------------------------------------------------------------------------

pool <- db_connect()
conn <- pool::poolCheckout(pool)
on.exit(pool::poolReturn(conn), add = TRUE)
connections::connection_view(conn)

db_tbl_gmh_segments <- dplyr::tbl(conn, dbplyr::in_schema("gmh", "segments"))
db_tbl_gmh_partners <- dplyr::tbl(conn, dbplyr::in_schema("gmh", "partners"))
db_tbl_gmh_properties <- dplyr::tbl(conn, dbplyr::in_schema("gmh", "properties"))
db_tbl_gmh_competitors <- dplyr::tbl(conn, dbplyr::in_schema("gmh", "competitors"))

temp_dm <- dm::dm(
  db_tbl_gmh_segments,
  db_tbl_gmh_partners,
  db_tbl_gmh_properties,
  db_tbl_gmh_competitors
)
temp_dm

dm_gmh <- dm::dm_from_con(conn, learn_keys = TRUE, schema = c("entrata", "gmh"))
dm_gmh

# table definitions -----------------------------------------------------------------------------------------------

# segments
tbl_gmh_segments <- tibble::tibble(
  segment_id = integer(),
  segment_name = character(),
  segment_website = character(),
  segment_logo_url = character(),
  segment_banner_url = character(),
  segment_description = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# (investment) partners
tbl_gmh_partners <- tibble::tibble(
  partner_id = integer(),
  partner_name = character(),
  partner_type = character(),
  partner_website = character(),
  partner_logo_url = character(),
  partner_icon_url = character(),
  partner_description = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# properties
tbl_gmh_properties <- tibble::tibble(
  property_id = integer(),
  property_name = character(),
  property_type = character(),
  property_status = character(),
  property_website = character(),
  property_phone = character(),
  property_email = character(),
  property_address = character(),
  property_image_url = character(),
  property_logo_url = character(),
  property_icon_url = character(),
  property_description = character(),
  parent_property_id = integer(),
  partner_id = integer(),
  segment_id = integer(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# competitors
tbl_gmh_competitors <- tibble::tibble(
  competitor_id = integer(),
  competitor_name = character(),
  competitor_website = character(),
  competitor_address = character(),
  competitor_image_url = character(),
  competitor_logo_url = character(),
  competitor_icon_url = character(),
  competitor_description = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# universities
tbl_gmh_universities <- tibble::tibble(
  university_id = integer(),
  university_name = character(),
  university_website = character(),
  university_address = character(),
  university_image_url = character(),
  university_logo_url = character(),
  university_icon_url = character(),
  university_description = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# locations
tbl_gmh_locations <- tibble::tibble(
  location_id = integer(),
  entity_type = character(),
  entity_id = integer(),
  location_name = character(),
  location_address = character(),
  location_city = character(),
  location_state = character(),
  location_postal_code = character(),
  location_country = character(),
  location_phone = character(),
  location_email = character(),
  location_website = character(),
  location_image_url = character(),
  latitude = double(),
  longitude = double(),
  gmaps_place_id = character(),
  gmaps_url = character(),
  gmaps_rating = double(),
  gmaps_reviews_count = integer(),
  map_layer = character(),
  map_marker_icon = character(),
  map_marker_color = character(),
  map_popup_html = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# property competitors
tbl_gmh_property_competitors <- tibble::tibble(
  property_id = integer(),
  competitor_id = integer(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# property universities
tbl_gmh_property_universities <- tibble::tibble(
  property_id = integer(),
  university_id = integer(),
  distance_km = double(),
  distance_miles = double(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# assets
tbl_gmh_assets <- tibble::tibble(
  asset_id = uuid::UUIDgenerate(),
  entity_type = character(),
  entity_id = integer(),
  asset_type = character(),
  asset_url = character(),
  asset_alt_text = character(),
  asset_href = character(),
  asset_description = character(),
  asset_content_type = character(),
  asset_color = character(),
  asset_size = character(),
  asset_aspect_ratio = numeric(),
  gcs_bucket = character(),
  gcs_path = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# leasing calendar
tbl_gmh_leasing_calendar <- tibble::tibble(
  date_key = vctrs::new_date(),
  calendar_year = integer(),
  leasing_year = integer(),
  pre_lease_year = integer(),
  fiscal_year = integer(),
  academic_year = integer(),
  calendar_week_number = integer(),
  leasing_season_start_date = vctrs::new_date(),
  leasing_season_end_date = vctrs::new_date(),
  pre_lease_season_start_date = vctrs::new_date(),
  pre_lease_season_end_date = vctrs::new_date(),
  leasing_week_start_date = vctrs::new_date(),
  leasing_week_end_date = vctrs::new_date(),
  weekly_period_start_date = vctrs::new_date(),
  weekly_period_end_date = vctrs::new_date(),
  leasing_week_number = integer(),
  leasing_weeks_left_to_lease = integer(),
  entrata_formatted_date = character(),
  http_date = character(),
  utc_date = character(),
  is_current_leasing_season = logical(),
  is_weekend = logical(),
  is_holiday = logical(),
  day_of_week = character(),
  day_of_month = integer(),
  day_of_year = numeric(),
  month_of_year = character(),
  quarter_of_year = integer(),
  week_of_year = integer(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# model beds
tbl_gmh_model_beds <- tibble::tibble(
  property_id = integer(),
  model_bed_count = integer(),
  model_bed_notes = character(),
  created_at = vctrs::new_datetime(tzone = "UTC"),
  updated_at = vctrs::new_datetime(tzone = "UTC")
)

# relationships & constraints -------------------------------------------------------------------------------------

dm_gmh <- dm::dm(
  tbl_gmh_segments,
  tbl_gmh_partners,
  tbl_gmh_properties,
  tbl_gmh_competitors,
  tbl_gmh_universities,
  tbl_gmh_locations,
  tbl_gmh_property_competitors,
  tbl_gmh_property_universities,
  tbl_gmh_assets,
  tbl_gmh_leasing_calendar,
  tbl_gmh_model_beds
) |>
  # primary keys
  dm::dm_add_pk(tbl_gmh_segments, segment_id, autoincrement = TRUE) |>
  dm::dm_add_pk(tbl_gmh_partners, partner_id, autoincrement = TRUE) |>
  dm::dm_add_pk(tbl_gmh_properties, property_id, autoincrement = FALSE) |>
  dm::dm_add_pk(tbl_gmh_competitors, competitor_id, autoincrement = TRUE) |>
  dm::dm_add_pk(tbl_gmh_universities, university_id, autoincrement = TRUE) |>
  dm::dm_add_pk(tbl_gmh_locations, location_id, autoincrement = TRUE) |>
  dm::dm_add_pk(tbl_gmh_property_competitors, c(property_id, competitor_id)) |>
  dm::dm_add_pk(tbl_gmh_property_universities, c(property_id, university_id)) |>
  dm::dm_add_pk(tbl_gmh_assets, asset_id, autoincrement = TRUE) |>
  dm::dm_add_pk(tbl_gmh_leasing_calendar, date_key, autoincrement = FALSE) |>
  dm::dm_add_pk(tbl_gmh_model_beds, property_id, autoincrement = FALSE) |>
  # foreign keys
  dm::dm_add_fk(tbl_gmh_properties, partner_id, tbl_gmh_partners, partner_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_properties, segment_id, tbl_gmh_segments, segment_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_properties, parent_property_id, tbl_gmh_properties, property_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_property_competitors, property_id, tbl_gmh_properties, property_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_property_competitors, competitor_id, tbl_gmh_competitors, competitor_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_property_universities, property_id, tbl_gmh_properties, property_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_property_universities, university_id, tbl_gmh_universities, university_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_assets, entity_id, tbl_gmh_properties, property_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_assets, entity_id, tbl_gmh_competitors, competitor_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_assets, entity_id, tbl_gmh_universities, university_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_locations, entity_id, tbl_gmh_properties, property_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_locations, entity_id, tbl_gmh_competitors, competitor_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_locations, entity_id, tbl_gmh_universities, university_id, on_delete = "no_action") |>
  dm::dm_add_fk(tbl_gmh_model_beds, property_id, tbl_gmh_properties, property_id, on_delete = "no_action") |>
  # unique constraints
  dm::dm_add_uk(tbl_gmh_segments, segment_name) |>
  dm::dm_add_uk(tbl_gmh_partners, partner_name) |>
  dm::dm_add_uk(tbl_gmh_properties, property_name) |>
  dm::dm_add_uk(tbl_gmh_competitors, competitor_name) |>
  dm::dm_add_uk(tbl_gmh_universities, university_name) |>
  dm::dm_add_uk(tbl_gmh_locations, c(entity_type, entity_id, location_name)) |>
  dm::dm_add_uk(tbl_gmh_assets, c(entity_type, entity_id, asset_type)) |>
  # table descriptions
  dm::dm_set_table_description(
    "Segments of the GMH Data Model" = tbl_gmh_segments,
    "(Investment) Partners of the GMH Data Model" = tbl_gmh_partners,
    "Properties of the GMH Data Model" = tbl_gmh_properties,
    "Competitors of the GMH Data Model" = tbl_gmh_competitors,
    "Universities of the GMH Data Model" = tbl_gmh_universities,
    "Locations of the GMH Data Model" = tbl_gmh_locations,
    "Property Competitors of the GMH Data Model" = tbl_gmh_property_competitors,
    "Property Universities of the GMH Data Model" = tbl_gmh_property_universities,
    "Assets of the GMH Data Model" = tbl_gmh_assets,
    "Leasing Calendar of the GMH Data Model" = tbl_gmh_leasing_calendar,
    "Model Beds of the GMH Data Model" = tbl_gmh_model_beds
  ) |>
  dm::dm_set_colors(
    blue = c(tbl_gmh_segments, tbl_gmh_partners),
    red = c(tbl_gmh_properties, tbl_gmh_competitors, tbl_gmh_universities),
    purple = c(tbl_gmh_property_competitors, tbl_gmh_property_universities),
    green = c(tbl_gmh_locations),
    orange = c(tbl_gmh_assets),
    yellow = c(tbl_gmh_leasing_calendar, tbl_gmh_model_beds)
  )

dm_gmh |>
  dm::dm_draw(
    view_type = "all",
    graph_name = "GMH Data Model",
    column_types = TRUE
  )

dm::dm_gui(dm = dm_gmh, debug = TRUE)


# keys ------------------------------------------------------------------------------------------------------------

pks <- dm_gmh |> dm::dm_get_all_pks()
fks <- dm_gmh |> dm::dm_get_all_fks()
uks <- dm_gmh |> dm::dm_get_all_uks()

# SQL -------------------------------------------------------------------------------------------------------------

# generate SQL for the DM
dm_gmh_sql <- dm::dm_sql(
  dm = dm_gmh,
  dest = conn
)
dm_gmh_sql

# tbl_gmh_portfolios <- tibble::tibble(
#   portfolio_id = integer(),
#   portfolio_name = character(),
#   portfolio_full_name = character(),
#   portfolio_type = character(),
#   portfolio_status = character(),
#   portfolio_website = character(),
#   portfolio_logo_url = character(),
#   portfolio_icon_url = character(),
#   portfolio_description = character(),
#   segment_id = integer(),
#   partner_id = integer(),
#   created_at = vctrs::new_datetime(),
#   updated_at = vctrs::new_datetime()
# )



# property locations
# tbl_gmh_property_locations <- tibble::tibble(
#   property_id = integer(),
#   location_name = character(),
#   location_address = character(),
#   location_city = character(),
#   location_state = character(),
#   location_postal_code = character(),
#   location_country = character(),
#   location_phone = character(),
#   location_email = character(),
#   location_website = character(),
#   location_image_url = character(),
#   latitude = double(),
#   longitude = double(),
#   gmaps_place_id = character(),
#   gmaps_url = character(),
#   gmaps_rating = double(),
#   gmaps_reviews_count = integer(),
#   map_layer = character(),
#   map_marker_icon = character(),
#   map_marker_color = character(),
#   map_popup_html = character(),
#   created_at = vctrs::new_datetime(tzone = "UTC"),
#   updated_at = vctrs::new_datetime(tzone = "UTC")
# )

# competitor locations
# tbl_gmh_competitor_locations <- tibble::tibble(
#   competitor_id = integer(),
#   location_name = character(),
#   location_address = character(),
#   location_city = character(),
#   location_state = character(),
#   location_postal_code = character(),
#   location_country = character(),
#   location_phone = character(),
#   location_email = character(),
#   location_website = character(),
#   location_image_url = character(),
#   latitude = double(),
#   longitude = double(),
#   gmaps_place_id = character(),
#   gmaps_url = character(),
#   gmaps_rating = double(),
#   gmaps_reviews_count = integer(),
#   map_layer = character(),
#   map_marker_icon = character(),
#   map_marker_color = character(),
#   map_popup_html = character(),
#   created_at = vctrs::new_datetime(tzone = "UTC"),
#   updated_at = vctrs::new_datetime(tzone = "UTC")
# )

# university locations
# tbl_gmh_university_locations <- tibble::tibble(
#   university_id = integer(),
#   location_name = character(),
#   location_address = character(),
#   location_city = character(),
#   location_state = character(),
#   location_postal_code = character(),
#   location_country = character(),
#   location_phone = character(),
#   location_email = character(),
#   location_website = character(),
#   location_image_url = character(),
#   latitude = double(),
#   longitude = double(),
#   gmaps_place_id = character(),
#   gmaps_url = character(),
#   gmaps_rating = double(),
#   gmaps_reviews_count = integer(),
#   map_layer = character(),
#   map_marker_icon = character(),
#   map_marker_color = character(),
#   map_popup_html = character(),
#   created_at = vctrs::new_datetime(tzone = "UTC"),
#   updated_at = vctrs::new_datetime(tzone = "UTC")
# )



tbl_gmh_locations <- tibble::tibble(
  location_id = integer(),
  location_name = character(),
  entity_type = character(),
  entity_id = integer(),
  address = character(),
  city = character(),
  state = character(),
  postal_code = character(),
  country = character(),
  latitude = double(),
  longitude = double(),
  phone_number = character(),
  email = character(),
  website = character(),
  image_url = character(),
  rating = double(),
  gmaps_url = character(),
  gmaps_place_id = character(),
  gmaps_rating = double(),
  gmaps_reviews_count = integer(),
  map_layer = character(),
  map_marker_icon = character(),
  map_marker_color = character(),
  map_popup_html = character(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)



temp_con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
dm_gmh_db <- dm::copy_dm_to(temp_con, dm_gmh, temporary = FALSE)
DBI::dbListTables(temp_con)
dm_gmh_db |> dm::dm_get_tables()
DBI::dbDisconnect(temp_con)

gmh_conn <- db_connect() |> pool::poolCheckout()

gmh_db_dm <- dm::dm_from_con(con = gmh_conn, schema = "gmh")
gmh_db_dm

pks <- gmh_db_dm |> dm::dm_get_all_pks()
fks <- gmh_db_dm |> dm::dm_get_all_fks()

dm::dm_gui(dm = gmh_db_dm)

library(dbplyr)

segments <- tbl(gmh_conn, I("gmh.segments"))
portfolios <- tbl(gmh_conn, I("gmh.portfolios"))
partners <- tbl(gmh_conn, I("gmh.partners"))
properties <- tbl(gmh_conn, I("gmh.properties"))
competitors <- tbl(gmh_conn, I("gmh.competitors"))
universities <- tbl(gmh_conn, I("gmh.universities"))
locations <- tbl(gmh_conn, I("gmh.locations"))

db_dm_gmh <- dm::dm(
  segments,
  portfolios,
  partners,
  properties,
  competitors,
  universities,
  locations
)

db_dm_gmh |> dm::dm_draw(view_type = "all", graph_name = "GMH Data Model")

db_dm_gmh |> dm::dm_enum_pk_candidates(table = "segments")
