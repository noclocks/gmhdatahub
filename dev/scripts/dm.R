
#  ------------------------------------------------------------------------
#
# Title : Data Model
#    By : Jimmy Briggs
#  Date : 2024-12-31
#
#  ------------------------------------------------------------------------

library(dm)
library(pool)
library(DBI)
library(connections)
library(dplyr)
library(dbplyr)
library(readr)
library(tibble)
library(vctrs)


# data frame / table specs ------------------------------------------------

tbl_gmh_segments <- tibble::tibble(
  segment_id = integer(),
  segment_name = character(),
  segment_description = character(),
  segment_website = character(),
  segment_logo_url = character(),
  segment_banner_url = character(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)

tbl_gmh_portfolios <- tibble::tibble(
  portfolio_id = integer(),
  portfolio_name = character(),
  portfolio_full_name = character(),
  portfolio_type = character(),
  portfolio_status = character(),
  portfolio_website = character(),
  portfolio_logo_url = character(),
  portfolio_icon_url = character(),
  portfolio_description = character(),
  segment_id = integer(),
  partner_id = integer(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)

tbl_gmh_partners <- tibble::tibble(
  partner_id = integer(),
  partner_name = character(),
  partner_description = character(),
  partner_website = character(),
  tenant_id = integer(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)

tbl_gmh_properties <- tibble::tibble(
  property_id = integer(),
  property_name = character(),
  parent_property_id = integer(),
  property_type = character(),
  property_status = character(),
  property_website = character(),
  property_phone = character(),
  property_email = character(),
  property_address = character(),
  property_description = character(),
  portfolio_id = integer(),
  partner_id = integer(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)

tbl_gmh_competitors <- tibble::tibble(
  competitor_id = integer(),
  competitor_name = character(),
  competitor_website = character(),
  competitor_description = character(),
  competitor_address = character(),
  competitor_logo_url = character(),
  property_id = integer(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)

tbl_gmh_universities <- tibble::tibble(
  university_id = integer(),
  university_name = character(),
  university_website = character(),
  university_description = character(),
  university_address = character(),
  university_logo_url = character(),
  property_id = integer(),
  created_at = vctrs::new_datetime(),
  updated_at = vctrs::new_datetime()
)

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

dm_gmh <- dm::dm(
  tbl_gmh_segments,
  tbl_gmh_portfolios,
  tbl_gmh_partners,
  tbl_gmh_properties,
  tbl_gmh_competitors,
  tbl_gmh_universities,
  tbl_gmh_locations
) |>
  dm::dm_add_pk(tbl_gmh_segments, segment_id) |>
  dm::dm_add_pk(tbl_gmh_portfolios, portfolio_id) |>
  dm::dm_add_pk(tbl_gmh_partners, partner_id) |>
  dm::dm_add_pk(tbl_gmh_properties, property_id) |>
  dm::dm_add_pk(tbl_gmh_competitors, competitor_id) |>
  dm::dm_add_pk(tbl_gmh_universities, university_id) |>
  dm::dm_add_pk(tbl_gmh_locations, location_id) |>
  dm::dm_add_fk(tbl_gmh_portfolios, segment_id, tbl_gmh_segments, segment_id) |>
  dm::dm_add_fk(tbl_gmh_portfolios, partner_id, tbl_gmh_partners, partner_id) |>
  dm::dm_add_fk(tbl_gmh_properties, portfolio_id, tbl_gmh_portfolios, portfolio_id) |>
  dm::dm_add_fk(tbl_gmh_properties, partner_id, tbl_gmh_partners, partner_id) |>
  dm::dm_add_fk(tbl_gmh_competitors, property_id, tbl_gmh_properties, property_id) |>
  dm::dm_add_fk(tbl_gmh_universities, property_id, tbl_gmh_properties, property_id) |>
  dm::dm_add_fk(tbl_gmh_locations, entity_id, tbl_gmh_properties, property_id) |>
  dm::dm_add_fk(tbl_gmh_locations, entity_id, tbl_gmh_competitors, competitor_id) |>
  dm::dm_add_fk(tbl_gmh_locations, entity_id, tbl_gmh_universities, university_id) |>
  # table descriptions
  dm::dm_set_table_description(
    "Segments of the GMH Data Model" = tbl_gmh_segments,
    "Portfolios of the GMH Data Model" = tbl_gmh_portfolios,
    "Partners of the GMH Data Model" = tbl_gmh_partners,
    "Properties of the GMH Data Model" = tbl_gmh_properties,
    "Competitors of the GMH Data Model" = tbl_gmh_competitors,
    "Universities of the GMH Data Model" = tbl_gmh_universities,
    "Locations of the GMH Data Model" = tbl_gmh_locations
  )

dm_gmh |>
  dm::dm_set_colors(
    blue = c(tbl_gmh_segments, tbl_gmh_portfolios, tbl_gmh_partners),
    green = c(tbl_gmh_properties, tbl_gmh_competitors, tbl_gmh_universities),
    red = c(tbl_gmh_locations)
  ) |>
  dm::dm_draw(
    view_type = "all",
    graph_name = "GMH Data Model",
    column_types = TRUE
  )

dm::dm_gui(dm = dm_gmh)

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


survey_db_dm <- dm::dm_from_con(con = gmh_conn, schema = "survey")
survey_db_dm

survey_db_dm |> dm::dm_get_all_pks()
survey_db_dm |> dm::dm_get_all_fks()

survey_db_dm |> dm::dm_draw(view_type = "all", graph_name = "Survey Data Model")

dm::dm_gui(dm = survey_db_dm)

tables <- c(
  "amenities.sql",
  "competitors.sql",
  "fees.sql",
  "fee_structures.sql",
  "floorplans.sql",
  "hours.sql",
  "leasing_summary.sql",
  "leasing_weeks.sql",
  "notes.sql",
  "parking.sql",
  "properties.sql",
  "property_amenities.sql",
  "property_summary.sql",
  "property_units.sql",
  "rents_by_floorplan.sql",
  "sections.sql",
  "short_term_leases.sql",
  "surveys.sql",
  "survey_sections.sql",
  "triggers.sql",
  "unit_amenities.sql",
  "unit_amenities_rates_premiums.sql",
  "users.sql",
  "utilities.sql"
) |>
  fs::path_ext_remove()

tables <- c(DBI::Id("amenities"),
            DBI::Id("competitors"),
            # DBI::Id("fee_structures"),
            DBI::Id("fees"),
            DBI::Id("hours"),
            DBI::Id("leasing_summary"),
            # DBI::Id("leasing_weeks"),
            DBI::Id("notes"),
            DBI::Id("parking"),
            DBI::Id("properties"),
            DBI::Id("property_amenities"),
            DBI::Id("property_summary"),
            DBI::Id("rents_by_floorplan"),
            # DBI::Id("sections"),
            DBI::Id("short_term_leases"),
            # DBI::Id("survey_sections"),
            # DBI::Id("surveys"),
            DBI::Id("unit_amenities"),
            # DBI::Id("unit_amenities_rates_premiums"),
            # DBI::Id("users"),
            DBI::Id("utilities"))

# get SQL DDL

sql_lines <- capture.output(
  dm::dm_ddl_pre(survey_db_dm, gmh_conn, temporary = FALSE)
)

sql <- paste(sql_lines, collapse = "\n")
cat(sql)
