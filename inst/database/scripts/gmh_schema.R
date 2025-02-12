source("data-raw/scripts/gmh.R")

pool <- db_connect()

leasing_calendar_tbl <- create_leasing_calendar(start_year = 2023, num_years = 3) |>
  dplyr::select(-date_key) |>
  dplyr::rename(date_key = date)

pool::dbAppendTable(
  pool,
  DBI::SQL("gmh.leasing_calendar"),
  leasing_calendar_tbl
)

db_init_tbl <- function(pool, tbl_name, tbl_data, sql_file = NULL) {

  # create table
  db_run_sql(sql_file, pool = pool)

  # seed table
  db_seed_tbl(pool, tbl_name = tbl_name, tbl_data = tbl_data)

  # read back
  hold <- db_read_tbl(pool, tbl_name = tbl_name)
  rm_cols <- c("created_at", "updated_at", "created_by", "updated_by", "modified_at", "modified_by")
  hold <- hold |> dplyr::select(-dplyr::any_of(rm_cols))

  # compare
  waldo::compare(hold, tbl_data)

  return(invisible(tbl_data))

}

db_init_tbl(pool, "gmh.segments", gmh_segments_tbl, "inst/database/schemas/gmh/tables/gmh.segments.sql")
db_init_tbl(pool, "gmh.partners", gmh_partners_tbl, "inst/database/schemas/gmh/tables/gmh.partners.sql")
db_init_tbl(pool, "gmh.portfolios", gmh_portfolios_tbl, "inst/database/schemas/gmh/tables/gmh.portfolios.sql")
db_init_tbl(pool, "gmh.properties", gmh_properties_tbl, "inst/database/schemas/gmh/tables/gmh.properties.sql")
db_init_tbl(pool, "gmh.competitors", gmh_competitors_tbl, "inst/database/schemas/gmh/tables/gmh.competitors.sql")
db_init_tbl(pool, "gmh.leasing_calendar", gmh_leasing_calendar_tbl, "inst/database/schemas/gmh/tables/gmh.leasing_calendar.sql")


# # create tables
# db_run_sql("inst/database/schemas/gmh/tables/gmh.segments.sql", pool = pool)
# # seed tables
# db_seed_tbl(pool, tbl_name = "gmh.segments", tbl_data = gmh_segments_tbl)
# # read back
# db_gmh_segments <- db_read_tbl(pool, tbl_name = "gmh.segments") |>
#   dplyr::select(-created_at)
# # compare
# waldo::compare(db_gmh_segments, gmh_segments_tbl)
#
#
# # leasing calendar --------------------------------------------------------
#
#
#
#


# generate_table_ddl(leasing_calendar_tbl, "leasing_calendar", "gmh") |>
#   cat(sep = "\n", file = "inst/database/schemas/gmh/tables/gmh.leasing_calendar.sql", append = TRUE)
#
# db_run_sql("inst/database/schemas/gmh/tables/gmh.leasing_calendar.sql", pool = pool)
#
# db_seed_tbl(pool, tbl_name = "gmh.leasing_calendar", tbl_data = leasing_calendar_tbl)
#
# # properties
#
# gmh_portfolios <- db_read_tbl(pool, "gmh.portfolios")
#
# tbl_entrata_properties <- db_read_tbl(pool, "entrata.properties")
#
# portfolio_assignments <- tibble::tibble(
#   portfolio_id = rep(c(1, 2, 3, 4, 5, 99), c(10L, 2L, 1L, 1L, 4L, 19L)),
#   portfolio = rep(
#     c("AGC", "CBRE", "CRG", "Medistar", "Principal", "GMH"),
#     c(10L, 2L, 1L, 1L, 4L, 19L)
#   ),
#   property_id = c(
#     739085, 739085, 739079, 739080, 739084, 641240, 676055, 535270, 676054,
#     833617, 1143679, 952515, 1197887, 0, 518044, 518041, 518042, 518046, 1311849,
#     0, 1197887, 1143679, 739085, 739085, 739079, 739080, 739084, 641240, 676055,
#     518044, 952515, 518041, 535270, 518042, 676054, 518046, 833617
#   ),
#   property_name = c(
#     "Campustown 1008 S. 4th", "1047 Commonwealth Avenue",
#     "Campustown 307 E. Daniel", "Campustown 501 S. 6th", "Campustown 908 S. 1st",
#     "Academy 65", "Academy Lincoln", "The Academy Chorro", "The Academy Palomar",
#     "The Dean Campustown", "Torre", "SOVA", "The Dean Reno", "Life Tower",
#     "Shortbread Lofts", "The Academy at Frisco", "The Academy on Charles",
#     "The Rise at Northgate", "The Venue at North Campus", "Life Tower",
#     "The Dean Reno", "Torre", "Campustown 1008 S. 4th",
#     "1047 Commonwealth Avenue", "Campustown 307 E. Daniel",
#     "Campustown 501 S. 6th", "Campustown 908 S. 1st", "Academy 65",
#     "Academy Lincoln", "Shortbread Lofts", "SOVA", "The Academy at Frisco",
#     "The Academy Chorro", "The Academy on Charles", "The Academy Palomar",
#     "The Rise at Northgate", "The Dean Campustown"
#   ),
# )
#
# tbl_gmh_properties <- tbl_entrata_properties |>
#   dplyr::mutate(
#     property_id = as.integer(property_id),
#     property_status = "Active",
#     property_description = short_description,
#     property_url = website
#   ) |>
#   dplyr::select(
#     property_id,
#     property_name = marketing_name,
#     property_type,
#     property_status,
#     property_description,
#     property_url
#   ) |>
#   dplyr::left_join(
#     dplyr::select(tbl_gmh_locations_db, location_id, property_name = location_name),
#     by = "property_name"
#   ) |>
#   dplyr::left_join(
#     portfolio_assignments |>
#       dplyr::filter(
#         portfolio != "GMH"
#       ) |>
#       dplyr::select(portfolio_id, property_id) |>
#       dplyr::mutate(property_id = as.integer(property_id)) |>
#       dplyr::distinct(),
#     by = "property_id"
#   )
#
# db_seed_tbl(pool, tbl_name = "gmh.properties", tbl_data = tbl_gmh_properties)
#
# # locations
#
# gmh_properties <- db_read_tbl(pool, "gmh.properties")
# mkt_properties <- db_read_tbl(pool, "mkt.properties")
# mkt_competitors <- db_read_tbl(pool, "mkt.competitors")
# mkt_locations <- db_read_tbl(pool, "mkt.locations")
# mkt_universities <- db_read_tbl(pool, "mkt.universities")
# mkt_university_locations <- db_read_tbl(pool, "mkt.university_locations")
# mkt_locations <- db_read_tbl(pool, "mkt.locations")
# mkt_property_competitors <- db_read_tbl(pool, "mkt.property_competitors")
# mkt_property_locations <- db_read_tbl(pool, "mkt.property_locations")
#
# gmh_competitors_tbl <- tibble::tibble(
#   competitor_name = c("1330 Boylston", "Van Ness", "Bower"),
#   competitor_url =
#   location_id = c(NA, NA, NA),
# )
#
# commonwealth_id <- entrata_properties_lst[["1047 Commonwealth Avenue"]]
#
# tbl_gmh_locations_properties_competitors <- mkt_locations |>
#   dplyr::mutate(
#     is_active = TRUE,
#     # postal code is in state by accident, need to get last 5 chars
#     postal_code = stringr::str_sub(state, -5),
#     state = "MA"
#   ) |>
#   dplyr::select(
#     location_name = property_name,
#     address,
#     city,
#     state,
#     postal_code,
#     country,
#     latitude,
#     longitude,
#     phone_number,
#     email,
#     website = gmaps_website,
#     image_url = property_image,
#     rating = gmaps_rating,
#     gmaps_url,
#     gmaps_place_id,
#     gmaps_rating,
#     gmaps_reviews_count = gmaps_num_of_reviews,
#     map_layer,
#     map_marker_icon,
#     map_marker_color,
#     map_popup_html
#   )
#
# tbl_gmh_locations_universities <- mkt_universities |>
#   dplyr::select(
#     university_id,
#     location_name = university_name,
#     address = university_address,
#     website = university_website,
#     image_url = university_logo
#   ) |>
#   dplyr::left_join(
#     dplyr::select(mkt_university_locations, -address),
#     by = "university_id"
#   ) |>
#   dplyr::left_join(
#     dplyr::select(
#       uni_gmaps_data,
#       address = original_address,
#       latitude = gmaps_latitude,
#       longitude = gmaps_longitude,
#       gmaps_url,
#       gmaps_place_id,
#       gmaps_rating,
#       gmaps_reviews_count = gmaps_user_ratings_total,
#       map_marker_icon = gmaps_icon,
#       phone_number = gmaps_phone_number,
#       website = gmaps_website,
#       gmaps_url
#     ),
#     by = "address"
#   ) |>
#   dplyr::mutate(
#     city = Vectorize(get_city_from_address)(address) |> as.character(),
#     state = "MA",
#     postal_code = Vectorize(get_postal_code_from_address)(address) |> as.character(),
#     country = "USA",
#     map_layer = "universities",
#     map_marker_icon = "university",
#     map_marker_color = "blue",
#     email = NA_character_,
#     rating = gmaps_rating
#   ) |>
#   dplyr::select(
#     location_name,
#     address,
#     website = website.y,
#     image_url,
#     latitude = latitude.y,
#     longitude = longitude.y,
#     gmaps_url = gmaps_url.y,
#     gmaps_place_id = gmaps_place_id.y,
#     gmaps_rating,
#     gmaps_reviews_count,
#     phone_number,
#     map_marker_icon,
#     city,
#     state,
#     postal_code,
#     country,
#     map_layer,
#     map_marker_color,
#     email,
#     rating
#   ) |>
#   dplyr::mutate(
#     map_popup_html = paste0(
#       "<b>",
#       location_name,
#       "</b><br>",
#       address,
#       "<br>",
#       "<a href='",
#       website,
#       "' target='_blank'>Website</a>"
#     )
#   ) |>
#   dplyr::mutate(
#     is_active = TRUE
#   ) |>
#   dplyr::select(
#     location_name,
#     address,
#     city,
#     state,
#     postal_code,
#     country,
#     latitude,
#     longitude,
#     phone_number,
#     email,
#     website,
#     image_url,
#     rating,
#     gmaps_url,
#     gmaps_place_id,
#     gmaps_rating,
#     gmaps_reviews_count,
#     map_layer,
#     map_marker_icon,
#     map_marker_color,
#     map_popup_html
#   )
#
# tbl_gmh_locations <- dplyr::bind_rows(
#   tbl_gmh_locations_properties_competitors,
#   tbl_gmh_locations_universities
# )
#
# db_seed_tbl(pool, tbl_name = "gmh.locations", tbl_data = tbl_gmh_locations)
#
# tbl_gmh_locations_db <- db_read_tbl(pool, "gmh.locations")
#
#
# tbl_gmh_competitors <- mkt_properties |>
#   dplyr::filter(is_competitor == TRUE) |>
#   dplyr::transmute(
#     competitor_name = property_name,
#     competitor_url = c("https://www.1330boylston.com", "https://www.vanness.com", "https://www.bower.com"),
#     property_id = as.integer(rep(commonwealth_id, 3))
#   ) |>
#   dplyr::left_join(
#     dplyr::select(tbl_gmh_locations_db, location_id, competitor_name = location_name),
#     by = "competitor_name"
#   )
#
# db_seed_tbl(pool, tbl_name = "gmh.competitors", tbl_data = tbl_gmh_competitors)
#
#
# tbl_gmh_universities <- mkt_universities |>
#   dplyr::select(
#     university_name,
#     university_address,
#     university_url = university_website
#   ) |>
#   dplyr::mutate(
#     property_id = as.integer(commonwealth_id)
#   ) |>
#   dplyr::left_join(
#     dplyr::select(tbl_gmh_locations_db, location_id, university_name = location_name),
#     by = "university_name"
#   )
#
# db_seed_tbl(pool, tbl_name = "gmh.universities", tbl_data = tbl_gmh_universities)

# get_city_from_address <- function(address) {
#
#   address_parts <- stringr::str_split(address, ", ") |> unlist()
#
#   if (length(address_parts) == 4) {
#     return(address_parts[2])
#   } else {
#     return(NA)
#   }
#
# }
#
# get_postal_code_from_address <- function(address) {
#
#   # "Commonwealth Ave, Boston, MA 02215, USA"
#
#   address_parts <- stringr::str_split(address, ", ") |> unlist()
#
#   if (length(address_parts) == 4) {
#     return(stringr::str_extract(address_parts[3], "\\d{5}"))
#   } else {
#     return(NA)
#   }
#
# }
#
# uni_addresses <- mkt_university_locations$address
# uni_gmaps_data <- purrr::map(uni_addresses, get_gmaps_data) |>
#   setNames(mkt_universities$university_name) |>
#   dplyr::bind_rows()
#
# uni_gmaps_data <- uni_gmaps_data[2:4, ]
# uni_gmaps_data <- uni_gmaps_data |>
#   dplyr::bind_rows(boston_unisersity_gmaps_data)
#
# tbl_gmh_universities_gmaps <- uni_gmaps_data |>
#   dplyr::select(
#     university_name = name,
#     address = original_address,
#     latitude = gmaps_latitude,
#     longitude = gmaps_longitude,
#     gmaps_url,
#     gmaps_place_id,
#     gmaps_rating,
#     gmaps_reviews_count
#   ) |>
#
#
#
# tbl_gmh_locations_universities <- mkt_universities |>
#   dplyr::select(
#     university_id,
#     location_name = university_name,
#     address = university_address,
#     website = university_website,
#     image_url = university_logo
#   ) |>
#   dplyr::left_join(
#     dplyr::select(mkt_university_locations, -address),
#     by = "university_id"
#   ) |>
#   dplyr::left_join(
#     dplyr::select(
#       uni_gmaps_data,
#       address = original_address,
#       latitude = gmaps_latitude,
#       longitude = gmaps_longitude,
#       gmaps_url,
#       gmaps_place_id,
#       gmaps_rating,
#       gmaps_reviews_count = gmaps_user_ratings_total,
#       map_marker_icon = gmaps_icon,
#       phone_number = gmaps_phone_number,
#       website = gmaps_website,
#       gmaps_url
#     ),
#     by = "address"
#   ) |>
#   dplyr::select(
#     website = website.y,
#     -website.x,
#     -latitude.x,
#     -longitude.x,
#     latitude = latitude.y,
#     longitude = longitude.y,
#     -coordinates,
#     gmaps_url = gmaps_url.y,
#     -gmaps_url.x,
#     image_url,
#     -image,
#     -popup,
#     -gmaps_url.y,
#
#
#   ) |>
#   dplyr::mutate(
#     city = Vectorize(get_city_from_address)(address),
#     state = "MA",
#     postal_code = Vectorize(get_postal_code_from_address)(address),
#     country = "USA",
#     phone_number = NA_character_,
#     email = NA_character_,
#     rating = NA_real_,
#     gmaps_rating = NA_real_,
#     gmaps_reviews_count = NA_integer_,
#     map_layer = "universities",
#     map_marker_icon = "university",
#     map_marker_color = "blue",
#     map_popup_html = paste0(
#       "<b>",
#       location_name,
#       "</b><br>",
#       address,
#       "<br>",
#       "<a href='",
#       website,
#       "' target='_blank'>Website</a>"
#     )
#   ) |>
#   dplyr::select(
#     location_name,
#     address,
#     city,
#     state,
#     postal_code,
#     country,
#     latitude,
#     longitude,
#     phone_number,
#     email,
#     website,
#     image_url,
#     rating,
#     gmaps_url,
#     gmaps_place_id,
#     gmaps_rating,
#     gmaps_reviews_count,
#     map_layer,
#     map_marker_icon,
#     map_marker_color,
#     map_popup_html
#   ) |>
#   dplyr::mutate(
#     is_active = TRUE
#   )
#
#
#   dplyr::select(
#     location_name,
#     address,
#     city,
#     state,
#     postal_code,
#     country,
#     latitude,
#     longitude,
#     phone_number,
#     email,
#     website,
#     image_url,
#     rating,
#     gmaps_url,
#     gmaps_place_id,
#     gmaps_rating,
#     gmaps_reviews_count,
#     map_layer,
#     map_marker_icon,
#     map_marker_color,
#     map_popup_html
#   )
#
#
#
#
#

