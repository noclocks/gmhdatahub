
#  ------------------------------------------------------------------------
#
# Title : Property Competitors
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_competitors_tbl <- readr::read_csv(
  "data-raw/data/working/gmh/gmh_competitors.csv",
  col_types = readr::cols(
    .default = readr::col_character(),
    competitor_id = readr::col_integer(),
    property_id = readr::col_integer()
  )
)

gmh_competitors_lst <- as.list(gmh_competitors_tbl$competitor_id) |>
  setNames(gmh_competitors_tbl$competitor_name)

# commonwealth_id <- get_property_id_by_name("1047 Commonwealth Avenue")
#
# gmh_competitors_tbl <- tibble::tibble(
#   competitor_id = c(1, 2, 3),
#   competitor_name = c("1330 Boylston", "Van Ness", "Bower"),
#   competitor_website = c("https://www.1330boylston.com", "https://www.vanness.com", "https://www.bower.com"),
#   property_id = rep(commonwealth_id, 3)
# )
#
# commonwealth_id <- entrata_properties_lst[["1047 Commonwealth Avenue"]]
#
# gmh_competitors_tbl <- tibble::tibble(
#   competitor_id = c(1, 2, 3),
#   competitor_name = c("1330 Boylston", "Van Ness", "Bower"),
#   competitor_url = c("https://www.1330boylston.com", "https://www.vanness.com", "https://www.bower.com"),
#   location_id = c(NA, NA, NA),
# )
#
# gmh_competitors_lst <- list(
#   "739085" = as.list(gmh_competitors_tbl$competitor_id) |>
#     setNames(gmh_competitors_tbl$competitor_name)
# )
#
# rm(commonwealth_id)
