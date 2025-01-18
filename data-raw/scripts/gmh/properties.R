
#  ------------------------------------------------------------------------
#
# Title : GMH Properties
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_properties_tbl <- readr::read_csv(
  "data-raw/data/working/gmh/gmh_properties.csv",
  col_types = list(
    .default = readr::col_character(),
    property_id = readr::col_integer(),
    parent_property_id = readr::col_integer(),
    portfolio_id = readr::col_integer(),
    partner_id = readr::col_integer()
  )
)

gmh_properties_lst <- as.list(gmh_properties_tbl$property_id) |>
  setNames(gmh_properties_tbl$property_name)

