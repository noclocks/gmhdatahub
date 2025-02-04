mod_properties_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Properties",
    "Properties data table"
  )
}

mod_property_units_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Property Units",
    "Units data table"
  )
}

mod_leases_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Leases",
    "Leases data table"
  )
}

mod_floorplans_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Floorplans",
    "Floorplans data"
  )
}

mod_residents_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Residents",
    "Residents data table"
  )
}

mod_box_score_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Box Score Report",
    "Box score metrics"
  )
}

mod_performance_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Performance Report",
    "Performance metrics"
  )
}
