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

mod_pre_lease_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    title = "Pre Lease Report",
    "Pre lease report content"
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

# mod_survey_admin_ui <- function(id) {
#   ns <- NS(id)
#   bslib::card(
#     title = "Survey Administration",
#     "Survey administration tools"
#   )
# }
#
# mod_survey_forms_ui <- function(id) {
#   ns <- NS(id)
#   bslib::card(
#     title = "Survey Forms",
#     "Survey form builder"
#   )
# }
#
# mod_survey_insights_ui <- function(id) {
#   ns <- NS(id)
#   bslib::card(
#     title = "Survey Insights",
#     "Survey analytics and insights"
#   )
# }
