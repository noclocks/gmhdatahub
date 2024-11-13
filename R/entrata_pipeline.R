
#  ------------------------------------------------------------------------
#
# Title : Entrata API Data Pipeline
#    By : Jimmy Briggs
#  Date : 2024-11-12
#
#  ------------------------------------------------------------------------


# investment partner/portfolio mapping ------------------------------------

# investment_partners_mapping <- readr::read_csv("data-raw/investment_partners.csv")

map_property_to_investment_partner <- function(property_name) {
  investment_partners_mapping |>
    dplyr::filter(.data$property_name == .env$property_name) |>
    dplyr::pull(.data$investment_partner)
}


# manual bed counts -------------------------------------------------------

# model_beds_tbl <- investment_partners_mapping |>
#   dplyr::mutate(model_beds = 0) |>
#   dplyr::arrange(.data$property_name)
#
# model_beds_tbl$model_beds[1] <- 4

# get main summary table --------------------------------------------------

# pre_lease_summary_data <- entrata_pre_lease_report()

