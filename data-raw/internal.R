
#  ------------------------------------------------------------------------
#
# Title : Internal Data Preparation
#    By : Jimmy Briggs <jimmy.briggs@noclocks.dev>
#  Date : 2024-11-23
#
#  ------------------------------------------------------------------------

# source --------------------------------------------------------------------

source("data-raw/scripts/entrata.R")
source("data-raw/scripts/gmh.R")
source("data-raw/scripts/shiny.R")

# bundle --------------------------------------------------------------------

usethis::use_data(
  app_choices,
  gmh_competitors_lst,
  gmh_competitors_tbl,
  gmh_investment_partners_tbl,
  gmh_portfolios_lst,
  gmh_portfolios_tbl,
  gmh_incentives,
  gmh_comp_statuses,
  gmh_product_types,
  gmh_property_statuses,
  gmh_property_types,
  gmh_segments_lst,
  gmh_reporting_cycles,
  entrata_endpoints,
  entrata_methods_lst,
  entrata_methods_tbl,
  entrata_method_versions_lst,
  entrata_method_versions_tbl,
  entrata_default_methods,
  entrata_params_lst,
  entrata_params_tbl,
  entrata_properties_lst,
  entrata_properties_tbl,
  entrata_leases_pick_list_tbls,
  entrata_properties_pick_list_tbls,
  entrata_report_names,
  entrata_report_versions_tbl,
  entrata_report_latest_versions_tbl,
  entrata_charge_codes_tbl,
  internal = TRUE,
  overwrite = TRUE
)
