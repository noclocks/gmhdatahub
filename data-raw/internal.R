
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
source("data-raw/scripts/survey.R")

# bundle --------------------------------------------------------------------

usethis::use_data(
  app_choices_lst,
  survey_choices_lst,
  gmh_competitors_lst,
  gmh_portfolios_lst,
  gmh_segments_lst,
  entrata_endpoints_lst,
  entrata_methods_lst,
  entrata_method_versions_lst,
  entrata_default_methods_lst,
  entrata_params_lst,
  entrata_properties_lst,
  entrata_leases_pick_list_tbls,
  entrata_properties_pick_list_tbls,
  entrata_report_names_lst,
  internal = TRUE,
  overwrite = TRUE
)
