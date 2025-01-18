
#  ------------------------------------------------------------------------
#
# Title : Exported Data
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

usethis::use_data(entrata_methods_tbl, internal = FALSE, overwrite = TRUE)
usethis::use_data(property_summary_inputs_tbl, internal = FALSE, overwrite = TRUE)
usethis::use_data(leasing_summary_inputs_tbl, internal = FALSE, overwrite = TRUE)
usethis::use_data(gmh_competitors_tbl, internal = FALSE, overwrite = TRUE)
usethis::use_data(gmh_partners_tbl, internal = FALSE, overwrite = TRUE)
usethis::use_data(gmh_portfolios_tbl, internal = FALSE, overwrite = TRUE)
