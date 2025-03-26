#  ------------------------------------------------------------------------
#
# Title : Entrata Data Preparation Scripts
#    By : Jimmy Briggs
#  Date : 2024-12-30
#
#  ------------------------------------------------------------------------

# source ------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

source("data-raw/scripts/entrata/endpoints.R")
source("data-raw/scripts/entrata/methods.R")
source("data-raw/scripts/entrata/versions.R")
source("data-raw/scripts/entrata/parameters.R")
source("data-raw/scripts/entrata/defaults.R")
source("data-raw/scripts/entrata/reports.R")
source("data-raw/scripts/entrata/properties.R")
source("data-raw/scripts/entrata/picklists.R")
source("data-raw/scripts/entrata/charge_codes.R")
source("data-raw/scripts/entrata/properties.R")

# cleanup -----------------------------------------------------------------

# remove everything but _tbl and _lst
rm(list = setdiff(ls(), c(grep("_tbl|_lst|_tbls", ls(), value = TRUE), "entrata_config")))

# inform ------------------------------------------------------------------

cli::cli_alert_success("Entrata data preparation completed successfully and generated the following objects:")
cli::cli_ul(ls())
