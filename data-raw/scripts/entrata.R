
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

# cleanup -----------------------------------------------------------------

rm(
  entrata_config,
  csv_path,
  get_entrata_properties,
  assign_versions,
  entrata_base_request,
  entrata_leases_picklist_request,
  entrata_properties_picklist_request,
  entrata_properties_request,
  entrata_req_perform,
  entrata_req_perform_save,
  get_entrata_config,
  get_entrata_report_filters,
  get_entrata_arcodes
)

# inform ------------------------------------------------------------------

cli::cli_alert_success("Entrata data preparation completed successfully and generated the following objects:")
cli::cli_ul(ls())


