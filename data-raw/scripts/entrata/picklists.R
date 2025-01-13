
#  ------------------------------------------------------------------------
#
# Title : Entrata Pick Lists
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

source("data-raw/scripts/entrata/picklists/properties_picklists.R")
source("data-raw/scripts/entrata/picklists/leases_picklists.R")

csv_path <- "data-raw/data/working/entrata/picklists/"
fs::dir_create(csv_path)

# write csv files
purrr::walk2(
  entrata_properties_pick_list_tbls,
  names(entrata_properties_pick_list_tbls),
  function(data, file_name) {
    readr::write_csv(data, paste0(csv_path, file_name, ".csv"), append = FALSE)
  }
)

purrr::walk2(
  entrata_leases_pick_list_tbls,
  names(entrata_leases_pick_list_tbls),
  function(data, file_name) {
    readr::write_csv(data, paste0(csv_path, file_name, ".csv"), append = FALSE)
  }
)

cli::cli_alert_success(
  c(
    "Successfully derived the Entrata Pick Lists (Properties & Leases).\n",
    "Derived CSV files are located at: {.path {csv_path}}."
  )
)

# cleanup
rm(csv_path)
