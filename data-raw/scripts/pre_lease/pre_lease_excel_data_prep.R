
source("R/utils_excel.R")

# files -----------------------------------------------------------------------------------------------------------

pre_lease_xl_path <- fs::path("data-raw/data/original/pre_lease")
pre_lease_xl_files <- fs::dir_ls(pre_lease_xl_path, type = "file", regexp = "~\\$", invert = TRUE)

# AGC Pre-Lease ---------------------------------------------------------------------------------------------------

xl_file <- pre_lease_xl_files[stringr::str_detect(pre_lease_xl_files, "AGC")]

# read in data
agc_global_summary_tbl <- xl_read_pre_lease_global_summary_tbl(xl_file)
agc_property_summary_tbls <- xl_read_pre_lease_property_summary_tbls(xl_file)
agc_property_details_tbls <- xl_read_pre_lease_property_details_tbls(xl_file, charge_codes = FALSE)
agc_report_params_tbl <- xl_read_pre_lease_report_parameters(xl_file)

# GMH (Non-AGC) ---------------------------------------------------------------------------------------------------

xl_files <- pre_lease_xl_files[!stringr::str_detect(pre_lease_xl_files, "AGC")]

pre_lease_summary_tbls <- purrr::map_dfr(
  xl_files,
  xl_read_pre_lease_property_summary_tbls,
  .id = "xl_file"
)

pre_lease_details_tbls <- purrr::map_dfr(
  xl_files,
  xl_read_pre_lease_property_details_tbls,
  charge_codes = TRUE,
  .id = "xl_file"
)

pre_lease_report_params_tbls <- purrr::map_dfr(
  xl_files,
  xl_read_pre_lease_report_parameters,
  .id = "xl_file"
)

# save ------------------------------------------------------------------------------------------------------------
csv_path <- "data-raw/data/working/pre_lease"
qs_path <- "data-raw/cache/pre_lease"

if (!fs::dir_exists(csv_path)) {
  fs::dir_create(csv_path)
}

if (!fs::dir_exists(qs_path)) {
  fs::dir_create(qs_path)
}

# save data
readr::write_csv(agc_global_summary_tbl, fs::path(csv_path, "agc_global_summary_tbl.csv"))
readr::write_csv(agc_property_summary_tbls, fs::path(csv_path, "agc_property_summary_tbls.csv"))
readr::write_csv(agc_property_details_tbls, fs::path(csv_path, "agc_property_details_tbls.csv"))
readr::write_csv(pre_lease_report_params_tbls, fs::path(csv_path, "pre_lease_report_params_tbls.csv"))

readr::write_csv(pre_lease_summary_tbls, fs::path(csv_path, "pre_lease_summary_tbls.csv"))
readr::write_csv(pre_lease_details_tbls, fs::path(csv_path, "pre_lease_details_tbls.csv"))
readr::write_csv(pre_lease_report_params_tbls, fs::path(csv_path, "pre_lease_report_params_tbls.csv"))

# save data as qs
qs::qsave(agc_global_summary_tbl, fs::path(qs_path, "agc_global_summary_tbl.qs"))
qs::qsave(agc_property_summary_tbls, fs::path(qs_path, "agc_property_summary_tbls.qs"))
qs::qsave(agc_property_details_tbls, fs::path(qs_path, "agc_property_details_tbls.qs"))
qs::qsave(pre_lease_report_params_tbls, fs::path(qs_path, "pre_lease_report_params_tbls.qs"))

qs::qsave(pre_lease_summary_tbls, fs::path(qs_path, "pre_lease_summary_tbls.qs"))
qs::qsave(pre_lease_details_tbls, fs::path(qs_path, "pre_lease_details_tbls.qs"))
qs::qsave(pre_lease_report_params_tbls, fs::path(qs_path, "pre_lease_report_params_tbls.qs"))

cli::cli_alert_success("Pre-lease data prep complete.")
cli::cli_alert_info("Data saved to {.path {csv_path}} and {.path {qs_path}}")


