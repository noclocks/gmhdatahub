
#  ------------------------------------------------------------------------
#
# Title : Entrata Versions
#    By : Jimmy Briggs
#  Date : 2024-12-30
#
#  ------------------------------------------------------------------------

# r2 & r3 methods ---------------------------------------------------------

r2_methods <- c(
  "getLeases",
  "getLeaseDetails",
  "getReportInfo",
  "getDependentFilter",
  "getPropertyPickLists",
  "getFinancialPickList",
  "getLeadPickLists"
)

r3_methods <- c(
  "getReportData"
)

# entrata_method_versions -------------------------------------------------

if (!exists("entrata_methods_tbl")) {
  source("data-raw/scripts/entrata/methods.R")
}

entrata_method_names <- entrata_methods_tbl$method_name

entrata_method_versions_lst <- assign_versions(entrata_method_names) |>
  setNames(entrata_method_names) |>
  as.list()

entrata_method_versions_tbl <- tibble::enframe(entrata_method_versions_lst, name = "method_name", value = "version")

# cleanup
rm(
  r2_methods,
  r3_methods,
  entrata_method_names
)
