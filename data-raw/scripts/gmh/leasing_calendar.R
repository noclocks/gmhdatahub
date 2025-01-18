
#  ------------------------------------------------------------------------
#
# Title : GMH Leasing Calendar Table Prep
#    By : Jimmy Briggs
#  Date : 2025-01-17
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_dates.R")

gmh_leasing_calendar_tbl <- create_leasing_calendar(start_year = 2023, num_years = 3) |>
  select(-date_key) |>
  dplyr::rename(date_key = date)
