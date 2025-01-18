
#  ------------------------------------------------------------------------
#
# Title : Original Excel Market Survey Data
#    By : Jimmy Briggs
#  Date : 2025-01-17
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_excel.R")

wb_path <- "data-raw/data/original/market_survey/UNLOCKED_1047_Commonwealth_Market_Survey_Updated.xlsm"

commonwealth_market_survey_data <- read_raw_market_survey_data(
  wb_path,
  sheet = "1047 Commonwealth",
  property_id = 739085,
  property_name = "1047 Commonwealth Avenue",
  leasing_week = get_leasing_week_start_date()
)
