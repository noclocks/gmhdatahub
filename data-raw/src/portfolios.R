
#  ------------------------------------------------------------------------
#
# Title : Portfolios
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------

source("data-raw/src/properties.R")

portfolios <- tibble::tribble(
  ~portfolio_id,      ~portfolio_name,              ~property_name,
          "AGC",                "AGC",  "1047 Commonwealth Avenue",
          "AGC",                "AGC",                "Academy 65",
          "AGC",                "AGC",           "Academy Lincoln",
          "AGC",                "AGC",    "Campustown 1008 S. 4th",
          "AGC",                "AGC",  "Campustown 307 E. Daniel",
          "AGC",                "AGC",     "Campustown 501 S. 6th",
          "AGC",                "AGC",     "Campustown 908 S. 1st",
          "AGC",                "AGC",        "The Academy Chorro",
          "AGC",                "AGC",       "The Academy Palomar",
          "AGC",                "AGC",       "The Dean Campustown",
          "GMH", "All GMH Properties",  "1047 Commonwealth Avenue",
          "GMH", "All GMH Properties",                "Academy 65",
          "GMH", "All GMH Properties",           "Academy Lincoln",
          "GMH", "All GMH Properties",    "Campustown 1008 S. 4th",
          "GMH", "All GMH Properties",  "Campustown 307 E. Daniel",
          "GMH", "All GMH Properties",     "Campustown 501 S. 6th",
          "GMH", "All GMH Properties",     "Campustown 908 S. 1st",
          "GMH", "All GMH Properties",                "Life Tower",
          "GMH", "All GMH Properties",          "Shortbread Lofts",
          "GMH", "All GMH Properties",                      "SOVA",
          "GMH", "All GMH Properties",     "The Academy at Frisco",
          "GMH", "All GMH Properties",        "The Academy Chorro",
          "GMH", "All GMH Properties",    "The Academy on Charles",
          "GMH", "All GMH Properties",       "The Academy Palomar",
          "GMH", "All GMH Properties",       "The Dean Campustown",
          "GMH", "All GMH Properties",             "The Dean Reno",
          "GMH", "All GMH Properties",     "The Rise at Northgate",
          "GMH", "All GMH Properties", "The Venue at North Campus",
          "GMH", "All GMH Properties",                     "Torre",
         "CBRE",               "CBRE",                      "SOVA",
         "CBRE",               "CBRE",                     "Torre",
          "CRG",                "CRG",             "The Dean Reno",
          "MED",           "Medistar",                "Life Tower",
         "PRIN",          "Principal",          "Shortbread Lofts",
         "PRIN",          "Principal",     "The Academy at Frisco",
         "PRIN",          "Principal",    "The Academy on Charles",
         "PRIN",          "Principal",     "The Rise at Northgate"
  ) |>
  dplyr::left_join(
    properties,
    by = c("property_name" = "property_name")
  ) |>
  dplyr::select(
    portfolio_id,
    portfolio_name,
    property_id,
    property_name
  )
