
#  ------------------------------------------------------------------------
#
# Title : Portfolios
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------

portfolio_names <- c(
  "CRG",
  "Medistar",
  "ASC",
  "Principal",
  "CBRE",
  "All GMH Properties"
)

portfolio_types <- c(
  "Equity Partner",
  "Owner",
  "Equity Partner",
  "Equity Partner",
  "Equity Partner",
  "Property Type"
)




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

portfolio_assignments <- tibble::tibble(
  portfolio = rep(
    c("AGC", "CBRE", "CRG", "Medistar", "Principal", "GMH"),
    c(10L, 2L, 1L, 1L, 4L, 19L)
  ),
  property_id = c(
    739085, 739085, 739079, 739080, 739084, 641240, 676055, 535270, 676054,
    833617, 1143679, 952515, 1197887, 0, 518044, 518041, 518042, 518046, 1311849,
    0, 1197887, 1143679, 739085, 739085, 739079, 739080, 739084, 641240, 676055,
    518044, 952515, 518041, 535270, 518042, 676054, 518046, 833617
  ),
  property_name = c(
    "Campustown 1008 S. 4th", "1047 Commonwealth Avenue",
    "Campustown 307 E. Daniel", "Campustown 501 S. 6th", "Campustown 908 S. 1st",
    "Academy 65", "Academy Lincoln", "The Academy Chorro", "The Academy Palomar",
    "The Dean Campustown", "Torre", "SOVA", "The Dean Reno", "Life Tower",
    "Shortbread Lofts", "The Academy at Frisco", "The Academy on Charles",
    "The Rise at Northgate", "The Venue at North Campus", "Life Tower",
    "The Dean Reno", "Torre", "Campustown 1008 S. 4th",
    "1047 Commonwealth Avenue", "Campustown 307 E. Daniel",
    "Campustown 501 S. 6th", "Campustown 908 S. 1st", "Academy 65",
    "Academy Lincoln", "Shortbread Lofts", "SOVA", "The Academy at Frisco",
    "The Academy Chorro", "The Academy on Charles", "The Academy Palomar",
    "The Rise at Northgate", "The Dean Campustown"
  ),
) |>
  structure(
    spec = list(
      cols = list(
        portfolio = list() |>
          structure(class = c("collector_character", "collector")),
        property_id = list() |>
          structure(class = c("collector_double", "collector")),
        property_name = list() |>
          structure(class = c("collector_character", "collector"))
      ),
      default = list() |>
        structure(class = c("collector_guess", "collector")),
      delim = ","
    ) |>
      structure(class = "col_spec"),
    problems = constructive::.xptr("000000802B74F0C0"),
    class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame")
  )
