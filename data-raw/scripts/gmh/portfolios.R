

#  ------------------------------------------------------------------------
#
# Title : GMH Portfolios
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_portfolios_tbl <- readr::read_csv(
  "data-raw/data/working/gmh/gmh_portfolios.csv",
  col_types = readr::cols(
    .default = readr::col_character(),
    portfolio_id = readr::col_integer(),
    partner_id = readr::col_integer()
  )
)

gmh_portfolios_lst <- as.list(gmh_portfolios_tbl$portfolio_id) |>
  setNames(gmh_portfolios_tbl$portfolio_name)
