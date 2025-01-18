#  ------------------------------------------------------------------------
#
# Title : GMH Investment Partners
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_partners_tbl <- readr::read_csv(
  "data-raw/data/working/gmh/gmh_partners.csv",
  col_types = readr::cols(
    .default = readr::col_character(),
    partner_id = readr::col_integer()
  )
)

gmh_partners_lst <- as.list(gmh_partners_tbl$partner_id) |>
  setNames(gmh_partners_tbl$partner_name)
