#  ------------------------------------------------------------------------
#
# Title : GMH Segments
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_segments_tbl <- readr::read_csv(
  "data-raw/data/working/gmh/gmh_segments.csv",
  col_types = readr::cols(
    segment_id = readr::col_integer(),
    segment_name = readr::col_character(),
    segment_description = readr::col_character(),
    segment_url = readr::col_character(),
    segment_logo_url = readr::col_character(),
    segment_banner_url = readr::col_character()
  )
)

gmh_segments_lst <- gmh_segments_tbl$segment_id |>
  setNames(gmh_segments_tbl$segment_name) |>
  as.list()
