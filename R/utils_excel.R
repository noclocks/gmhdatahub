get_xl_report_file_name <- function(
    report_name,
    report_date = Sys.Date(),
    ...
) {

  report_date_str <- format(as.Date(report_date), "%Y-%m-%d")
  report_file_ext <- ".xlsx"
  abbrs <- c("GMH", "AGC", "CBRE", "JHU", "AEW", "CRG")
  replace <- c("Gmh" = "GMH", "Agc" = "AGC", "Cbre" = "CBRE", "Jhu" = "JHU", "Aew" = "AEW", "Crg" = "CRG")

  report_name <- snakecase::to_title_case(report_name) |>
    stringr::str_replace_all(" ", "_") |>
    # replace all abbreviations with uppercase
    stringr::str_replace_all(replace)

  paste0(
    report_date_str,
    "-",
    report_name,
    report_file_ext
  )

}
