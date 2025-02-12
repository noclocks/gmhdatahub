get_xl_report_file_name <- function(
    report_name,
    report_date = Sys.Date(),
    ...
) {

  report_date_str <- format(as.Date(report_date), "%Y-%m-%d")
  report_file_ext <- ".xlsx"
  abbrs <- c("GMH")

  report_name <- snakecase::to_title_case(report_name) |>
    stringr::str_replace_all(" ", "_") |>
    stringr::str_replace_all("Gmh", "GMH")

  paste0(
    report_date_str,
    "-",
    report_name,
    report_file_ext
  )

}
