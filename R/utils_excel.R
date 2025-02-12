get_xl_report_file_name <- function(
    report_name,
    report_date = Sys.Date(),
    ...
) {

  report_date_str <- format(as.Date(report_date), "%Y-%m-%d")
  report_file_ext <- ".xlsx"
  abbrs <- c("GMH")
  report_name <- snakecase::to_screaming_snake_case(
    report_name,
    abbreviations = abbrs
  )


  paste0(
    report_date_str,
    "-",
    report_name,
    report_file_ext
  )

}
