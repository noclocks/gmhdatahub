
#  ------------------------------------------------------------------------
#
# Title : Logging Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------


# logger ------------------------------------------------------------------

initialize_logger <- function(
  log_file = tempfile(pattern = "gmhdatahub", fileext = ".log")
) {

  # handle args
  logger::log_formatter(logger::formatter_glue)
  logger::log_appender(logger::appender_tee(file = log_file, append = TRUE))
  logger::log_layout(logger::layout_glue_colors)
  logger::log_threshold(logger::INFO)

  cli::cli_alert_success(
    c(
      "Logger initialized with the following setup:\n",
      "Log File: {.path {log_file}}\n",
      "Formatter: {.field logger::formatter_glue}\n",
      "Appender: {.field logger::appender_tee}\n",
      "Layout: {.field logger::layout_glue_colors}\n"
    )
  )

  return(invisible(NULL))

}

read_local_logs <- function() {
  path <- tempfile(pattern = "gmhdatahub", fileext = ".log")
  if (!file.exists(path)) {
    cli::cli_alert_warning("No log file found.")
    return(NULL)
  }
  con <- file(path, open = "r")
  logs <- readLines(con)
  close(con)
  paste(logs, collapse = "\n")
}
