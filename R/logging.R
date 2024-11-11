

log_formatter_json <- function(log_entry) {
  log_entry <- jsonlite::toJSON(log_entry, auto_unbox = TRUE, pretty = TRUE)
  return(log_entry)
}

set_log_threshold <- function(level = "info") {
  if (level %in% c("info", "warning", "error")) {
    options(entrata.log = level)
  } else {
    cli::cli_alert_danger(
      c(
        "Invalid log threshold level specified.",
        "Valid levels are: 'info', 'warning', 'error'."
      )
    )
  }
}

entrata_req_log <- function(req, log_path = NULL) {

  check_request(req)

  # extract entities from request
  req_url <- purrr::pluck(req, "url")
  req_method <- purrr::pluck(req, "method")
  req_headers <- purrr::pluck(req, "headers")
  req_body <- purrr::pluck(req, "body", "data")
  req_fields <- purrr::pluck(req, "fields")
  req_options <- purrr::pluck(req, "options")
  req_policies <- purrr::pluck(req, "policies")

  req_id <- purrr::pluck(req_body, "requestId")
  req_body_json <- jsonlite::toJSON(req_body, auto_unbox = TRUE, pretty = TRUE)
  req_options_json <- jsonlite::toJSON(req_options, auto_unbox = TRUE, pretty = TRUE)
  req_policies_json <- jsonlite::toJSON(names(req_policies), auto_unbox = TRUE, pretty = TRUE)

  # get endpoint
  entrata_endpoint <- stringr::str_split_1(req_url, "/") |>
    purrr::pluck(-1) |>
    tolower()

  # get method
  entrata_method <- req_body |>
    purrr::pluck("method", "name")

  # get method version
  entrata_method_version <- req_body |>
    purrr::pluck("method", "version")

  # get method params
  entrata_method_params <- req_body |>
    purrr::pluck("method", "params")

  # log request
  log_entry <- list(
    request_id = req_id,
    endpoint = entrata_endpoint,
    method = entrata_method,
    method_version = entrata_method_version,
    method_params = entrata_method_params,
    request_url = req_url,
    request_method = req_method,
    request_headers = req_headers,
    request_body = req_body_json,
    request_fields = req_fields,
    request_options = req_options_json,
    request_policies = req_policies_json
  ) |>
    purrr::compact()

  if (is.null(log_path)) {
    log_path <- "entrata_request_log.json"
  }

  logger <- logger::logger(
    threshold = getOption("entrata.log", "INFO"),
    formatter = log_formatter_json,
    layout = logger::layout_json(),
    appender = logger::appender_file(log_path)
  )

  logger("INFO", log_entry)

  return(req)

}
