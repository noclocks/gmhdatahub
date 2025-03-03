
#  ------------------------------------------------------------------------
#
# Title : Checking Utilities
#    By : Jimmy Briggs
#  Date : 2024-11-29
#
#  ------------------------------------------------------------------------

# entrata -----------------------------------------------------------------

#' `httr2` Checks
#'
#' @name httr2_checks
#'
#' @description
#' These functions check the validity of HTTP request and response objects as
#' well as HTTP headers by checking the R object's class against the classes
#' defined in the `httr2` package.
#'
#' @param req The HTTP response object to check.
#' @param resp The HTTP response object to check.
#' @param headers The HTTP headers object to check.
#' @inheritParams rlang::args_error_context
#'
#' @return
#' Invisibly returns `NULL` if the request/response/headers is valid, otherwise
#' will throw an error.
#'
#' @seealso [httr2::request()], [httr2::response()], [httr2::req_headers()],
#'   [httr2::resp_headers()]
NULL

#' @rdname httr2_checks
#' @export
#' @importFrom rlang caller_arg caller_env
check_request <- function(
    req,
    arg = rlang::caller_arg(req),
    call = rlang::caller_env()
) {
  if (is_request(req)) { return(invisible(req)) }
  stop_input_type(
    req,
    "an Entrata API HTTP request object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

check_http_request <- function(
    req,
    arg = rlang::caller_arg(req),
    call = rlang::caller_env()
) {
  if (is_http_request(req)) { return(invisible(req)) }
  stop_input_type(
    req,
    "an HTTP request object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

check_entrata_request <- function(
    req,
    arg = rlang::caller_arg(req),
    call = rlang::caller_env()
) {
  if (is_entrata_request(req)) { return(invisible(req)) }
  stop_input_type(
    req,
    "an Entrata API HTTP request object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

#' @rdname httr2_checks
#' @export
#' @importFrom rlang caller_arg caller_env
check_response <- function(
    resp,
    arg = rlang::caller_arg(resp),
    call = rlang::caller_env()
) {
  # if (is_http_response(resp) && !is_entrata_response(resp)) {
  # resp <- as_entrata_response(resp)
  # }
  if (is_http_response(resp)) { return(invisible(resp)) }
  stop_input_type(
    resp,
    "an Entrata API HTTP response object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

check_response_json <- function(
    resp,
    arg = rlang::caller_arg(resp),
    call = rlang::caller_env()
) {
  if (httr2::resp_content_type(resp) != "application/json") {
    cli::cli_abort(
      c(
        "Provided API response {.arg {arg}} is not JSON.",
        " Detected content type of: {.field {httr2::resp_content_type(resp)}}"
      ),
      call = call
    )
  }
}

check_http_response <- function(
    resp,
    arg = rlang::caller_arg(resp),
    call = rlang::caller_env()
) {
  if (is_http_response(resp)) { return(invisible(resp)) }
  stop_input_type(
    resp,
    "an HTTP response object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

check_entrata_response <- function(
    resp,
    arg = rlang::caller_arg(resp),
    call = rlang::caller_env()
) {
  if (is_entrata_response(resp)) { return(invisible(resp)) }
  stop_input_type(
    resp,
    "an Entrata API HTTP response object",
    allow_null = FALSE,
    arg = arg,
    call = call
  )
}

#' @keywords internal
#' @noRd
is_request <- function(req) {
  # is_entrata_request(req) && is_http_request(req)
  is_http_request(req)
}

#' @keywords internal
#' @noRd
is_response <- function(resp) {
  is_http_response(resp)
}

#' @keywords internal
#' @noRd
is_entrata_request <- function(req) {
  inherits(req, "entrata_request")
}

#' @keywords internal
#' @noRd
is_entrata_response <- function(resp) {
  inherits(resp, "entrata_response")
}

# httr2 -------------------------------------------------------------------

#' @keywords internal
#' @noRd
is_http_request <- function(req) {
  inherits(req, "httr2_request")
}

#' @keywords internal
#' @noRd
is_http_response <- function(resp) {
  inherits(resp, "httr2_response")
}

#' @keywords internal
#' @noRd
is_http_headers <- function(headers) {
  inherits(headers, "httr2_headers")
}


# tibbles -----------------------------------------------------------------

check_tibble <- function(
    tbl,
    arg = rlang::caller_arg(tbl),
    call = rlang::caller_env()
) {
  if (!tibble::is_tibble(tbl)) {
    cli::cli_abort(
      c(
        "Invalid {.arg {arg}}",
        "The provided object is not a tibble."
      ),
      call = call
    )
  }
  return(invisible(tbl))
}

check_tibble_cols <- function(
    tbl,
    cols,
    arg = rlang::caller_arg(tbl),
    call = rlang::caller_env()
) {
  check_tibble(tbl, arg = arg, call = call)
  cols <- rlang::arg_match(cols, colnames(tbl))
  return(invisible(cols))
}


# columns -----------------------------------------------------------------

#' Validate Column Names
#'
#' @description
#' This function validates the column names of a data frame. It is used
#' throughout the package to ensure that the data frame has the required
#' columns before attempting to use or process the data.
#'
#' @param data The data frame to validate against.
#' @param req_cols Character vector of required column names.
#' @param optional_cols Character vector of optional column names.
#' @inheritParams rlang::args_error_context
#'
#' @returns
#' Returns invisibly if the data frame has the required columns.
#'
#' @export
#'
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang caller_env
#'
#' @examples
#' validate_col_names(mtcars, c("mpg", "cyl"))
validate_col_names <- function(
    data,
    req_cols,
    optional_cols = NULL,
    call = rlang::caller_env()
) {

  stopifnot(is.data.frame(data) || tibble::is_tibble(data))

  if (is.null(optional_cols)) {
    optional_cols <- c()
  }

  missing_req_cols <- setdiff(req_cols, colnames(data))
  if (length(missing_req_cols) > 0) {
    cli::cli_abort(
      c(
        "The following required columns are missing from the provided {.arg data}:\n",
        "{.field {missing_req_cols}}"
      ),
      call = call
    )
  }

  missing_opt_cols <- setdiff(optional_cols, colnames(data))
  if (length(missing_opt_cols) > 0) {
    cli::cli_alert_warning(
      c(
        "The following optional columns are missing from the provided {.arg data}:\n",
        "{.field {missing_opt_cols}}"
      ),
      call = call
    )
  }

}

# logic -------------------------------------------------------------------

check_in_set <- function(x, set, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!all(x %in% set)) {
    cli::cli_abort(
      "Invalid value(s) provided for {.arg {arg}}. Must be one of: {.field {set}}",
      call = call
    )
  }
  return(TRUE)
}

check_entrata_date <- function(date, arg = rlang::caller_arg(date), call = rlang::caller_env()) {
  if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) {
    cli::cli_abort(
      c(
        "Invalid Date Format Provided: {.arg {arg}}",
        "The provided date is not in the format 'MM/DD/YYYY'."
      ),
      call = call
    )
  }
  return(TRUE)
}

check_entrata_daterange <- function(daterange, arg = rlang::caller_arg(daterange), call = rlang::caller_env()) {
  if (!is.list(daterange) || !all(c("daterange-start", "daterange-end") %in% names(daterange))) {
    cli::cli_abort(
      c(
        "Invalid Date Range Provided: {.arg {arg}}",
        "The provided date range must be a list with 'daterange-start' and 'daterange-end' keys."
      ),
      call = call
    )
  }
  check_entrata_date(daterange[[1]], call = call)
  check_entrata_date(daterange[[2]], call = call)
  if (as.Date(daterange[[1]]) > as.Date(daterange[[2]])) {
    cli::cli_abort(
      c(
        "Invalid Date Range Provided: {.arg {arg}}",
        "The start date must be before the end date."
      ),
      call = call
    )
  }
  return(TRUE)
}


# dates --------------------------------------------------------------------

#' Check Date
#'
#' @description
#' This function checks if the provided date is a valid date object.
#'
#' @param date A date object.
#' @inheritParams rlang::args_error_context
#'
#' @returns The provided date object invisibly if it is valid.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom lubridate is.Date is.POSIXt
check_date <- function(
    date,
    arg = rlang::caller_arg(date),
    call = rlang::caller_env()
) {
  if (!lubridate::is.Date(date) && !lubridate::is.POSIXt(date)) {
    cli::cli_abort(
      c(
        "Invalid Date Provided: {.arg {arg}}",
        "The provided date is not a valid date object."
      ),
      call = call
    )
  }
  return(invisible(date))
}

# files and paths --------------------------------------------------------------------

check_path <- function(
    path,
    arg = rlang::caller_arg(path),
    call = rlang::caller_env()
) {
  if (!fs::dir_exists(path) && !fs::file_exists(path)) {
    cli::cli_abort(
      c(
        "Invalid Path Provided: {.arg {arg}}",
        "The provided path does not exist."
      ),
      call = call
    )
  }
  return(invisible(path))
}
