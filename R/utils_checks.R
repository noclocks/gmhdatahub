validate_col_names <- function(data, cols) {
  stopifnot(all(cols %in% colnames(data)))
}


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

# database connection -----------------------------------------------------

#' Check Database Connection
#'
#' @description
#' This function checks if the provided connection is a valid database
#' connection. The function will throw an error if the connection is not
#' a valid DBI or pool connection.
#'
#' @param conn A database connection object.
#' @inheritParams rlang::args_error_context
#'
#' @returns The provided connection object invisibly if it is valid.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom DBI dbIsValid
#' @importFrom pool dbIsValid
check_db_conn <- function(
    conn,
    arg = rlang::caller_arg(conn),
    call = rlang::caller_env()
) {

  if (inherits(conn, "PqConnection")) {
    return(check_db_conn_dbi(conn, arg = arg, call = call))
  } else if (inherits(conn, "Pool")) {
    return(check_db_conn_pool(conn, arg = arg, call = call))
  } else if (inherits(conn, "connConnection")) {
    return(check_db_conn_rstudio(conn, arg = arg, call = call))
  } else {
    cli::cli_abort(
      c(
        "Invalid Database Connection Provided: {.arg {arg}}",
        "The provided connection is not a valid database connection."
      ),
      call = call
    )
  }

}

check_db_conn_pool <- function(conn, arg = rlang::caller_arg(conn), call = rlang::caller_env()) {
  if (!inherits(conn, "Pool")) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Pool Provided: {.arg {arg}}",
        "The provided connection is not a valid database connection pool."
      ),
      call = call
    )
  }

  if (!pool::dbIsValid(conn)) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Pool Provided: {.arg {arg}}",
        "The provided connection pool is not valid."
      ),
      call = call
    )
  }

  return(invisible(conn))

}

check_db_conn_dbi <- function(conn, arg = rlang::caller_arg(conn), call = rlang::caller_env()) {
  if (!inherits(conn, "PqConnection")) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Provided: {.arg {arg}}",
        "The provided connection is not a valid DBI connection."
      ),
      call = call
    )
  }

  if (!DBI::dbIsValid(conn)) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Provided: {.arg {arg}}",
        "The provided DBI connection is not valid."
      ),
      call = call
    )
  }

  return(invisible(conn))

}

check_db_conn_rstudio <- function(conn, arg = rlang::caller_arg(conn), call = rlang::caller_env()) {
  if (!inherits(conn, "connConnection")) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Provided: {.arg {arg}}",
        "The provided connection is not a valid RStudio connection."
      ),
      call = call
    )
  }

  return(invisible(conn))
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
