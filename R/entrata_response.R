
#  ------------------------------------------------------------------------
#
# Title : Entrata Response
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------


# entrata_response --------------------------------------------------------


# entrata_resp_body_json --------------------------------------------------

#' Entrata Response Body JSON
#'
#' @description
#' This function extracts the JSON response body from an Entrata API response.
#'
#' @details
#' This function is used to extract the JSON response body from an Entrata API
#' response object. The function will check the response object for the expected
#' structure and return the response body as a list containing the request ID,
#' status code, and result object.
#'
#' @param resp The Entrata API response object. Must be a valid HTTP response.
#'
#' @return A list containing the request ID, status code, and result object.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck_exists pluck
#' @importFrom cli cli_alert_danger
entrata_resp_body_json <- function(resp) {

  check_response(resp)
  httr2::resp_check_status(resp)
  httr2::resp_check_content_type(resp)

  if (!httr2::resp_check_content_type())

    resp_body <- httr2::resp_body_json(resp)

  if (!purrr::pluck_exists(resp_body, "response")) {
    cli::cli_alert_danger(
      c(
        "The Entrata API response does not contain the expected 'response' object.",
        "The response body may be malformed or incomplete."
      )
    )
    return(resp)
  }

  # parse request id, status code and body response/result
  resp_req_id <- purrr::pluck(resp_body, "response", "requestId")
  resp_status <- purrr::pluck(resp_body, "response", "code")
  resp_result <- purrr::pluck(resp_body, "response", "result")

  # return response body JSON
  return(list(
    request_id = resp_req_id,
    status = resp_status,
    result = resp_result
  ))

}

# entrata_resp_is_error ---------------------------------------------------

entrata_resp_is_error <- function(resp) {

  check_response(resp)

  resp_body <- httr2::resp_body_json(resp)

  if (purrr::pluck_exists(resp_body, "response", "error")) {
    return(TRUE)
  }

  return(FALSE)

}


# entrata_resp_body_error -------------------------------------------------

entrata_resp_body_error <- function(resp, ...) {

  check_response(resp)

  if (!entrata_resp_is_error(resp)) {
    cli::cli_alert_info(
      c(
        "The Entrata API response does not contain an error object."
      )
    )
    return(NULL)
  }

  resp_body <- httr2::resp_body_json(resp)
  err <- purrr::pluck(resp_body, "response", "error")
  err_code <- purrr::pluck(err, "code")
  err_msg <- purrr::pluck(err, "message")

  cli::cli_bullets(
    c(
      "i" = "HTTP Error Status Code: {err_code}",
      "i" = "HTTP Error Message: {err_msg}"
    )
  )

  return(err)

}

# entrata_resp_status -----------------------------------------------------

#' Entrata API Response Status Helpers
#'
#' @description
#' Helper functions for working with the Entrata API response status codes,
#' descriptions, and errors.
#'
#' - `entrata_resp_status()` retrieves the integer `HTTP` status code from an
#'   Entrata API response.
#' - `entrata_resp_status_desc()` retrieves a brief textual description of the
#'   `HTTP` status code from an Entrata API response.
#' - `entrata_resp_is_error()` determines if the response is an error, i.e.
#'   it returns `TRUE` if the status code is greater than or equal to `400`.
#' - `entrata_resp_check_status()` checks the status of an Entrata API response
#'   by turning HTTP/Entrata specific errors into R errors.
#' - `entrata_resp_abort()` aborts the response if the status is an error.
#'
#' Note that these functions are mostly for internal use but are exported
#' for clarity and consistency.
#'
#' @details
#' These functions are used to help with the processing of Entrata API
#' responses.
#'
#' The Entrata API is unique in that all endpoints utilize `POST` `HTTP` methods
#' and the API will always return a `200` status code, regardless of the success
#' or failure of the request.
#'
#' This makes the usage of the default `httr2` functions like [httr2::resp_check_status()]
#' or [httr2::resp_is_error()] useless as they rely on the `HTTP` status code to
#' determine if a request was successful or not.
#'
#' To mitigate this, we must inspect the response body to determine if the request
#' was successful or not. If the response body contains an error message, we can
#' assume that the request was not successful.
#'
#' @section Functions:
#'
#' - `entrata_resp_status()` - Extract the status code from an Entrata API response.
#' - `entrata_resp_status_desc()` - Extract the status description from an Entrata API response.
#' - `entrata_resp_is_error()` - Determine if the response is an error.
#' - `entrata_resp_check_status()` - Check the status of an Entrata API response.
#' - `entrata_resp_abort()` - Abort the requested response using the response object.
#'
#' @seealso [httr2::resp_status()]
#'
#' @param resp [httr2::response()] object
#' @param info Additional information to include in the error message.
#' @param error_call The calling environment for the error. Defaults to the
#'   caller environment.
#' @inheritParams httr2::resp_status
#' @inheritParams rlang::args_error_context
#'
#' @return
#' - `entrata_resp_status()` - The status code of the response.
#' - `entrata_resp_status_desc()` - The status description of the response.
#' - `entrata_resp_is_error()` - Logical value indicating whether the response is an error.
#' - `entrata_resp_check_status()` - Invisibly returns the response if the status is not an error.
#' - `entrata_resp_abort()` - Aborts the response if the status is an error.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck_exists pluck
entrata_resp_status <- function(resp) {

  resp_body <- httr2::resp_body_json(resp)

  if (purrr::pluck_exists(resp_body, "response", "error")) {
    status_code <- purrr::pluck(resp_body, "response", "error", "code") |>
      as.integer()
  } else {
    status_code <- purrr::pluck(resp_body, "response", "code") |>
      as.integer()
  }

  status_code

}

#' @rdname entrata_resp_status
#' @export
entrata_resp_status_desc <- function(resp) {

  status <- entrata_resp_status(resp)

  if (status %in% http_statuses) {
    http_statuses[[as.character(status)]]
  } else {
    NA_character_
  }

}

#' @rdname entrata_resp_status
#' @export
#' @importFrom rlang caller_env
#' @importFrom glue glue
entrata_resp_abort <- function(resp, req, info = NULL, call = rlang::caller_env()) {

  status <- entrata_resp_status(resp)
  desc <- entrata_resp_status_desc(resp)
  message <- glue::glue("HTTP {status} {desc}.")

  rlang::abort(
    c(message, info),
    status = status,
    resp = resp,
    class = c(
      glue::glue("entrata_http_{status}"),
      glue::glue("httr2_http_{status}"),
      "httr2_http",
      "httr2_error"
    ),
    request = req,
    call = call
  )

}

#' @rdname entrata_resp_status
#' @export
entrata_resp_check_status <- function(resp, info = NULL, error_call = rlang::caller_env()) {

  if (!entrata_resp_is_error(resp)) {
    invisible(resp)
  } else {
    entrata_resp_abort(resp, httr2:::req_info(resp), info, error_call)
  }
}

# entrata_resp_is_transient -----------------------------------------------


