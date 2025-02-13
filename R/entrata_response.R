#  ------------------------------------------------------------------------
#
# Title : Entrata Response
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Response
#'
#' @family Entrata
#' @name entrata_response
#'
#' @description
#' Functions for working with API responses from the Entrata API.
#'
#' @param resp An [httr2::response()] object.
#'
#' @seealso [entrata_request()]
NULL

#' Entrata Response Body
#'
#' @description
#' Extract the response body from the Entrata API response.
#'
#' @param resp `httr2::response` object.
#'
#' @returns `list` with the response body.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck
entrata_resp_body <- function(resp) {
  check_response(resp)
  check_response_json(resp)
  httr2::resp_body_json(resp)
}

entrata_resp_status <- function(resp) {
  check_response(resp)
  resp_body <- httr2::resp_body_json(resp)
  if (entrata_resp_is_error(resp)) {
    return(purrr::pluck(resp_body, "response", "error", "code"))
  } else {
    return(purrr::pluck(resp_body, "response", "result", "status"))
  }
}



entrata_resp_is_error <- function(resp) {
  check_response(resp)
  resp_content_type <- httr2::resp_content_type(resp)
  if (resp_content_type != "application/json") {
    return(FALSE)
  }
  resp_body <- httr2::resp_body_json(resp)
  if (purrr::pluck_exists(resp_body, "response", "error")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

entrata_resp_error_body <- function(resp) {
  check_response(resp)
  resp_body <- httr2::resp_body_json(resp)
  if (purrr::pluck_exists(resp_body, "response", "error")) {
    return(purrr::pluck(resp_body, "response", "error"))
  } else {
    return(NULL)
  }
}

#' Determine if the Entrata Response is Transient
#'
#' @description
#' Check if the response from Entrata is transient based on the status code
#' or headers.
#'
#' @param resp `httr2::response` object.
#'
#' @returns `logical` indicating if the response is transient.
#'
#' @export
#'
#' @importFrom httr2 resp_status resp_header_exists resp_headers
entrata_resp_is_transient <- function(resp) {
  check_response(resp)
  resp_body <- httr2::resp_body_json(resp)
  if (entrata_resp_is_error(resp)) {
    if (purrr::pluck(resp_body, "response", "error", "code") == 310) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}



entrata_resp_check_status <- function(resp, error_call = rlang::caller_env()) {

  check_response(resp)

  resp_body <- httr2::resp_body_json(resp)

  if (entrata_resp_is_error(resp)) {
    err_code <- purrr::pluck(resp_body, "response", "error", "code")
    err_msg <- purrr::pluck(resp_body, "response", "error", "message")
    cli::cli_abort(
      c(
        "Entrata API Error: {.field {err_code}}",
        "Message: {.field {err_msg}}"
      )
    )
  } else {
    return(invisible(resp))
  }
}

#' Entrata Response Success Body
#'
#' @description
#' Extract the body from a successful response.
#'
#' @param resp `httr2::response` object.
#'
#' @returns the response body.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
entrata_resp_body_success <- function(resp) {
  check_response(resp)

  httr2::resp_body_json(resp) |>
    purrr::pluck("response", "result")

}

#' Entrata Response Error Body
#'
#' @description
#' Extract the error body and message from the response.
#'
#' @param resp `httr2::response` object.
#'
#' @returns the response error body.
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck
#' @importFrom cli cli_alert_info cli_bullets
entrata_resp_body_error <- function(resp) {
  check_response(resp)

  if (!entrata_resp_is_error(resp)) {
    cli::cli_alert_info("Response is not an error.")
    return(NULL)
  }
  resp_body <- httr2::resp_body_json(resp)
  error_body <- purrr::pluck(resp_body, "response", "error")
  error_code <- purrr::pluck(error_body, "code")
  error_message <- purrr::pluck(error_body, "message")

  cli::cli_bullets(
    c(
      "i" = "Error Status Code: {error_code}",
      "i" = "Error Message: {error_message}"
    )
  )

  return(error_body)

}

#' Entrata Response Retry-After
#'
#' @description
#' Get the Retry-After time from the response headers.
#'
#' @param resp `httr2::response` object.
#'
#' @returns `numeric` indicating the time to wait before retrying,
#'   or `NULL` if not found.
#'
#' @export
#'
#' @importFrom httr2 resp_headers
entrata_resp_retry_after <- function(resp) {

  check_response(resp)

  if (!httr2::resp_header_exists(resp, "Retry-After")) {
    return(NA)
  }

  entrata_resp_headers_retry_after(resp)

}

entrata_resp_headers_retry_after <- function(resp) {

  if (!httr2::resp_header_exists(resp, "Retry-After")) {
    return(NA)
  }

  header <- httr2::resp_header(resp, header = "Retry-After")

  # check if the header is a date or an integer
  if (grepl("^[0-9]+$", header)) {
    return(
      tryCatch({
        as.numeric(header) + 1
      }, error = function(e) {
        return(NULL)
      })
    )
  } else {
    return(
      tryCatch({
        as.POSIXct(header, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
      }, error = function(e) {
        return(NULL)
      })
    )
  }

}

entrata_resp_rate_limit_headers <- function(resp) {

  headers <- .get_rate_limit_headers(resp)

  limit <- .parse_limits_remaining(headers$`x-ratelimit-limit`)
  remaining <- .parse_limits_remaining(headers$`x-ratelimit-remaining`)

  reset <- as.integer(strsplit(headers$`x-ratelimit-reset`, ";")[[1]])

  expires <- if (!is.null(headers$expires)) {
    as.POSIXct(headers$expires, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  } else {
    NA
  }

  list(
    limit = list(
      day = limit[[1]]$value,
      hour = limit[[2]]$value,
      minute = limit[[3]]$value
    ),
    remaining = list(
      day = remaining[[1]]$value,
      hour = remaining[[2]]$value,
      minute = remaining[[3]]$value
    ),
    reset = list(
      day = reset[1],
      hour = reset[2],
      minute = reset[3]
    ),
    expires = expires
  )
}


.get_rate_limit_headers <- function(resp) {
  httr2::resp_headers(
    resp,
    filter = "x-ratelimit-limit|x-ratelimit-remaining|x-ratelimit-reset|expires"
  )
}

.parse_limits_remaining <- function(header) {
  limits <- strsplit(header, ";")[[1]]
  lapply(limits, function(limit) {
    parts <- strsplit(limit, "/")[[1]]
    list(value = as.integer(parts[1]), period = ifelse(length(parts) > 1, parts[2], NA))
  })
}

# utility -----------------------------------------------------------------

get_response_endpoint <- function(resp) {
  check_http_response(resp)
  url <- purrr::pluck(resp, "url")
  if (is.null(url)) return(NULL)
  hold <- basename(url)
  if (!(hold %in% entrata_endpoints)) {
    cli::cli_alert_warning(
      "Request endpoint {.field {hold}} is not a valid Entrata API endpoint."
    )
    return(hold)
  }
  return(hold)
}

get_response_request_body <- function(resp) {
  check_response(resp)
  req <- purrr::pluck(resp, "request")
  check_request(req)
  purrr::pluck(req, "body", "data")
}

get_response_request_method <- function(resp) {
  check_response(resp)
  req <- purrr::pluck(resp, "request")
  check_request(req)
  purrr::pluck(req, "body", "data", "method")
}

get_response_request_method_name <- function(resp) {
  check_response(resp)
  req <- purrr::pluck(resp, "request")
  check_request(req)
  purrr::pluck(get_request_method(req), "name")
}

ge_response_request_method_version <- function(resp) {
  check_response(resp)
  req <- purrr::pluck(resp, "request")
  check_request(req)
  purrr::pluck(get_request_method(req), "version")
}

get_response_request_method_params <- function(resp) {
  check_response(resp)
  req <- purrr::pluck(resp, "request")
  check_request(req)
  purrr::pluck(get_request_method(req), "params")
}

get_response_request_id <- function(resp) {
  check_response(resp)
  req <- purrr::pluck(resp, "request")
  check_request(req)
  purrr::pluck(req, "body", "data", "requestId")
}
