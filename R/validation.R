
#  ------------------------------------------------------------------------
#
# Title : Validation Functions
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------


# entrata request validation ----------------------------------------------

entrata_req_validate <- function(
    req,
    arg = rlang::caller_arg(req),
    call = rlang::caller_env()
) {

  # preliminary check
  check_request(req, arg = arg, call = call)

  # extract entities from request
  req_url <- purrr::pluck(req, "url")
  req_method <- purrr::pluck(req, "method")
  req_body <- purrr::pluck(req, "body", "data")
  req_headers <- purrr::pluck(req, "headers")
  req_fields <- purrr::pluck(req, "fields")
  req_policies <- purrr::pluck(req, "policies")

  # parse req_body.method:
  req_method_name <- purrr::pluck(req_body, "method", "name")
  req_method_version <- purrr::pluck(req_body, "method", "version")
  req_method_params <- purrr::pluck(req_body, "method", "params")

  # validate HTTP Method
  if (req_method != "POST") {
    cli::cli_abort("The request method must be POST.", call = call)
  }

  # extract endpoint from url
  base_url_ <- "https://gmhcommunities.entrata.com/api/v1/"
  req_endpoint <- gsub(paste0("^", base_url_), "", req_url)

  # validate URL and Endpoint
  if (!grepl(base_url_, req_url)) {
    cli::cli_abort(
      "The request URL must begin with {.field {base_url_}}.",
      call = call
    )
  }
  if (!(endpoint %in% entrata_endpoints)) {
    cli::cli_abort(
      "The request endpoint {.field {endpoint}} is not a valid Entrata API endpoint.",
      call = call
    )
  }

  # validate entrata endpoint method
  if (!(req_method_name %in% entrata_methods[[req_endpoint]])) {
    cli::cli_abort(
      "The request method {.field {req_method_name}} is not a valid Entrata API method for the endpoint {.field /{req_endpoint}}.",
      call = call
    )
  }

  # validate method version
  if (!(req_method_version %in% entrata_methods[[req_endpoint]][[req_method_name]])) {
    cli::cli_abort(
      "The request method version {.field {req_method_version}} is not a valid Entrata API method version for the endpoint {.field /{req_endpoint}}.",
      call = call
    )
  }

  # headers validation
  req_headers <- c("Authorization", "Content-Type")
  missing_headers <- setdiff(req_headers, names(req$headers))
  if (length(missing_headers) > 0) {
    cli::cli_abort(
      "Missing required headers: {.field {missing_headers}}."
    )
  }

  # ensure content-type is correct
  content_type <- req$headers$`Content-Type`
  if (!grepl("application/json", content_type)) {
    cli::cli_abort("Invalid `Content-Type` Header: {field {content_type}}, must include 'application/json'.")
  }

  # request body validation
  req_body <- req$body$data

  if (!is.list(req_body) || !all(names(entrata_default_req_body) %in% names(req_body))) {
    cli::cli_abort(
      "The request body must be a list containing the following fields: {.field {names(entrata_default_req_body)}}."
    )
  }

  # request body method object
  req_body_method <- req_body$method

  if (!is.list(req_body_method) || !all(names(entrata_default_req_body$method) %in% names(req_body_method))) {
    cli::cli_abort(
      "The request body method object must be a list containing the following fields: {.field {names(entrata_default_req_body$method)}}."
    )
  }

  return(req)


}

# request validation ------------------------------------------------------

#' Validate Entrata API Request
#'
#' @family Entrata
#' @family Validation
#'
#' @description
#' This function validates an Entrata API request to ensure that the provided
#' endpoint, method, method parameters, and method version are valid.
#'
#' @details
#' This function is used to validate an Entrata API request before sending it
#' to the Entrata API. It checks that the provided endpoint is valid, that the
#' method is valid for the endpoint, that the method parameters are valid for
#' the method, and that the method version is valid if provided.
#'
#' @param endpoint The Entrata API endpoint to validate.
#' @param method The method to validate for the provided endpoint.
#' @param method_params A list of parameters to validate for the method.
#' @param method_version The version of the method to validate. Default is `NULL`.
#' @param arg_endpoint,arg_method,arg_method_params,arg_method_version Internal
#'   arguments for capturing the caller's argument names.
#' @inheritParams rlang::args_error_context
#'
#' @return All validation functions are used for their side effects and have
#'   no return values, except for the main `validate_entrata_request()` function,
#'   which will invisibly return `TRUE` if the request is valid.
#'
#' @export
#'
#' @seealso [Entrata API Documentation](https://docs.entrata.com/api/v1/documentation)
#'
#' @importFrom cli cli_alert_success cli_alert_info cli_alert_warning cli_abort
#' @importFrom dplyr filter pull
#' @importFrom rlang caller_arg caller_env
validate_entrata_request <- function(
    endpoint,
    method,
    method_params,
    method_version = NULL,
    arg_endpoint = rlang::caller_arg(endpoint),
    arg_method = rlang::caller_arg(method),
    arg_method_params = rlang::caller_arg(method_params),
    arg_method_version = rlang::caller_arg(method_version),
    call = rlang::caller_env()
) {

  validate_entrata_request_endpoint(
    endpoint,
    arg = arg_endpoint,
    call = call
  )

  validate_entrata_request_endpoint_method(
    endpoint,
    method,
    arg_endpoint = arg_endpoint,
    arg_method = arg_method,
    call = call
  )

  validate_entrata_endpoint_method_params(
    endpoint,
    method,
    method_params,
    arg_endpoint = arg_endpoint,
    arg_method = arg_method,
    arg_method_params = arg_method_params,
    call = call
  )

  if (!is.null(method_version)) {
    validate_entrata_method_version(
      endpoint,
      method,
      method_version,
      arg_endpoint = arg_endpoint,
      arg_method = arg_method,
      arg_method_version = arg_method_version,
      call = call
    )
  }

  cli::cli_alert_success("The Entrata Request is valid.")

  return(invisible(TRUE))

}

#' @rdname validate_entrata_request
#' @export
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg caller_env
validate_entrata_request_endpoint <- function(
    endpoint,
    arg = rlang::caller_arg(endpoint),
    call = rlang::caller_env()
) {
  if (!endpoint %in% entrata_endpoints) {
    cli::cli_abort(
      c(
        "Invalid endpoint: {.field {endpoint}}",
        "{.arg {arg}} is not a valid Entrata API endpoint.",
        "i" = "Valid endpoints are: {.val {entrata_endpoints}}"
      ),
      call = call
    )
  }
}

#' @rdname validate_entrata_request
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr filter pull
#' @importFrom rlang caller_arg caller_env
validate_entrata_request_endpoint_method <- function(
    endpoint,
    method,
    arg_endpoint = rlang::caller_arg(endpoint),
    arg_method = rlang::caller_arg(method),
    call = rlang::caller_env()
) {

  valid_methods <- entrata_endpoint_methods_tbl |>
    dplyr::filter(endpoint == {{ endpoint }}) |>
    dplyr::pull("method")

  if (!method %in% valid_methods) {
    cli::cli_abort(
      c(
        "Invalid method for endpoint {.arg {arg_endpoint}}: {.field {method}}",
        "{.arg {arg_method}} is not a valid method for endpoint {.arg {arg_endpoint}}.",
        "i" = "Valid methods are for provided endpoint are: {.val {valid_methods}}"
      ),
      call = call
    )
  }

}

#' @rdname validate_entrata_request
#' @export
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom rlang caller_arg caller_env
validate_entrata_endpoint_method_params <- function(
    endpoint,
    method,
    method_params,
    arg_endpoint = rlang::caller_arg(endpoint),
    arg_method = rlang::caller_arg(method),
    arg_method_params = rlang::caller_arg(method_params),
    call = rlang::caller_env()
) {

  expected_params <- entrata_endpoint_method_parameters[[endpoint]][[method]]

  if (is.null(expected_params)) {
    cli::cli_alert_info(
      c(
        "No parameters are expected for the {.field {method}} method of the ",
        "{.field {endpoint}} endpoint."
      )
    )
    return(invisible())
  }

  for (param in names(method_params)) {

    if (!param %in% names(expected_params)) {
      cli::cli_alert_warning("Unexpected parameter provided: {.field {param}}")
      next
    }

    param_info <- expected_params[[param]]
    param_value <- method_params[[param]]

    if (param_info$required && is.null(param_value)) {
      cli::cli_abort(
        c(
          "Missing required parameter: {.field {param}}",
          "i" = "This parameter is required for the {.field {method}} method of the {.field {endpoint}} endpoint."
        ),
        call = call
      )
    }

    if (!is.null(param_value)) {
      validate_param_type(param, param_value, param_info$type)

      if (!is.null(param_info$multiple) && param_info$multiple &&
          length(param_value) > 1 && !is.vector(param_value)) {

        cli::cli_abort(
          c(
            "Invalid multiple parameter value: {.field {param}}",
            "i" = "The parameter {.field {param}} should be a vector for multiple values."
          ),
          call = call
        )

      }
    }
  }
}

#' @rdname validate_entrata_request
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr filter pull
#' @importFrom rlang caller_arg caller_env .data .env
validate_entrata_method_version <- function(
    endpoint,
    method,
    method_version,
    arg_endpoint = rlang::caller_arg(endpoint),
    arg_method = rlang::caller_arg(method),
    arg_method_version = rlang::caller_arg(method_version),
    call = rlang::caller_env()
) {

  valid_version <- entrata_endpoint_methods_tbl |>
    dplyr::filter(.data$endpoint == .env$endpoint,
                  .data$method == .env$method) |>
    dplyr::pull("method_version")

  if (method_version != valid_version) {
    cli::cli_abort(
      c(
        "Invalid method version for {.field {method}} method of the {.field {endpoint}} endpoint.",
        "i" = "The valid version is {.val {valid_version}}"
      ),
      call = call
    )
  }

}

# internal ----------------------------------------------------------------

#' Is Request
#' @keywords internal
#' @noRd
is_request <- function(x) { inherits(x, "httr2_request") }

#' Check Request
#' @keywords internal
#' @noRd
check_request <- function(
    req,
    arg = rlang::caller_arg(req),
    call = rlang::caller_env(),
    allow_null = FALSE
) {

  if (!missing(req)) {
    if (is_request(req)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(req)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    req,
    "an HTTP request object",
    allow_null = allow_null,
    arg = arg,
    call = call
  )

}
