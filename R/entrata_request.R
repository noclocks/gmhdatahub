
#  ------------------------------------------------------------------------
#
# Title : Base Entrata HTTP Request
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------

#' Entrata HTTP Request
#'
#' @description
#' This function prepares the base HTTP API request object for the
#' GMH Communities Entrata API.
#'
#' @details
#' This is the primary function for preparing HTTP requests to send to the
#' GMH Communities Entrata API. This function is used by other functions
#' that call the API to provide a robust and consistent way to prepare the
#' request object.
#'
#' The function handles authentication, headers, default request options,
#' validation, caching, logging, rate limits, throttling, retry logic, and
#' error handling.
#'
#' @param endpoint Character string specifying the Entrata API endpoint to
#'   send the HTTP request to. Must be one of the valid [entrata_endpoints()].
#'   If left `NULL` will default to the base Entrata API URL:
#'   `https://gmhcommunities.entrata.com/api/v1/`.
#' @param method Character string specifying the **Entrata** endpoint method
#'   (not the HTTP method) to use for the request. Must be one of the valid
#'   [entrata_methods()] for the provided `endpoint` value. If left `NULL`,
#'   a default method will be determined based on the `endpoint` value.
#' @param method_version Character string specifying the version of the
#'   method to use for the request. Must be one of the valid [entrata_method_versions()]
#'   based on the provided `method` (and `endpoint`) values. If left `NULL`,
#'   a default version will be determined based on the `method` and `endpoint`
#'   values (typically default version is `"r1"`).
#' @param method_params List of parameters to include in the request body's
#'   "method" object. This is where the actual API method parameters are
#'
#'
#'
#'


# perform -----------------------------------------------------------------



entrata_req_perform <- function(
    req,
    path = NULL,
    verbosity = getOption("entrata.verbosity", NULL),
    mock = getOption("entrata.mock", NULL),
    log = getOption("entrata.log", NULL),
    error_call = rlang::current_env(),
    ...
) {

  check_request(req)

  if (!is.null(log)) {
    entrata_req_log(req, log)
  }

  resp <- NULL

  tryCatch({
    resp <- httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "An error occurred while performing the Entrata API request.",
        "The request could not be completed.",
        "Error: {conditionMessage(e)}"
      )
    )
  })

  return(resp)
}
