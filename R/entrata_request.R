
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
#' @param method_name Character string specifying the **Entrata** endpoint method
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
#'   specified. The parameters must be in the correct format for the method
#'   being called. If left `NULL`, the method will be called with no parameters.
#' @param request_id An integer or string to use for the request ID.
#'   By default this will first check the global option
#'   `entrata.default_request_id` and then fall back to `15L`.
#' @param config A list of configuration options for the Entrata API.
#'   This should include the base URL, username, and password and
#'   can also include the user agent, default request ID, retry logic,
#'   rate limit handling, error handling, caching options, logging options,
#'   and more.
#' @param ... Additional arguments to pass to the request object.
#'
#' @return An httr2 request object with the Entrata API request configuration.
#'
#' @export
#'
#' @importFrom httr2 request req_method req_auth_basic req_headers req_user_agent req_body_json
#' @importFrom config get
#' @importFrom cli cli_alert_danger
#'
#'
#'
#'


entrata_request <- function(
  endpoint,
  body_method = list(
    name = NULL,
    version = NULL,
    params = list(NULL)
  ),
  options = list(
    request_id = getOption("entrata.default_request_id", 15L),
    user_agent = getOption("entrata.user_agent", "entrata-r/0.1.0"),
    verbosity = getOption("entrata.verbosity", "info"),
    debug = getOption("entrata.debug", FALSE),
    log = getOption("entrata.log", NULL),
    retry = getOption("entrata.retry", 3L),
    timeout = getOption("entrata.timeout", 30L),
    progress = getOption("entrata.progress", FALSE),
    cache = getOption("entrata.cache", FALSE),
    cache_path = getOption("entrata.cache_path", ":memory:")
  ),
  config = get_entrata_config(),
  ...
) {

  # ensure config


  base_url <- config$base_url
  user_agent <- config$user_agent
  username <- config$username
  password <- config$password

  req <- httr2::request(base_url) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(
      username = username,
      password = password
    ) |>
    httr2::req_headers(
      `Content-Type` = "application/json; charset=utf-8",
      `Accept` = "application/json",
    ) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_body_json(
      list(
        auth = list(
          type = "bsic"
        ),
        requestId = request_id,
        method = list(
          name = method_name,
          version = method_version,
          params = method_params
        )
      )
    )

  return(req)


}


# request modifiers -------------------------------------------------------

#' Entrata Request Modifiers
#'
#' @name entrata_request_modifiers
#'
#' @description
#' These functions are used to modify the Entrata request object before
#' sending the request to the API. These functions are used to set the
#' endpoint path, Entrata internal endpoint method (name, version, and params),
#' additional headers, authentication, request body, error handling, logging,
#' and more.
NULL

#' Set Basic Authentication for Entrata Request
#'
#' @description
#' Adds basic authentication (username and password) to the Entrata request.
#'
#' @param req An httr2 request object
#' @param username Username for authentication
#' @param password Password for authentication
#'
#' @return The modified request object with authentication headers.
#'
#' @export
#'
#' @importFrom httr2 req_auth_basic
entrata_req_auth <- function(req, username = NULL, password = NULL) {

  check_request(req)

  username <- username %||% getOption("entrata.username") %||%
    Sys.getenv("ENTRATA_USERNAME") %||% config::get("entrata")$username

  password <- password %||% getOption("entrata.password") %||%
    Sys.getenv("ENTRATA_PASSWORD") %||% config::get("entrata")$password

  req |> httr2::req_auth_basic(username, password)
}

#' Set Request Body for Entrata API Request
#'
#' @description
#' Configures the request body with specific values for the request.
#'
#' @param req An httr2 request object
#' @param id Request ID
#' @param method The endpoint method name
#' @param version The endpoint method version
#' @param params List of parameters for the method
#'
#' @return Modified request object with the configured body
#'
#' @export
entrata_req_body <- function(req, id = NULL, method = NULL, version = "r1", params = list()) {

  check_request(req)

  req_body <- list(
    auth = list(
      type = "basic"
    ),
    requestId = id %||% getOption("entrata.default_request_id", 15L),
    method = list(
      name = method,
      version = version,
      params = params
    )
  ) |>
    purrr::compact()

  req |>
    httr2::req_body_json(req_body)

}

#' Set Endpoint Path for Entrata Request
#'
#' @description
#' Appends an endpoint path to the request URL.
#'
#' @param req An httr2 request object
#' @param endpoint The API endpoint to set
#'
#' @return Modified request object with the endpoint URL path appended
#'
#' @export
#'
#' @importFrom httr2 req_url_path_append
entrata_req_endpoint <- function(req, endpoint) {

  check_request(req)

  pre_endpoint <- req_get_endpoint(req)

  if (is.null(pre_endpoint) || pre_endpoint == "") {
    out <- req |>
      httr2::req_url_path_append(endpoint)
  } else {
    out <- req
  }

  return(out)

}

#' Set Error Handling for Entrata Request
#'
#' @description Configures error handling for the request, using custom error functions.
#' @param req An httr2 request object
#' @param is_error Function to check if response contains an error
#' @param error_body Function to parse error body in case of an error
#' @return Modified request with error handling
#' @export
#' @importFrom httr2 req_error
entrata_req_error <- function(req, is_error = entrata_resp_is_error, error_body = entrata_resp_body) {
  req |> httr2::req_error(is_error = is_error, body = error_body)
}

#' Log Entrata Request Details
#'
#' @description Logs request information according to the configured logger.
#' @param req An httr2 request object
#' @param logger Optional logger to use
#' @return The original request object
#' @export
#' @importFrom logger log_info
entrata_req_log <- function(req, logger = entrata_req_logger) {
  log_info(logger, "Request ID: {req$body$requestId}")
  log_info(logger, "Endpoint: {req$url}")
  log_info(logger, "Method: {req$method}")
  invisible(req)
}

#' Perform Entrata API Request
#'
#' @description Executes a prepared Entrata request with custom options.
#' @param req An httr2 request object
#' @param cache Logical indicating if response should be cached
#' @param cache_path Path to cache the request
#' @param save Logical indicating if request should be saved
#' @param save_path Path to save request data
#' @param verbosity Verbosity level of output
#' @param debug Logical for debugging request
#' @param logger Logger object
#' @param mock Mock request response if needed
#' @param error_call Call environment for error reporting
#' @param ... Additional arguments passed to req_perform
#' @return The response object
#' @export
#' @importFrom httr2 req_perform req_dry_run
entrata_req_perform <- function(
    req, cache = FALSE, cache_path = ":memory:", save = FALSE, save_path = NULL,
    verbosity = NULL, debug = FALSE, logger = NULL, mock = NULL,
    error_call = rlang::caller_env(), ...
) {
  # Caching, saving, and debugging logic
  if (debug) req <- httr2::req_dry_run(req)

  response <- httr2::req_perform(req, ...)

  # Additional post-processing, like logging and error handling, can be added here
  entrata_req_log(req, logger = logger)

  response
}

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



req_get_endpoint <- function(req) {
  check_request(req)
  req_url <- purrr::pluck(req, "url")
  gsub(paste0("^", "https://gmhcommunities.entrata.com/api/v1/"), "", req_url)
}
