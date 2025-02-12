
#  ------------------------------------------------------------------------
#
# Title : Entrata Request
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

# request -----------------------------------------------------------------

#' Entrata API HTTP Request
#'
#' @description
#' Create a generic HTTP request to the Entrata API. This function initializes
#' and modifies a base request to the Entrata API by calling [entrata_req_init()],
#' [entrata_req_id()], [entrata_req_endpoint()], and [entrata_req_body()]
#' as necessary to derive the final request object.
#'
#' This functions primary purpose is to combine the functionality behind the
#' [entrata_req_endpoint()] and [entrata_req_body()] functions into a single
#' function call allowing the end-developer to specify both the endpoint path
#' and its corresponding request body method details in a single function call.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' [httr2::request()] object
#'
#' @export
#'
#' @seealso [Entrata API Documentation](https://docs.entrata.com/api/v1/documentation/)
#' @seealso [httr2::request()]
#' @seealso [entrata_req_init()], [entrata_req_endpoint()], and [entrata_req_body()]
entrata_request <- function(
    entrata_config = get_entrata_config(),
    endpoint = NULL,
    method_name = NULL,
    method_version = NULL,
    method_params = list(),
    request_id = NULL
) {

  validate_entrata_config(entrata_config)

  entrata_req_init(entrata_config = entrata_config) |>
    entrata_req_endpoint(endpoint) |>
    entrata_req_body(method_name, method_version, method_params, request_id)

}

as_entrata_request <- function(req) {
  check_request(req)
  structure(req, class = c("entrata_request", class(req)))
}

#' Initialize Entrata API Request
#'
#' @description
#' Initialize a request to the Entrata API. By default this function sets up the
#' request with the following configured settings:
#'
#' - Sets the base URL of the Entrata API.
#' - Sets up basic authentication with the username and password.
#' - Sets the content type and accept headers to `application/json`.
#' - Sets the user agent.
#' - Sets up error handling for Entrata API specific errors.
#'
#' @param base_url The base URL of the Entrata API. If left `NULL`, will attempt
#'   to retrieve from `get_entrata_config("base_url")`.
#' @param username The username for the Entrata API. If left `NULL`, will attempt
#'   to retrieve from `get_entrata_config("username")`.
#' @param password The password for the Entrata API. If left `NULL`, will attempt
#'   to retrieve from `get_entrata_config("password")`.
#' @param user_agent The user agent for the request. If left `NULL`, will attempt
#'   to retrieve from `get_entrata_config("user_agent")`.
#' @inheritParams .shared-params
#'
#' @returns
#' [httr2::request()] object
#'
#' @export
#'
#' @importFrom httr2 request req_method req_headers req_auth_basic
entrata_req_init <- function(
    base_url = NULL,
    username = NULL,
    password = NULL,
    user_agent = NULL,
    entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)

  base_url <- base_url %||% entrata_config$base_url
  username <- username %||% entrata_config$username
  password <- password %||% entrata_config$password
  user_agent <- user_agent %||% entrata_config$user_agent %||% "gmhdatahub/0.0.1"

  httr2::request(base_url) |>
    httr2::req_method("POST") |>
    entrata_req_auth(username, password) |>
    entrata_req_headers() |>
    entrata_req_user_agent(user_agent) |>
    entrata_req_error() |>
    as_entrata_request()
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
#' @param username Username for authentication. If left `NULL`, will attempt
#'   to retrieve systematically.
#' @param password Password for authentication. If left `NULL`, will attempt
#'   to retrieve systematically.
#'
#' @return The modified request object with authentication headers.
#'
#' @export
#'
#' @importFrom httr2 req_auth_basic
entrata_req_auth <- function(req, username = NULL, password = NULL) {

  check_request(req)

  username <- username %||% getOption("entrata.username") %||%
    Sys.getenv("ENTRATA_USERNAME") %||% get_entrata_config("username")

  password <- password %||% getOption("entrata.password") %||%
    Sys.getenv("ENTRATA_PASSWORD") %||% get_entrata_config("password")

  if (is.null(username) || is.null(password)) {
    cli::cli_abort(
      "Entrata API username and password must be provided."
    )
  }

  httr2::req_auth_basic(req, username, password)

}

entrata_req_user_agent <- function(req, user_agent = NULL) {

  check_request(req)

  user_agent <- user_agent %||% getOption("entrata.user_agent") %||%
    Sys.getenv("ENTRATA_USER_AGENT") %||% get_entrata_config("user_agent") %||%
    "gmhcommunities/0.0.1"

  httr2::req_user_agent(req, user_agent)

}

entrata_req_endpoint <- function(req, endpoint = NULL) {
  check_request(req)
  req_endpoint <- get_request_endpoint(req)
  if (is.null(endpoint)) {
    return(req)
  } else {
    return(
      httr2::req_url_path_append(req, endpoint)
    )
  }
}

entrata_req_headers <- function(req, ...) {
  check_request(req)
  default_headers <- list(
    "Content-Type" = "application/json; charset=utf-8",
    "Accept" = "application/json"
  )
  headers <- list(...) |> modifyList(default_headers)
  httr2::req_headers(req, !!!headers, .redact = "Authorization")
}

entrata_req_body <- function(
    req,
    method_name = NULL,
    method_version = NULL,
    method_params = NULL,
    request_id = NULL
) {

  check_request(req)
  req_endpoint <- get_request_endpoint(req)

  # if (is.null(method_name)) method_name <- get_default_entrata_method(endpoint = req_endpoint)
  # if (is.null(method_version)) method_version <- get_default_entrata_method_version(endpoint = req_endpoint, method = method_name)
  # if (is.null(method_params)) method_params <- get_default_entrata_method_params(method_name)
  if (is.null(request_id)) request_id <- get_default_entrata_request_id()

  req_body <- list(
    auth = list(type = "basic"),
    requestId = request_id,
    method = list(
      name = method_name,
      version = method_version,
      params = method_params
    )
  ) |>
    purrr::compact()

  httr2::req_body_json(req, req_body)

}

entrata_req_error <- function(
  req,
  is_error = entrata_resp_is_error,
  body = entrata_resp_error_body
) {
  check_request(req)
  stopifnot(rlang::is_function(is_error), rlang::is_function(body))
  httr2::req_error(req, is_error = is_error, body = body)
}

entrata_req_retry <- function(
  req,
  max_tries = 20,
  max_seconds = 600,
  retry_on_failure = TRUE,
  is_transient = entrata_resp_is_transient,
  backoff = exponential_backoff
) {

  check_request(req)
  stopifnot(rlang::is_function(is_transient), rlang::is_function(backoff))

  req |>
    httr2::req_retry(
      max_tries = max_tries,
      max_seconds = max_seconds,
      retry_on_failure = retry_on_failure,
      is_transient = is_transient,
      backoff = backoff
    )
}

entrata_req_pagination <- function(
    req,
    page_number = 1,
    per_page = 500,
    include_pagination_links = FALSE
) {
  check_request(req)
  if (include_pagination_links) {
    req <- req |>
      httr2::req_headers(
        `X-Send-Pagination-Links` = 1
      )
  }
  if (!is.null(page_number)) {
    req <- req |>
      httr2::req_url_query(
        "page_no" = page_number
      )
  }
  if (!is.null(per_page)) {
    req <- req |>
      httr2::req_url_query(
        "per_page" = per_page
      )
  }
  return(req)
}

entrata_req_id <- function(req, request_id = NULL) {
  check_request(req)
  if (is.null(request_id)) request_id <- get_default_entrata_request_id()
  req_id <- get_request_id(req)
  if (!is.null(req_id)) {
    cli::cli_alert_warning(
      "Replacing existing request id {.field {req_id}} with new id {.field {request_id}}."
    )
  }
  req$body$data$requestId <- request_id
  return(req)
}

entrata_req_throttle <- function(req, rate, realm = NULL) {
  check_request(req)
  httr2::req_throttle(req, rate, realm)
}

entrata_req_parse <- function(req) {

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

  # endpoint, method, version, etc.
  entrata_endpoint <- get_request_endpoint(req)
  entrata_method <- get_request_method(req)
  entrata_method_name <- get_request_method_name(req)
  entrata_method_version <- get_request_method_version(req)
  entrata_method_params <- get_request_method_params(req)

  list(
    request = req,
    http_metadata = list(
      url = req_url,
      method = req_method,
      headers = req_headers,
      body = req_body,
      fields = req_fields,
      options = req_options,
      policies = req_policies
    ),
    entrata_metadata = list(
      endpoint = entrata_endpoint,
      method = entrata_method,
      method_name = entrata_method_name,
      method_version = entrata_method_version,
      method_params = entrata_method_params
    )
  )

}

entrata_req_log <- function(
    req,
    logger = entrata_logger()
) {

}

entrata_req_hash <- function(req) {
  check_request(req)
  endpoint <- get_request_endpoint(req)
  body <- req$body$data
  cache_key <- paste0(endpoint, digest::digest(body, algo = "md5"), sep = "_")
  return(cache_key)
}

entrata_req_cache <- function() {}

entrata_req_debug <- function() {}

entrata_req_timeout <- function() {}

entrata_req_validate <- function() {}

#' @export
entrata_req_perform <- function(
    req,
    path = NULL,
    debug = FALSE,
    cache = FALSE,
    logger = NULL,
    ...
) {
  check_request(req)
  config <- get_entrata_config()

  # Add retry logic
  req <- req |>
    entrata_req_retry(
      max_tries = config$max_tries,
      max_seconds = config$max_seconds
    ) |>
    # Add rate limiting
    entrata_req_throttle(
      rate = config$rate_limits$minute,
      realm = "minute"
    )

  if (debug) {
    return(httr2::req_dry_run(req))
  }

  tryCatch({
    resp <- httr2::req_perform(req)

    if (entrata_resp_is_error(resp)) {
      cli::cli_abort(
        entrata_resp_error_body(resp)
      )
    }

    as_entrata_response(
      resp,
      endpoint = get_request_endpoint(req),
      method = get_request_method_name(req)
    )
  }, error = function(e) {
    cli::cli_abort(
      c("Entrata API request failed",
        "i" = "Error: {conditionMessage(e)}"
      )
    )
  })
}

# utility -----------------------------------------------------------------

get_request_endpoint <- function(req) {
  check_request(req)
  url <- purrr::pluck(req, "url")
  if (is.null(url)) return(NULL)
  hold <- basename(url)
  if (hold == "v1") return(NULL)
  if (!(hold %in% entrata_endpoints_lst)) {
    cli::cli_alert_warning(
      "Request endpoint {.field {hold}} is not a valid Entrata API endpoint."
    )
    return(hold)
  }
  return(hold)
}

get_request_body <- function(req) {
  check_request(req)
  purrr::pluck(req, "body", "data")
}

get_request_method <- function(req) {
  check_request(req)
  purrr::pluck(req, "body", "data", "method")
}

get_request_method_name <- function(req) {
  check_request(req)
  purrr::pluck(get_request_method(req), "name")
}

get_request_method_version <- function(req) {
  check_request(req)
  purrr::pluck(get_request_method(req), "version")
}

get_request_method_params <- function(req) {
  check_request(req)
  purrr::pluck(get_request_method(req), "params")
}

get_request_id <- function(req) {
  check_request(req)
  purrr::pluck(req, "body", "data", "requestId")
}

exponential_backoff <- function(num_requests) {
  backoff <- 2^num_requests
  jitter <- runif(1, 0, 1)
  backoff <- backoff + jitter
  return(min(backoff, 60))
}


