
#  ------------------------------------------------------------------------
#
# Title : Shared
#    By : Jimmy Briggs
#  Date : 2024-12-23
#
#  ------------------------------------------------------------------------

# parameters --------------------------------------------------------------

#' Shared Parameters
#'
#' @name .shared-params
#' @keywords internal
#'
#' @description
#' These parameters are used in multiple functions throughout the package.
#' They are defined here to make it easier to update them in one place.
#'
#' To use these parameters in a function, simply include the following
#' in the function's documentation:
#'
#' ```
#' #' @inheritParams .shared-params
#' ```
#'
#' @param as_of_date Date to use as the reference point for deriving other dates from.
#'   Defaults to the current system date.
#'
#' @param conn A database connection object.
#' @param pool A database connection pool object.
#'
#' @param key Character string representing the configuration key to retrieve.
#'   Defaults to `NULL` which returns the full configuration list.
#' @param file A character string representing the path to the configuration file.
#' @param config A character string representing the configuration to use from the
#'   configuration file.
#' @param cfg List containing the configuration values or a string to the path
#'   of a configuration file. If `NULL`, the default configuration file path will be used.
#'
#' @param entrata_config Entrata API Configuration values as a list. The configuration
#'   values should include the following keys: `username`, `password`, and `base_url`
#'   at a minimum. If left `NULL`, will retrieve the configuration via the
#'   [get_entrata_config()] function.
#' @param db_config Database configuration values as a list. The configuration
#'   values should include the following keys: `host`, `port`, `dbname`, `user`,
#'   and `password` at a minimum. If left `NULL`, will retrieve the configuration
#'   via the [get_db_config()] function.
#' @param gmaps_config Google Maps API configuration values as a list. The configuration
#'   values should include the following keys: `api_key` at a minimum. If left `NULL`,
#'   will retrieve the configuration via the [get_gmaps_config()] function.
#'
#' @param request_id The unique identifier for the request.
#'   This is used to track the request and response.  If left `NULL`, a
#'   unique identifier will be generated via `as.integer(Sys.time())`.
#'
#' @param endpoint The Entrata API endpoint to send the request to.
#'   This must be a valid Entrata API endpoint path. If left `NULL`,
#'   no endpoint will be appended to the request's URL. See
#'   details for how to get the available Entrata API endpoints.
#' @param method_name The name of the Entrata API "method" (operation) to use
#'   in the request. Not to be confused with the `HTTP` request method
#'   (i.e. `GET`, `POST`, etc.), this method must be a method available for
#'   the specified `endpoint` argument, making it conditional on the supplied
#'   `endpoint`. If left `NULL`, a pre-defined default method will be used
#'   depending on the supplied `endpoint`. For example, the default method
#'   for the `/properties` endpoint is `getProperties`. See details for how
#'   to get the available Entrata API methods by their endpoint.
#' @param method_version The version of the Entrata API method to use in the
#'   request. This is conditional on the supplied `method_name` argument
#'   and must be a valid version for the given method name. If left `NULL`,
#'   a pre-defined default version will be used depending on the supplied
#'   `method_name`. Note that all Entrata API method versions are one of
#'   `"r1"`, `"r2"`, or `"r3"`.
#' @param method_params A named, potentially nested list of request method parameters
#'   to pass to the Entrata API. These parameters will be added to the request
#'   body's `method.params` object. Depending on the supplied `endpoint`,
#'   `method_name`, and `method_version`, the required parameters will vary
#'   and some will have required, optional, or default values. If left `NULL`,
#'   any required method parameters without default values will throw an error.
NULL


# rlang -------------------------------------------------------------------

##' @inheritParams rlang::args_error_context
##' @inheritParams rlang::args_dots_empty
##' @inheritParams rlang::args_data_masking

# request -----------------------------------------------------------------

#' `httr2` Request
#'
#' @name .shared-return-request
#' @keywords internal
#' @noRd
#'
#' @returns
#' A [httr2::request()] object.
NULL

# response ----------------------------------------------------------------

#' @name .shared-return-response
#' @keywords internal
#' @noRd
#'
#' @returns
#' A [httr2::response()] object.
NULL



