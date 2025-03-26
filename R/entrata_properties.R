#  ------------------------------------------------------------------------
#
# Title : Entrata /properties Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Call the `/properties` Entrata API Endpoint
#'
#' @description
#' Calls the `/properties` endpoint from the Entrata API.
#'
#' Currently the following "methods" (i.e. operations) are supported:
#'
#' - `getProperties`
#' - `getFloorPlans`
#'
#' @param method_name The Entrata endpoint method name. Defaults to `"getProperties"`,
#'   but other methods can be used as needed.
#' @param method_params Named list of parameters to pass to the method object
#'   of the request body. Defaults to an empty list, but some methods may
#'   have required method parameters that must be provided.
#' @param request_id (Optional) A unique identifier for the request. Defaults to `NULL`
#'   which will use the current time as an integer for the request ID.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`.
#' @param verbose (Optional) Logical indicating if verbose output should be printed.
#'   Defaults to `FALSE`.
#' @param progress (Optional) Logical indicating if a progress indicator should be
#'   displayed. Defaults to `FALSE`.
#'
#' @returns A parsed response containing property related data depending on which
#'   endpoint method is used.
#'
#' @export
#'
#' @importFrom httr2 req_verbose req_progress req_perform
entrata_properties <- function(
    method_name = c("getProperties", "getFloorPlans", "getWebsites"),
    method_params = list(propertyIds = NULL),
    request_id = NULL,
    entrata_config = get_entrata_config(),
    verbose = FALSE,
    progress = FALSE
) {

  method_name <- match.arg(method_name)
  validate_entrata_method_name(method_name = method_name)
  method_version <- get_default_entrata_method_version("properties", method_name)
  validate_entrata_method_params(endpoint = "properties", method_name = method_name, method_version = method_version, method_params = method_params)
  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      method_name = method_name,
      method_version = method_version,
      method_params = method_params,
      request_id = request_id
    )

  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  if (progress) {
    req <- httr2::req_progress(req)
  }

  resp <- httr2::req_perform(req)

  parser <- switch(
    method_name,
    "getProperties" = entrata_resp_parse_properties,
    "getFloorPlans" = entrata_resp_parse_floorplans,
    "getWebsites" = entrata_resp_parse_property_websites
  )

  parser(resp)

}

# getProperties -----------------------------------------------------------

#' Entrata Properties: Get Properties
#'
#' @description
#' Calls the `getProperties` method from the `/properties` endpoint of the Entrata API.
#'
#' @param property_ids (Optional) Character vector of Entrata Property IDs.
#'   If left `NULL` (the default), will return all Entrata properties.
#'   Note that this argument will be merged into a single comma-separated, character
#'   string before being passed to the API request body.
#' @param property_lookup_code (Optional) Character vector of Entrata Property Lookup Codes.
#'   Not used in this package.
#' @param show_all_status (Optional) Logical indicating if all property statuses should be shown.
#'   Defaults to `TRUE`. Note that this argument will be coerced to an integer string before
#'   being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param request_id (Optional) A unique identifier for the request. If left as `NULL`,
#'   (the default), will set a request ID via `as.integer(Sys.time())`.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`. See [get_entrata_config()] for more information.
#'
#' @returns A parsed response containing property related datasets.
#'
#' @export
#'
#' @seealso [entrata_properties()]
#'
#' @importFrom rlang %||%
#' @importFrom httr2 req_perform
entrata_properties_getProperties <- function(
    property_ids = NULL,
    property_lookup_code = NULL,
    show_all_status = TRUE,
    request_id = NULL,
    entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)
  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      method_name = "getProperties",
      method_version = get_default_entrata_method_version("properties", "getProperties"),
      method_params = list(
        propertyIds = property_ids,
        propertyLookupCode = property_lookup_code,
        showAllStatus = as.integer(show_all_status)
      ),
      request_id = request_id
    )

  entrata_req_perform(req)

}


# getFloorPlans -----------------------------------------------------------

#' Entrata Properties: Get Floor Plans
#'
#' @description
#' Calls the `getFloorPlans` method from the `/properties` endpoint of the Entrata API.
#'
#' @param property_id (Required) The Entrata Property ID to retrieve the floor plans
#'   for. Must be a valid Entrata property ID.
#' @param floorplan_ids (Optional) Character or Integer vector of Entrata Floor Plan IDs.
#'   If left `NULL` (the default), will return all floor plans for the specified property.
#'   Note that this argument will be merged into a single comma-separated, character
#'   string before being passed to the API request body.
#' @param use_property_preferences (Optional) Logical indicating if property preferences
#'   should be used. Defaults to `TRUE`. Note that this argument will be coerced to an
#'   integer string before being passed to the API request body (i.e. TRUE -> `"1"`).
#' @param include_disabled_floorplans (Optional) Logical indicating if disabled floor plans
#'   should be included. Defaults to `FALSE`. Note that this argument will be coerced to an
#'   integer string before being passed to the API request body (i.e. FALSE -> `"0"`).
#' @param request_id (Optional) A unique identifier for the request. If left as `NULL`,
#'   (the default), will set a request ID via `as.integer(Sys.time())`.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`. See [get_entrata_config()] for more information.
#'
#' @returns A parsed response containing floor plan data for the specified property.
#'
#' @export
#'
#' @importFrom httr2 req_perform
#' @importFrom rlang %||%
entrata_properties_getFloorPlans <- function(
    property_id,
    floorplan_ids = NULL,
    use_property_preferences = NULL,
    include_disabled_floorplans = NULL,
    request_id = NULL,
    entrata_config = get_entrata_config()
) {

  validate_entrata_config(entrata_config)
  # validate_entrata_property_id(property_id)

  request_id <- request_id %||% get_default_entrata_request_id()


  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      method_name = "getFloorPlans",
      method_version = get_default_entrata_method_version("properties", "getFloorPlans"),
      method_params = list(
        propertyId = property_id,
        usePropertyPreferences = as.integer(use_property_preferences),
        includeDisabledFloorplans = as.integer(include_disabled_floorplans)
      ),
      request_id = request_id
    )

  resp <- httr2::req_perform(req)

  entrata_resp_parse_floorplans(resp)

}

entrata_req_floorplans <- function(property_id, request_id = NULL, entrata_config = get_entrata_config()) {

  validate_entrata_config(entrata_config)

  request_id <- request_id %||% as.integer(Sys.time())

  entrata_request(entrata_config) |>
    entrata_req_endpoint("properties") |>
    entrata_req_body(
      method_name = "getFloorPlans",
      method_version = get_default_entrata_method_version("properties", "getFloorPlans"),
      method_params = list(propertyId = property_id),
      request_id = request_id
    )

}

entrata_floorplans <- function(
  property_ids = NULL,
  entrata_config = get_entrata_config()
) {

  if (is.null(property_ids)) {
    property_ids <- get_entrata_property_ids(entrata_config)
  }

  reqs <- purrr::map(
    property_ids,
    entrata_req_floorplans,
    request_id = as.integer(Sys.time()),
    entrata_config = entrata_config
  )

  resps <- httr2::req_perform_parallel(reqs, on_error = "continue", progress = TRUE)
  # resp_successes <- resps |> httr2::resps_successes()
  # resp_failures <- resps |> httr2::resps_failures()
  # req_failures <- resp_failures |> httr2::resps_requests()

  resps |>
    purrr::map_dfr(entrata_resp_parse_floorplans)

}

get_entrata_property_ids <- function(entrata_config = get_entrata_config()) {

  req <- entrata_request(
    endpoint = "properties",
    method_name = "getProperties",
    entrata_config = entrata_config
  )

  resp <- httr2::req_perform(req)

  resp_content <- resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property")

  prop_names <- resp_content |>
    purrr::map_chr(
      ~ as.character(purrr::pluck(.x, "MarketingName"))
    )

  resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    purrr::map_chr(
      ~ as.character(purrr::pluck(.x, "PropertyID"))
    ) |>
    setNames(prop_names)

}

mem_get_entrata_property_ids <- memoise::memoise(get_entrata_property_ids)
