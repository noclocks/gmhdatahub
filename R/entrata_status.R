
#  ------------------------------------------------------------------------
#
# Title : Entrata Status
#    By : Jimmy Briggs
#  Date : 2025-03-06
#
#  ------------------------------------------------------------------------

#' Entrata Status
#'
#' @description
#' Check the status of the Entrata API by calling the `/status` endpoint's `getStatus`
#' method.
#'
#' @param request_id The request ID. If left `NULL`, a unique identifier will be
#'   generated via `as.integer(Sys.time())`.
#' @param entrata_config Entrata API Configuration values as a list. The configuration
#'   values should include the following values: `base_url`, `username`, and `password`.
#'   Defaults to [get_entrata_config()].
#'
#' @returns
#' A list containing the response from the Entrata API.
#'
#' @export
entrata_status <- function(request_id = NULL, entrata_config = get_entrata_config()) {

  if (is.null(request_id)) request_id <- as.integer(Sys.time())

  endpoint <- "status"
  method_name <- get_default_entrata_method(endpoint)
  method_version <- get_default_entrata_method_version(endpoint, method_name)

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("status") |>
    entrata_req_body(
      method_name = method_name,
      method_version = method_version,
      method_params = NULL,
      request_id = request_id
    )

  resp <- entrata_req_perform(req)
  entrata_resp_check_status(resp)

  entrata_resp_body(resp)

}
