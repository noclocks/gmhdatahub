entrata_req_arcodes <- function(property_id = NULL, request_id = NULL, ...) {

  # property_id (is provided) can only be a single integer value for /arcodes
  if (!is.null(property_id)) {
    property_id <- as.integer(property_id)
    if (length(property_id) != 1) {
      cli::cli_abort("{.arg property_id} must be a single integer value.")
    }
  }

  # body params -------------------------------------------------------------
  method_params <- list(
    propertyId = property_id
  ) |>
    purrr::compact()

  # build request -----------------------------------------------------------
  req <- entrata_request() |>
    entrata_req_endpoint("arcodes") |>
    entrata_req_method("getArCodes") |>
    entrata_req_method_params(method_params) |>
    entrata_req_id(request_id)

  # perform request into response -------------------------------------------
  resp <- entrata_req_perform(req, ...)

  # parse response ----------------------------------------------------------
  entrata_resp_parse_arcodes(resp)

}

entrata_resp_parse_arcodes <- function(resp) {

  check_response(resp)

  resp_body <- httr2::resp_body_json(resp)
  arcodes <- purrr::pluck(resp_body, "response", "result", "arcodes", "arcode")

  purrr::map_dfr(
    arcodes,
    .parse_arcode_to_tibble
  )

}

entrata_req_validate_arcodes <- function(req) {

  check_request(req)

  schema <- get_request_schema("arcodes", "getArCodes")

  validation <- jsonvalidate::json_validate(
    json = httr2::req_body_json(req),
    schema = schema,
    engine = "ajv",
    verbose = TRUE,
    greedy = TRUE,
    error = FALSE
  )

  if (!validation) {
    cli::cli_abort("Request validation failed.")
  }

  return(invisible(req))

}

entrata_resp_validate_arcodes <- function(resp) {

  check_response(resp)

  resp_body <- httr2::resp_body_json(resp) |>
    jsonlite::toJSON(auto_unbox = TRUE)

  schema <- get_response_schema("arcodes", "getArCodes")

  validation <- jsonvalidate::json_validate(
    json = resp_body,
    schema = schema,
    engine = "ajv",
    verbose = TRUE,
    greedy = TRUE,
    error = FALSE
  )

  if (!validation) {
    cli::cli_abort("Response validation failed.")
  }

  return(invisible(resp))

}



.parse_arcode_to_tibble <- function(arcode) {
  arcode |>
    tibble::as_tibble() |>
    dplyr::select(
      id,
      code,
      name,
      code_type = codeType,
      charge_usage = chargeUsage,
      associated_ledger = associatedLedger,
      debit_gl_account_id = debitGlAccountId,
      credit_gl_account_id = creditGlAccountId,
      display_as = displayAs,
      is_disabled = isDisabled,
      is_entrata_disabled = isEntrataDisabled,
      is_taxable = isTaxable
    )
}
