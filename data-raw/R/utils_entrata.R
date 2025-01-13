
get_entrata_config <- function() {
  config::get("entrata")
}

entrata_base_request <- function() {

  # get config
  entrata_config <- get_entrata_config()

  # base request
  httr2::request(entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(
      entrata_config$username,
      entrata_config$password
    ) |>
    httr2::req_headers(
      `Content-Type` = "application/json; charset=utf-8",
      `Accept` = "application/json"
    )

}

entrata_properties_request <- function() {
  # base request
  entrata_base_request() |>
    httr2::req_url_path_append("properties") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getProperties",
          version = "r1",
          params = list(NULL)
        )
      )
    )
}

entrata_req_perform <- function(req) {
  req |> httr2::req_perform()
}

entrata_req_perform_save <- function(req, path = "data-raw/cache/entrata/responses") {
  # Create directories if they don't exist
  fs::dir_create(path)
  request_dir <- fs::path(dirname(path), "requests")
  fs::dir_create(request_dir)

  # Generate file names
  endpoint <- basename(req$url)
  method_name <- req$body$data$method$name
  method_version <- req$body$data$method$version
  file_name <- glue::glue("{endpoint}.{method_name}.{method_version}")
  response_file <- fs::path(path, paste0(file_name, ".response.json"))
  response_file_qs <- fs::path(path, paste0(file_name, ".response.qs"))
  request_file <- fs::path(request_dir, paste0(file_name, ".request.json"))

  # Process request
  req_json <- jsonlite::toJSON(req$body$data, auto_unbox = TRUE, pretty = TRUE)
  perform <- TRUE

  if (fs::file_exists(request_file)) {
    existing_req <- jsonlite::read_json(request_file)
    if (identical(existing_req, req$body$data)) {
      perform <- FALSE
    }
  }

  if (perform) {
    jsonlite::write_json(req$body$data, request_file, auto_unbox = TRUE, pretty = TRUE)
    logger::log_info("Request saved to file: {request_file}")
  }

  # Handle response
  if (perform || !fs::file_exists(response_file_qs)) {
    tryCatch({
      resp <- req |> httr2::req_perform(path = response_file)
      qs2::qs_save(resp, response_file_qs)
      logger::log_info("Response saved to file: {response_file}")
      return(resp)
    }, error = function(e) {
      logger::log_error("Error performing request: {e$message}")
      stop(e)
    })
  } else {
    tryCatch({
      resp <- qs2::qs_read(response_file_qs)
      logger::log_info("Response read from file: {response_file}")
      return(resp)
    }, error = function(e) {
      logger::log_error("Error reading cached response: {e$message}")
      stop(e)
    })
  }
}

# entrata_req_perform_save <- function(req, path = "data-raw/cache/entrata/responses") {
#
#   browser()
#
#   # derive file name
#   endpoint <- basename(req$url)
#   method_name <- req$body$data$method$name
#   method_version <- req$body$data$method$version
#   file_name <- glue::glue("{endpoint}.{method_name}.{method_version}.response.json")
#   file_path <- file.path(path, file_name)
#
#   request_dir <- fs::path(dirname(path), "requests")
#   request_file <- file.path(request_dir, file_name) |>
#     stringr::str_replace(".response.json", ".request.json")
#
#   # save request if it doesn't exist
#   req_json <- req$body$data |> jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)
#   req_hash <- digest::digest(req_json, algo = "sha256")
#
#   perform <- TRUE
#
#   if (!file.exists(request_file)) {
#     jsonlite::write_json(req_json, request_file)
#     cli::cli_alert_info("Request saved to file: {.path {request_file}}")
#   } else {
#     req_file_hash <- digest::digest(request_file, algo = "sha256")
#     if (req_hash != req_file_hash) {
#       jsonlite::write_json(req_json, request_file)
#       cli::cli_alert_info("Request saved to file: {.path {request_file}}")
#     } else {
#       perform <- FALSE
#     }
#   }
#
#   # determine if need to perform request to get response or
#   # if response already exists and can be read from file
#   if (perform || !file.exists(file_path)) {
#     resp <- req |> httr2::req_perform(path = file_path)
#     return(resp)
#   } else {
#     resp <- jsonlite::read_json(file_path)
#     cli::cli_alert_info("Response read from file: {.path {file_path}}")
#     return(resp)
#   }
# }

entrata_properties_picklist_request <- function(property_ids = NULL) {

  if (is.null(property_ids)) {
    property_ids <- get_entrata_properties()$id
  }

  entrata_base_request() |>
    httr2::req_url_path_append("properties") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getPropertyPickLists",
          version = "r2",
          params = list(
            propertyId = stringr::str_c(property_ids, collapse = ",")
          )
        )
      )
    )

}

entrata_leases_picklist_request <- function() {
  entrata_base_request() |>
    httr2::req_url_path_append("leases") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getLeasePickList",
          version = "r1",
          params = list()
        )
      )
    )
}

get_entrata_properties <- function() {
  entrata_config <- get_entrata_config()
  resp_properties <- entrata_properties_request() |>
    entrata_req_perform()
  resp_properties_json <- resp_properties |> httr2::resp_body_json()
  resp_properties_json |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    purrr::map(function(x) {
      name <- purrr::pluck(x, "MarketingName")
      id <- purrr::pluck(x, "PropertyID")
      out <- list(name = name, id = id)
    }) |>
    purrr::map_dfr(~tibble::tibble(name = .x$name, id = .x$id))
}

get_entrata_properties <- memoise::memoise(get_entrata_properties)

assign_versions <- function(methods) {
  purrr::map(methods, ~ if (.x %in% r3_methods) {
    "r3"
  } else if (.x %in% r2_methods) {
    "r2"
  } else {
    "r1"
  })
}

get_entrata_arcodes <- function() {
  entrata_config <- get_entrata_config()
  resp_arcodes <- entrata_base_request() |>
    httr2::req_url_path_append("arcodes") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getArCodes",
          version = "r1",
          params = list()
        )
      )
    ) |>
    entrata_req_perform()
  resp_json <- resp_arcodes |> httr2::resp_body_json()
  resp_json |>
    purrr::pluck("response", "result", "arcodes", "arcode") |>
    dplyr::bind_rows() |>
    dplyr::select(
      code_id = "id",
      ar_code = "code",
      code_name = "name",
      code_type = "codeType",
      charge_usage = "chargeUsage",
      associated_ledger = "associatedLedger",
      debit_gl_account_id = "debitGlAccountId",
      credit_gl_account_id = "creditGlAccountId",
      display_as = "displayAs",
      is_disabled = "isDisabled",
      is_entrata_disabled = "isEntrataDisabled",
      is_taxable = "isTaxable"
    )

}

get_entrata_arcodes <- memoise::memoise(get_entrata_arcodes)

get_entrata_report_filters <- function(report_name) {

  entrata_config <- get_entrata_config()
  resp_report_list <- entrata_base_request() |>
    httr2::req_url_path_append("reports") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getReportList",
          version = "r2",
          params = list(
            reportName = report_name
          )
        )
      )
    ) |>
    entrata_req_perform()
  resp_report_info_json <- resp_report_info |> httr2::resp_body_json()
  resp_report_info_content <- resp_report_info_json |>
    purrr::pluck("response", "result", "reports", "report")

  report_name <- purrr::pluck(resp_report_info_content, 1, "name")
  report_description <- purrr::pluck(resp_report_info_content, 1, "description")
  report_filters <- purrr::pluck(resp_report_info_content, 1, "filters")

  list(
    report_name = report_name,
    report_description = report_description,
    report_filters = report_filters
  )

}

