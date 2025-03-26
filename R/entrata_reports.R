
#  ------------------------------------------------------------------------
#
# Title : Entrata /reports Endpoint
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

# main endpoint function:
# -------------------------
# entrata_reports

# endpoint method wrappers:
# -------------------------
# entrata_reports_getReportList
# entrata_reports_getReportInfo
# entrata_reports_getReportData
# entrata_reports_getDependentFilter

# individual report wrappers:
# -------------------------
# entrata_reports_pre_lease
# entrata_reports_lease_execution
# entrata_reports_box_score
# entrata_reports_weekly_performance
# entrata_reports_scheduled_charges_by_lease
# entrata_reports_income_statement
# entrata_reports_lease_activity
# entrata_reports_traffic_and_events
# entrata_reports_specials
# entrata_reports_market_rent_schedule
# entrata_reports_resident_data
# entrata_reports_buildings_and_units
# entrata_reports_receipts_by_charge_code


# parsers
# -------------------------
# parse_reports_list_response
# parse_reports_info_response
# parse_reports_data_response
# parse_reports_dependent_filter_response

# utils
# -------------------------
# get_latest_report_version
# get_report_info
# get_report_queue_id



# entrata_reports ---------------------------------------------------------


#' Entrata `/reports` Endpoint
#'
#' @name entrata_reports
#'
#' @family Entrata
#' @family Reports
#'
#' @description
#' Functions for interacting with the Entrata `/reports` endpoint and its
#' corresponding methods.
#'
#' - `entrata_reports()`: Call the Entrata `/reports` endpoint
#' - `entrata_reports_list()`: List all available reports via the `getReportsList` method.
#' - `entrata_reports_info()`: Get information about reports via the `getReportInfo` method.
#' - `entrata_reports_data()`: Get a `queueId` for a report via the `getReportData` method.
#'
#' plus,
#'
#' - `get_latest_report_version()`: Get the latest version of a specified report.
#'
#' @details
#' The Entrata `/reports` endpoint provides methods for interacting with
#' the Entrata reporting system and all of the various pre-built reports
#' specific to the GMH Communities API tenant.
#'
#' The `/reports` endpoint provides the following methods:
#'
#' - `getDependentFilter`: Retrieves dependent report filters for a report.
#' - `getReportData`: Retrieves a `queueId` for the corresponding report data.
#' - `getReportInfo`: Retrieves information about a report.
#' - `getReportsList`: Retrieves a list of available reports.
#'
#' @param request_id (Optional) A unique identifier for the request. Defaults to `NULL`
#'   which will use the current time as an integer for the request ID.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`.
#' @param method_name The Entrata endpoint method name. Defaults to `"getReportList"`,
#'   but other methods can be used as needed.
#' @param method_params Named list of parameters to pass to the method object
#'   of the request body. Defaults to an empty list, but some methods may
#'   have required method parameters that must be provided.
#' @param verbose (Optional) Logical indicating if verbose output should be printed.
#'   Defaults to `FALSE`.
#' @param progress (Optional) Logical indicating if a progress indicator should be
#'   displayed. Defaults to `FALSE`.
#'
#' @returns A parsed response containing report related data depending on which
#'   endpoint method is used.
#'
#' @export
#'
#' @seealso [entrata_request()], [entrata_response()], [entrata_errors()]
entrata_reports <- function(
    method_name = c("getReportList", "getReportInfo", "getReportData", "getDependentFilter"),
    method_params = list(NULL),
    request_id = NULL,
    entrata_config = get_entrata_config(),
    verbose = FALSE,
    progress = FALSE
) {

  method_name <- rlang::arg_match(method_name)
  validate_entrata_method_name(method_name)
  method_version <- get_default_entrata_method_version("reports", method_name)
  validate_entrata_method_params(endpoint = "reports", method_name, method_params)
  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("reports") |>
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

  return(req)

  # resp <- httr2::req_perform(req)
  #
  # parser <- switch(
  #   method_name,
  #   "getReportList" = parse_reports_list_response,
  #   "getReportInfo" = parse_report_info_response,
  #   "getReportData" = parse_report_data_response,
  #   "getDependentFilter" = parse_dependent_filter_response
  # )
  #
  # parser(resp)

}

# reports list ------------------------------------------------------------

#' Entrata Reports List
#'
#' @description
#' List all available Entrata API reports via the `getReportsList` method for
#' the `/reports` endpoint.
#'
#' @param request_id (Optional) A unique identifier for the request. Defaults to `NULL`
#'   which will use the current time as an integer for the request ID.
#' @param entrata_config (Optional) An Entrata Configuration list. Defaults to
#'   `get_entrata_config()`.
#' @param latest_only Logical. If `TRUE`, only the latest version of each report
#'   will be returned. Defaults to `TRUE`.
#'
#' @returns A tibble containing the list of available reports.
#'
#' @export
entrata_reports_list <- function(
    request_id = NULL,
    entrata_config = get_entrata_config(),
    latest_only = TRUE
) {

  request_id <- request_id %||% as.integer(Sys.time())

  req <- entrata_reports(
    method_name = "getReportList",
    request_id = request_id,
    method_params = NULL
  )

  resp <- httr2::req_perform(req)

  resp_body <- entrata_resp_body(resp)

  reports_specs <- list(
    "getReportList" = list(
      main = tibblify::tspec_df(
        tibblify::tib_int("id"),
        tibblify::tib_chr("reportName"),
        tibblify::tib_chr("systemName"),
        tibblify::tib_row(
          "reportVersions",
          tibblify::tib_df(
            "reportVersion",
            tibblify::tib_chr("version"),
            tibblify::tib_lgl("isLatest"),
            tibblify::tib_chr("titleAddendum", required = FALSE),
            tibblify::tib_chr("expiryDate", required = FALSE),
          )
        )
      ),
      versions = tibblify::tspec_row(
        tibblify::tib_chr("version"),
        tibblify::tib_lgl("isLatest"),
        tibblify::tib_chr("titleAddendum", required = FALSE),
        tibblify::tib_chr("expiryDate", required = FALSE)
      )
    )
  )

  resp_data <- resp_body |>
    purrr::pluck("response", "result", "reports", "report") |>
    tibblify::tibblify(reports_specs$getReportList$main)

  report_names <- resp_data$reportName
  report_versions <- resp_data$reportVersions$reportVersion |>
    setNames(report_names) |>
    purrr::list_rbind(names_to = "report_name")

  hold <- dplyr::select(
    resp_data,
    "report_id" = "id",
    "report_name" = "reportName",
    "system_name" = "systemName",
    -c("reportVersions")
  ) |>
    dplyr::left_join(
      report_versions,
      by = "report_name"
    ) |>
    dplyr::select(
      "report_id",
      "report_name",
      "system_name",
      "report_version" = "version",
      "is_latest" = "isLatest",
      -c("titleAddendum", "expiryDate")
    )

  if (!latest_only) {
    return(hold)
  }

  hold |>
    dplyr::filter(
      .data$is_latest == TRUE
    ) |>
    dplyr::select(
      -c("is_latest")
    )

}



# report info -------------------------------------------------------------

entrata_reports_info <- function(
    report_name,
    report_version = "latest",
    entrata_config = get_entrata_config()
) {

  validate_entrata_report_name(report_name)

  if (report_version == "latest") {
    report_version <- get_latest_report_version(report_name)
  }

  req <- entrata_request(entrata_config = entrata_config) |>
    entrata_req_endpoint("reports") |>
    entrata_req_body(
      method_name = "getReportInfo",
      method_version = get_default_entrata_method_version("reports", "getReportInfo"),
      method_params = list(
        report = report_name,
        version = report_version
      )
    )

  resp <- httr2::req_perform(req)
  resp_body <- entrata_resp_body(resp)

}


# report version ----------------------------------------------------------

#' Get Latest Report Version
#'
#' @param report The report name. Must be a valid report name.
#'
#' @returns The latest version of the specified report.
#'
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang .data .env
get_latest_report_version <- function(report_name) {

  validate_entrata_report_name(report_name)

  entrata_reports_list(latest_only = TRUE) |>
    dplyr::filter(
      .data$report_name == {{ report_name }}
    ) |>
    dplyr::pull("report_version")

}

get_report_versions <- function(report_name) {

  validate_entrata_report_name(report_name)

  entrata_reports_list(latest_only = FALSE) |>
    dplyr::filter(
      .data$report_name == {{ report_name }}
    )

}

# utils -------------------------------------------------------------------

