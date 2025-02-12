
#  ------------------------------------------------------------------------
#
# Title : Entrata Weekly (lease_execution_(applicant)) Report
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Weekly Lease Execution Report
#'
#' @description
#' This function retrieves the weekly lease execution report data from Entrata.
#'
#' This data is used in conjunction with the data retrieved via [entrata_pre_lease_report()]
#' to append data for the prior seven days (hence weekly) new leases, renewals,
#' and total by property.
#'
#' @param property_ids Vector of property IDs to include in the report. Defaults to all properties.
#' @param report_date Date to use for the report. Defaults to the current date.
#' @param ... Additional parameters to pass to the report request.
#' @param request_id Request ID for the report. Defaults to the current time stamp
#'   as an integer.
#' @param report_version Version of the report to request. Defaults to "1.8".
#' @param max_retries Maximum number of retries to attempt when performing the request to
#'   the `/queue` endpoint. Defaults to `15`.
#' @param entrata_config Entrata configuration object. Defaults to the
#'   global Entrata configuration object.
#'
#' @returns
#' A tibble containing the weekly lease execution report data.
#'
#' @seealso [entrata_pre_lease_report()], [entrata_reports()]
#'
#' @export
#'
#' @importFrom cli cli_alert_info cli_alert_danger
#' @importFrom dplyr select mutate rename across everything
#' @importFrom httr2 req_body_json req_perform resp_body_json req_progress
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom purrr pluck map_chr
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider replace_na
entrata_lease_execution_report <- function(
    property_ids = NULL,
    report_date = NULL,
    ...,
    request_id = NULL,
    report_version = NULL,
    max_retries = 15,
    entrata_config = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- get_entrata_config() }
  if (is.null(report_date)) { report_date <- Sys.Date() }
  if (is.null(request_id)) { request_id <- as.integer(Sys.time()) }
  if (is.null(report_version)) { report_version <- get_default_entrata_report_version("lease_execution_(applicant)") }
  if (is.null(property_ids)) { property_ids <- get_entrata_property_ids() }

  # derive daterange period
  weekly_period <- get_weekly_period() |> format_date_for_entrata()
  req_period <- list(
    daterange = list(
      "daterange-start" = weekly_period[[1]],
      "daterange-end" = weekly_period[[2]]
    ),
    "period_type" = "daterange"
  )

  # create the report request body
  req_body <- list(
    auth = list(type = "basic"),
    requestId = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "lease_execution_(applicant)",
        reportVersion = report_version, # get_latest_report_version("lease_execution_(applicant)"),
        filters = list(
          property_group_ids = property_ids,
          period = req_period,
          results_based_on = "activity",
          lease_type = c("1", "3"),
          summarize_by = "lease_type",
          group_by = "property",
          consolidate_by = "no_consolidation",
          arrange_by_property = "0",
          subtotals = "0"
        )
      )
    )
  )

  req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("reports") |>
    httr2::req_body_json(req_body)

  resp <- httr2::req_perform(req)

  queue_id <- httr2::resp_body_json(resp) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "Lease Execution Report data has been successfully queued for processing.",
      "The queue ID is: {.field {queue_id}}"
    )
  )

  # call queue endpoint with queue id
  queue_req <- entrata_request(entrata_config) |>
    entrata_req_endpoint("queue") |>
    httr2::req_body_json(
      list(
        auth = list(type = 'basic'),
        request_id = request_id,
        method = list(
          name = "getResponse",
          version = "r1",
          params = list(
            queueId = queue_id,
            serviceName = "getReportData"
          )
        )
      )
    ) |>
    # enable progress
    httr2::req_progress() |>
    # setup retry logic
    entrata_req_retry(max_tries = max_retries)

  # send queue request
  queue_resp <- tryCatch({
    httr2::req_perform(queue_req)
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to retrieve pre_lease report data from Entrata",
        "Error: {.error {e}}"
      )
    )
  })

  # check response
  entrata_resp_check_status(queue_resp)

  # parse response to get report data
  report_data <- httr2::resp_body_json(queue_resp) |>
    pluck("response", "result", "reportData")

  report_data |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select("property_name", "lease_type", "signed") |>
    tidyr::pivot_wider(names_from = lease_type, values_from = signed) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0))) |>
    dplyr::rename(weekly_new = `New Lease`, weekly_renewal = Renewal) |>
    dplyr::mutate(
      # get property id from named vector (names = property names)
      property_id = purrr::map_chr(property_name, ~ purrr::pluck(property_ids, .x)),
      property_id = as.integer(property_id),
      report_date = report_date,
      weekly_total = weekly_new + weekly_renewal
    ) |>
    dplyr::select(
      report_date,
      property_id,
      property_name,
      weekly_new,
      weekly_renewal,
      weekly_total
    )
}
