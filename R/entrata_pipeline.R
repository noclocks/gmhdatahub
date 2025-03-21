
#  ------------------------------------------------------------------------
#
# Title : Entrata API Data Pipeline
#    By : Jimmy Briggs
#  Date : 2025-03-03
#
#  ------------------------------------------------------------------------

# pre-lease response data processing ------------------------------------------------------------------------------

#' Process Pre-Lease Summary Data
#'
#' @description
#' This function processes the Pre-Lease Summary Data returned from the `pre_lease` Entrata API report summary
#' dataset. It performs data type conversion, column renaming, and reordering.
#'
#' @param summary_data The original Pre-Lease Summary Data.
#' @param report_date The report date. Default is the current system date.
#'
#' @returns
#' A tibble containing the processed Pre-Lease Summary Data.
#'
#' @export
#'
#' @importFrom dplyr arrange select mutate across coalesce
#' @importFrom tidyselect any_of ends_with starts_with
process_pre_lease_summary_data <- function(summary_data, report_date = Sys.Date()) {

  int_cols <- c(
    tidyselect::all_of(c("property_id", "units", "variance")),
    tidyselect::ends_with("_count"),
    tidyselect::ends_with("_count_prior"),
    tidyselect::starts_with("number_")
  )

  num_cols <- c(
    tidyselect::starts_with("avg_"),
    tidyselect::ends_with("_percent"),
    tidyselect::ends_with("_percent_prior"),
    tidyselect::ends_with("_total"),
    tidyselect::ends_with("_rent"),
    tidyselect::ends_with("_rent_total")
  )

  summary_data |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      dplyr::across(int_cols, function(n) dplyr::coalesce(as.integer(n), 0L)),
      dplyr::across(num_cols, function(n) dplyr::coalesce(as.numeric(n), 0.00))
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      tidyselect::any_of(c("unit_type", "floorplan_name", "space_option")),
      "total_unit_count" = "units",
      "excluded_unit_count",
      "rentable_unit_count",
      "occupied_unit_count" = "occupied_count",
      "available_unit_count" = "available_count",
      tidyselect::starts_with("avg_"),
      tidyselect::starts_with("started_"),
      tidyselect::starts_with("partially_completed_"),
      tidyselect::starts_with("completed_"),
      tidyselect::starts_with("approved_"),
      tidyselect::starts_with("preleased_"),
      "yoy_variance" = "variance",
      "total_scheduled_rent" = "scheduled_rent_total"
    ) |>
    dplyr::arrange(.data$property_name)

}

#' Process Pre-Lease Details Data
#'
#' @description
#' This function processes the Pre-Lease Details Data returned from the `pre_lease` Entrata API report details
#' dataset. It performs data type conversion, column renaming, and reordering.
#'
#' @param details_data The original Pre-Lease Details Data.
#' @param report_date The report date. Default is the current system date.
#'
#' @returns
#' A tibble containing the processed Pre-Lease Details Data.
#'
#' @export
#'
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr arrange select mutate distinct across coalesce desc
#' @importFrom janitor get_dupes
#' @importFrom lubridate mdy
#' @importFrom tidyselect all_of any_of starts_with ends_with everything
process_pre_lease_details_data <- function(details_data, report_date = Sys.Date()) {

  date_cols <- function() {
    tidyselect::all_of(
      c(
        "lease_start",
        "lease_end",
        "lease_started_on",
        "lease_partially_completed_on",
        "lease_completed_on",
        "lease_approved_on",
        "move_in_date"
      )
    )
  }

  int_cols <- function() {
    c(
      tidyselect::any_of(c("property_id", "sqft", "resident_id", "lease_term")),
      tidyselect::starts_with("number_"),
      tidyselect::ends_with("_count"),
      tidyselect::ends_with("_count_prior"),
      tidyselect::ends_with("_variance")
    )
  }

  num_cols <- function() {
    c(
      tidyselect::starts_with("deposit_"),
      tidyselect::ends_with("_rent"),
      tidyselect::ends_with("_rate"),
      tidyselect::ends_with("_total"),
      tidyselect::ends_with("_charges")
    )
  }

  # check for duplicates
  dups <- janitor::get_dupes(details_data, tidyselect::everything())
  if (nrow(dups) > 0) {
    cli::cli_alert_warning("Detected {.field {nrow(dups)}} duplicate records in the Pre-Lease Details Data!")
  }

  details_data |>
    dplyr::distinct() |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      dplyr::across(date_cols(), lubridate::mdy),
      dplyr::across(int_cols(), function(n) dplyr::coalesce(as.integer(n), 0L)),
      dplyr::across(num_cols(), function(n) dplyr::coalesce(as.numeric(n), 0.00))
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      tidyselect::any_of(c("bldg_unit", "unit_type", "unit_status", "floorplan_name", "bldg")),
      "sqft",
      tidyselect::starts_with("number_"),
      "resident_id",
      "resident_name" = "resident",
      "resident_email" = "email",
      "resident_phone" = "phone_number",
      "resident_gender" = "gender",
      "student_id_number",
      "lease_id" = "lease_id_display",
      "lease_status",
      "lease_sub_status",
      "lease_occupancy_type",
      "lease_term_name",
      "lease_term_month" = "lease_term",
      "space_option",
      "space_option_preferred",
      "leasing_agent",
      "lease_start_date" = "lease_start",
      "lease_end_date" = "lease_end",
      "lease_started_on_date" = "lease_started_on",
      "lease_partially_completed_on_date" = "lease_partially_completed_on",
      "lease_completed_on_date" = "lease_completed_on",
      "lease_approved_on_date" = "lease_approved_on",
      "move_in_date",
      tidyselect::any_of(c("ledger_name", "charge_code")),
      "deposit_charged",
      "deposit_held",
      "market_rent",
      "budgeted_rent",
      "advertised_rate",
      "scheduled_rent",
      "actual_charges",
      "scheduled_rent_total"
    ) |>
    dplyr::arrange(
      .data$property_name,
      dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
      .data$unit_type,
      .data$bldg_unit
    )

}

#' Prepare Pre-Lease Summary Data
#'
#' @description
#' This function prepares the Pre-Lease Summary Data for further analysis and visualization. It calculates various
#' derived metrics and KPIs, such as occupancy rates, pre-leased percentages, year-over-year variances, and velocity
#' rates.
#'
#' @param summary_data The processed Pre-Lease Summary Data.
#'
#' @returns
#' A tibble containing the prepared Pre-Lease Summary Data.
#'
#' @export
#'
#' @importFrom dplyr pull mutate transmute across coalesce
#' @importFrom purrr pluck
#' @importFrom tidyselect any_of where
prepare_pre_lease_summary_data <- function(summary_data) {

  rept_date <- summary_data |> dplyr::pull("report_date") |> purrr::pluck(1)
  weeks_left_to_lease <- get_weeks_left_to_lease(rept_date)

  summary_data |>
    dplyr::transmute(
      report_date = .data$report_date,
      property_id = .data$property_id,
      property_name = .data$property_name,
      dplyr::across(tidyselect::any_of(c("unit_type", "floorplan_name")), ~.x, .names = "{.col}"),
      total_beds = .data$available_unit_count,
      current_occupied = .data$occupied_unit_count,
      current_occupancy = dplyr::coalesce(.data$occupied_unit_count / .data$total_beds, 0),
      current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
      current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
      current_total_leases = .data$current_total_new + .data$current_total_renewals,
      current_preleased_percent = .data$current_total_leases / .data$total_beds,
      prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
      prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
      prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
      prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
      yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
      yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
      beds_left = .data$total_beds - .data$current_total_leases,
      vel_90 = .data$beds_left * .90 / .env$weeks_left_to_lease,
      vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
      vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~dplyr::coalesce(.x, 0)
      )
    )

}

#' Prepare Pre-Lease Summary Data by Unit
#'
#' @description
#' This function prepares the Pre-Lease Summary Data by Unit for further analysis and visualization. It calculates
#' various derived metrics and KPIs, such as occupancy rates, pre-leased percentages, year-over-year variances, and
#' projected availability.
#'
#' @param summary_data The processed Pre-Lease Summary Data.
#'
#' @returns
#' A tibble containing the prepared Pre-Lease Summary Data by Unit.
#'
#' @importFrom dplyr mutate transmute across coalesce
#' @importFrom tidyselect any_of where
prepare_pre_lease_summary_data_by_unit <- function(summary_data) {

  summary_data |>
    dplyr::transmute(
      report_date = .data$report_date,
      property_id = .data$property_id,
      property_name = .data$property_name,
      dplyr::across(
        tidyselect::any_of(c("unit_type", "floorplan_name")),
        ~.x,
        .names = "{.col}"
      ),
      excluded_units = .data$excluded_unit_count,
      rentable_units = .data$rentable_unit_count,
      occupied_units = .data$occupied_count,
      available_units = .data$available_count,
      total_beds = .data$available_count,
      current_occupied = .data$occupied_count,
      current_occupancy = dplyr::coalesce(.data$occupied_count / .data$total_beds, 0),
      current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
      current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
      current_total_leases = .data$current_total_new + .data$current_total_renewals,
      current_preleased_percent = .data$current_total_leases / .data$total_beds,
      prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
      prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
      prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
      prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
      yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
      yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
      projected_availability = .data$total_beds - .data$current_total_leases
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~dplyr::coalesce(.x, 0)
      )
    )

}

#' Summarize Pre-Lease Details Data by Unit
#'
#' @description
#' This function summarizes the Pre-Lease Details Data by Unit and merges it with the Pre-Lease Summary Data.
#'
#' @param details_data The processed Pre-Lease Details Data.
#' @param summary_data The prepared Pre-Lease Summary Data by Unit.
#'
#' @returns
#' A tibble containing the summarized Pre-Lease Details Data by Unit.
#'
#' @export
#'
#' @importFrom dplyr ungroup mutate summarize group_by across coalesce arrange select left_join
#' @importFrom tidyselect starts_with where
summarize_pre_lease_details_data_by_unit <- function(details_data, summary_data) {

  details_grouped <- details_data |>
    dplyr::group_by(
      .data$report_date,
      .data$property_id,
      .data$property_name,
      .data$unit_type
    ) |>
    dplyr::summarize(
      avg_market_rent = mean(.data$market_rent, na.rm = TRUE),
      avg_budgeted_rent = mean(.data$budgeted_rent, na.rm = TRUE),
      avg_advertised_rate = mean(.data$advertised_rate, na.rm = TRUE),
      avg_scheduled_rent = mean(.data$scheduled_rent, na.rm = TRUE),
      avg_actual_charges = mean(.data$actual_charges, na.rm = TRUE),
      avg_scheduled_charges = mean(.data$scheduled_rent_total, na.rm = TRUE),
      total_scheduled_charges = sum(.data$scheduled_rent_total, na.rm = TRUE),
      total_sqft = sum(.data$sqft, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("avg_"),
        ~dplyr::coalesce(.x, 0)
      ),
      dplyr::across(
        tidyselect::starts_with("total_"),
        ~dplyr::coalesce(.x, 0)
      )
    ) |>
    dplyr::ungroup()

  summary_data |>
    dplyr::left_join(
      details_grouped,
      by = c("report_date", "property_id", "property_name", "unit_type")
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~dplyr::coalesce(.x, 0)
      )
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      "unit_type",
      "excluded_units",
      "rentable_units",
      "occupied_units",
      "available_units",
      "total_beds",
      "avg_market_rent",
      "avg_budgeted_rent",
      "avg_advertised_rate",
      "avg_actual_charges",
      "avg_scheduled_charges",
      "total_scheduled_charges",
      "total_sqft",
      tidyselect::starts_with("current_"),
      tidyselect::starts_with("prior_"),
      tidyselect::starts_with("yoy_"),
      "projected_availability"
    ) |>
    dplyr::arrange(
      .data$property_name,
      .data$unit_type
    )

}

get_entrata_pre_lease_report_property_data <- function(
  request_id = NULL,
  ...
) {

  entrata_config <- get_entrata_config()
  report_date <- Sys.Date()
  report_version <- "3.2"
  property_ids <- memoise::memoise(get_entrata_property_ids)()
  period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
  period <- list(date = period_date, period_type = "date")
  request_id <- request_id %||% as.integer(Sys.time())

  req_filter_params <- list(
    property_group_ids = c(as.character(unlist(unname(property_ids)))),
    period = period,
    summarize_by = "property",
    group_by = "property", # "do_not_group"
    consider_pre_leased_on = "332",
    charge_code_detail = 1L,
    space_options = "do_not_show",
    additional_units_shown = "available",
    combine_unit_spaces_with_same_lease = 0L,
    consolidate_by = "no_consolidation",
    arrange_by_property = 0L,
    subtotals = list("summary", "details"),
    yoy = 1L
  )

  req_body <- list(
    auth = list(type = 'basic'),
    request_id = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "pre_lease",
        reportVersion = report_version,
        filters = req_filter_params
      )
    )
  )

  req <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("reports") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(req_body)

  resp <- httr2::req_perform(req)

  queue_id <- resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "queueId", 1)

  cli::cli_alert_info("Queue ID: {.field {stringr::str_trunc(queue_id, 15)}}")

  queue_req <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("queue") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = request_id,
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
    httr2::req_progress() |>
    httr2::req_retry(
      max_tries = 20,
      max_seconds = 600,
      retry_on_failure = TRUE,
      is_transient = entrata_resp_is_transient,
      backoff = exponential_backoff
    )

  queue_resp <- httr2::req_perform(queue_req)

  resp_data <- queue_resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "reportData")

  summary_data_original <- resp_data |>
    purrr::pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  summary_data_processed <- summary_data_original |>
    process_pre_lease_summary_data()

  summary_data_working <- summary_data_processed |>
    transform_pre_lease_summary_data()

  details_data_original <- resp_data |>
    purrr::pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  details_data_processed <- details_data_original |>
    process_pre_lease_details_data()

  params_lst <- list(
    request_id = request_id,
    report_date = report_date,
    report_name = "pre_lease",
    report_version = report_version,
    report_queue_id = queue_id,
    property_ids = "ALL",
    period_date = period_date,
    summarize_by = "Property",
    group_by = "None",
    consider_pre_leased_on = "Lease Partially Completed (332)",
    charge_code_detail = "TRUE",
    space_options = "Do Not Show",
    additional_units_shown = "Available",
    combine_unit_spaces_with_same_lease = "FALSE",
    consolidate_by = "None",
    arrange_by_property = "FALSE",
    subtotals = "Summary and Details",
    yoy = "TRUE"
  )

  params_json <- params_lst |> jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
  initial_req_json <- jsonlite::toJSON(req_body, pretty = TRUE, auto_unbox = TRUE)
  initial_resp_json <- jsonlite::toJSON(httr2::resp_body_json(resp), pretty = TRUE, auto_unbox = TRUE)
  queue_req_json <- jsonlite::toJSON(queue_req$body$data, pretty = TRUE, auto_unbox = TRUE)
  queue_resp_json <- jsonlite::toJSON(httr2::resp_body_json(queue_resp), pretty = TRUE, auto_unbox = TRUE)

  list(
    summary_original = summary_data_original,
    summary_processed = summary_data_processed,
    summary_working = summary_data_working,
    details_original = details_data_original,
    details_proceessed = details_data_processed,
    parameters = params_lst,
    metadata = list(
      report_params = params_json,
      report_request = initial_req_json,
      report_response = initial_resp_json,
      queue_id = queue_id,
      queue_request = queue_req_json,
      queue_response = queue_resp_json
    )
  )

}

get_entrata_pre_lease_report_unit_data <- function(
    request_id = NULL,
    ...
) {

  entrata_config <- get_entrata_config()
  report_date <- Sys.Date()
  report_version <- "3.2"
  property_ids <- memoise::memoise(get_entrata_property_ids)()
  period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
  period <- list(date = period_date, period_type = "date")
  request_id <- request_id %||% as.integer(Sys.time())

  req_filter_params <- list(
    property_group_ids = c(as.character(unlist(unname(property_ids)))),
    period = period,
    summarize_by = "unit_type",
    group_by = "unit_type",
    consider_pre_leased_on = "332",
    charge_code_detail = 1L,
    space_options = "do_not_show",
    additional_units_shown = "available",
    combine_unit_spaces_with_same_lease = 0L,
    consolidate_by = "no_consolidation",
    arrange_by_property = 0L,
    subtotals = list("summary", "details"),
    yoy = 1L
  )

  req_body <- list(
    auth = list(type = 'basic'),
    request_id = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "pre_lease",
        reportVersion = report_version,
        filters = req_filter_params
      )
    )
  )

  req <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("reports") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(req_body)

  resp <- httr2::req_perform(req)

  queue_id <- resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "queueId", 1)

  cli::cli_alert_info("Queue ID: {.field {stringr::str_trunc(queue_id, 15)}}")

  queue_req <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("queue") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = request_id,
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
    httr2::req_progress() |>
    httr2::req_retry(
      max_tries = 20,
      max_seconds = 600,
      retry_on_failure = TRUE,
      is_transient = entrata_resp_is_transient,
      backoff = exponential_backoff
    )

  queue_resp <- httr2::req_perform(queue_req)

  resp_data <- queue_resp |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "reportData")

  summary_data_original <- resp_data |>
    purrr::pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    process_pre_lease_summary_data()

  details_data_original <- resp_data |>
    purrr::pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    process_pre_lease_details_data()

  summary_data_working <- summary_data_original |>
    prepare_pre_lease_summary_data_by_unit()

  ##################################

  details_data_working <- summarize_pre_lease_details_data_by_unit(
    details_data_original,
    summary_data_working
  )

  params_lst <- list(
    request_id = request_id,
    report_date = report_date,
    report_name = "pre_lease",
    report_version = report_version,
    report_queue_id = queue_id,
    property_ids = "ALL",
    period_date = period_date,
    summarize_by = "Unit Type",
    group_by = "Unit Type",
    consider_pre_leased_on = "Lease Partially Completed (332)",
    charge_code_detail = "TRUE",
    space_options = "Do Not Show",
    additional_units_shown = "Available",
    combine_unit_spaces_with_same_lease = "FALSE",
    consolidate_by = "None",
    arrange_by_property = "FALSE",
    subtotals = "Summary and Details",
    yoy = "TRUE"
  )

  params_json <- params_lst |> jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)

  initial_req_json <- jsonlite::toJSON(req_body, pretty = TRUE, auto_unbox = TRUE)
  initial_resp_json <- jsonlite::toJSON(httr2::resp_body_json(resp), pretty = TRUE, auto_unbox = TRUE)

  queue_req_json <- jsonlite::toJSON(queue_req$body$data, pretty = TRUE, auto_unbox = TRUE)
  queue_resp_json <- jsonlite::toJSON(httr2::resp_body_json(queue_resp), pretty = TRUE, auto_unbox = TRUE)

  list(
    summary_original = summary_data_original,
    summary_working = summary_data_working,
    details_original = details_data_original,
    details_working = details_data_working,
    parameters = params_lst,
    metadata = list(
      report_params = params_json,
      report_request = initial_req_json,
      report_response = initial_resp_json,
      queue_id = queue_id,
      queue_request = queue_req_json,
      queue_response = queue_resp_json
    )
  )

}

get_entrata_weekly_pre_lease_report_data <- function(
  request_id = NULL,
  ...
) {

  request_id <- request_id %||% as.integer(Sys.time())

  entrata_lease_execution_report(
    request_id = request_id,
    max_retries = 20
  )

}

db_update_entrata_pre_lease_data_by_property <- function(
    pool,
    data_lst
) {

  check_db_conn(pool)

  summary_data <- purrr::pluck(data_lst, "summary_original")
  report_params <- purrr::pluck(data_lst, "parameters")

  pool::poolWithTransaction(pool, function(conn) {

    tryCatch({
      dbx::dbxUpsert(
        conn,
        table = DBI::SQL("entrata.pre_lease_summary_by_property"),
        records = summary_data,
        where_cols = c("report_date", "property_id"),
        skip_existing = FALSE
      )
      cli::cli_alert_success("Pre-Lease Report by Property updated successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error updating Pre-Lease Report by Property: {.error {e}}")
    })

    tryCatch({
      dbx::dbxInsert(
        conn,
        table = DBI::SQL("entrata.pre_lease_report_parameters"),
        records = report_params
      )
      cli::cli_alert_success("Pre-Lease Report Parameters inserted successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error inserting Pre-Lease Report Parameters: {.error {e}}")
    })

  })

  return(invisible(NULL))

}

db_update_entrata_pre_lease_data_by_unit <- function(
    pool,
    data_lst
) {

  check_db_conn(pool)

  summary_data <- purrr::pluck(data_lst, "summary_original")
  details_data <- purrr::pluck(data_lst, "details_original")
  property_details <- purrr::pluck(data_lst, "details_property_summary_working")
  report_params <- purrr::pluck(data_lst, "parameters")

  pool::poolWithTransaction(pool, function(conn) {

    tryCatch({
      dbx::dbxUpsert(
        conn,
        table = DBI::SQL("entrata.pre_lease_summary_by_unit"),
        records = summary_data,
        where_cols = c("report_date", "property_id", "unit_type"),
        skip_existing = FALSE
      )
      cli::cli_alert_success("Pre-Lease Report by Unit updated successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error updating Pre-Lease Report by Unit: {.error {e}}")
    })

    tryCatch({
      pool::dbWriteTable(
        pool,
        DBI::SQL("entrata.pre_lease_report_details"),
        details_data,
        append = TRUE
      )
      cli::cli_alert_success("Pre-Lease Report Details by Unit updated successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error updating Pre-Lease Report Details by Unit: {.error {e}}")
    })

    tryCatch({
      dbx::dbxUpsert(
        conn,
        table = DBI::SQL("entrata.pre_lease_property_details_by_unit"),
        records = property_details,
        where_cols = c("report_date", "property_id", "unit_type"),
        skip_existing = FALSE
      )
      cli::cli_alert_success("Pre-Lease Report Property Details by Unit updated successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error updating Pre-Lease Report Property Details by Unit: {.error {e}}")
    })

    tryCatch({
      dbx::dbxInsert(
        conn,
        table = DBI::SQL("entrata.pre_lease_report_parameters"),
        records = report_params
      )
      cli::cli_alert_success("Pre-Lease Report Parameters inserted successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error inserting Pre-Lease Report Parameters: {.error {e}}")
    })

  })

  return(invisible(NULL))

}

db_update_entrata_weekly_pre_lease_data <- function(
    pool,
    data_lst
) {

  check_db_conn(pool)

  weekly_data <- purrr::pluck(data_lst, "report_data")

  pool::poolWithTransaction(pool, function(conn) {

    tryCatch({
      dbx::dbxUpsert(
        conn,
        table = DBI::SQL("entrata.pre_lease_weekly"),
        records = weekly_data |>
          dplyr::select(-property_name, -weekly_total),
        where_cols = c("report_date", "property_id"),
        skip_existing = FALSE
      )
      cli::cli_alert_success("Weekly Leasing Data updated successfully!")
    }, error = function(e) {
      cli::cli_alert_danger("Error updating Weekly Leasing Data: {.error {e}}")
    })

  })

  return(invisible(NULL))

}


############


#' #  ------------------------------------------------------------------------
#' #
#' # Title : Entrata Pre-Lease Report
#' #    By : Jimmy Briggs
#' #  Date : 2024-11-24
#' #
#' #  ------------------------------------------------------------------------
#'
#' #' Entrata Pre-Lease Report
#' #'
#' #' @description
#' #' Retrieves the summary and details tables for the `pre_lease` report from the
#' #' Entrata API. The function dynamically constructs the API request and parses
#' #' the response to return clean summary and details tables.
#' #'
#' #' @param property_ids A vector of property IDs to include in the report. Defaults
#' #'   to all properties.
#' #' @param report_date The date to use for the report. Defaults to the current date.
#' #'   This date will be used to derive the target date for the pre-lease season, which
#' #'   is always `9/1` of the current year or the next year, depending on the current
#' #'   date's month.
#' #' @param summarize_by A character value representing the `summarize_by` report filter
#' #'   parameter for the pre_lease report. Defaults to `"property"`. Other options are
#' #'   `"unit_type"`, `"floorplan_name"`, or `"do_not_summarize"`.
#' #' @param group_by A character value representing the `group_by` report filter parameter
#' #'   for the pre_lease report. Defaults to `"do_not_group"`. Other options are `"unit_type"`,
#' #'   `"floorplan_name"`, or `"lease_term"`, or `"do_not_group"`.
#' #' @param consider_pre_leased_on A numeric value representing the `consider_pre_leased_on`
#' #'   report filter parameter for the pre_lease report. Defaults to `32` which represents
#' #'   `"Lease:Partially Completed"`. Other options are `33`, `34`, `41`, `42`, `43`, and `44`.
#' #' @param ... Named parameters to pass as additional pre_lease report filter parameters.
#' #'   Must be valid parameters for the pre_lease report.
#' #' @param request_id A unique identifier for the request. Defaults to the current
#' #'   timestamp.
#' #' @param max_retries The maximum number of retries to attempt when performing the request to
#' #'   the `/queue` endpoint. Defaults to `10`.
#' #' @param entrata_config The Entrata configuration object. Defaults to the global
#' #'   Entrata configuration object.
#' #'
#' #' @returns
#' #' A list containing:
#' #' - `summary` - A tibble representing the summary table for the pre_lease report.
#' #' - `details` - A tibble representing the details table for the pre_lease report.
#' #' - `parameters` - A list of the parameters used in the request.
#' #'
#' #' @export
#' entrata_pre_lease_report <- function(
#'     property_ids = NULL,
#'     report_date = Sys.Date(),
#'     summarize_by = c("unit_type", "property", "floorplan_name", "do_not_summarize"),
#'     group_by = c("unit_type", "floorplan_name", "lease_term", "do_not_group"),
#'     consider_pre_leased_on = "332",
#'     charge_code_detail = 0,
#'     space_options = "do_not_show",
#'     additional_units_shown = "available",
#'     combine_unit_spaces_with_same_lease = 0,
#'     consolidate_by = "no_consolidation",
#'     arrange_by_property = 0,
#'     subtotals = list("summary", "details"),
#'     yoy = 1,
#'     lease_occupancy_types = NULL,
#'     report_version = c("3.2", "3.3"),
#'     request_id = NULL,
#'     max_retries = 10,
#'     entrata_config = NULL
#' ) {
#'
#'   if (is.null(entrata_config)) { entrata_config <- get_entrata_config() }
#'   if (is.null(report_date)) { report_date <- Sys.Date() }
#'   if (is.null(request_id)) { request_id <- as.integer(Sys.time()) }
#'   if (is.null(property_ids)) { property_ids <- get_entrata_property_ids() }
#'
#'   # validation
#'   group_by <- rlang::arg_match(group_by, multiple = FALSE)
#'   summarize_by <- rlang::arg_match(summarize_by, multiple = FALSE)
#'   report_version <- rlang::arg_match(report_version, multiple = FALSE)
#'   charge_code_detail <- as.integer(charge_code_detail) %||% 1
#'   space_options <- space_options %||% "do_not_show"
#'   additional_units_shown <- additional_units_shown %||% "available"
#'   combine_unit_spaces_with_same_lease <- combine_unit_spaces_with_same_lease %||% 0
#'   consolidate_by <- consolidate_by %||% "no_consolidation"
#'   arrange_by_property <- arrange_by_property %||% 0
#'   subtotals <- subtotals %||% list("summary", "details")
#'   yoy <- yoy %||% 1
#'   period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
#'   period <- list(
#'     date = period_date,
#'     period_type = "date"
#'   )
#'
#'   # derive request body report filter parameters
#'   report_filter_params <- list(
#'     property_group_ids = c(as.character(unlist(unname(property_ids)))),
#'     period = period,
#'     summarize_by = summarize_by,
#'     group_by = group_by,
#'     consider_pre_leased_on = as.character(consider_pre_leased_on),
#'     charge_code_detail = charge_code_detail,
#'     space_options = space_options,
#'     additional_units_shown = additional_units_shown,
#'     combine_unit_spaces_with_same_lease = combine_unit_spaces_with_same_lease,
#'     consolidate_by = consolidate_by,
#'     arrange_by_property = arrange_by_property,
#'     subtotals = subtotals,
#'     yoy = yoy
#'   )
#'
#'   # create the report request body
#'   req_body <- list(
#'     auth = list(type = 'basic'),
#'     request_id = request_id,
#'     method = list(
#'       name = "getReportData",
#'       version = "r3",
#'       params = list(
#'         reportName = "pre_lease",
#'         reportVersion = report_version,
#'         filters = report_filter_params
#'       )
#'     )
#'   )
#'
#'   # build request
#'   req <- httr2::request(entrata_config$base_url) |>
#'     httr2::req_url_path_append("reports") |>
#'     httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
#'     httr2::req_method("POST") |>
#'     httr2::req_headers(
#'       "Content-Type" = "application/json",
#'       "Accept" = "application/json"
#'     ) |>
#'     httr2::req_body_json(req_body)
#'
#'   # send request
#'   resp <- httr2::req_perform(req)
#'
#'   # check response
#'   entrata_resp_check_status(resp)
#'
#'   # parse response to get queue id
#'   queue_id <- httr2::resp_body_json(resp) |>
#'     pluck("response", "result", "queueId", 1)
#'
#'   cli::cli_alert_success(
#'     c(
#'       "Pre-Lease Report Request Submitted\n",
#'       "Queue ID: {.field {queue_id}}"
#'     )
#'   )
#'
#'   # call queue endpoint with queue id
#'   queue_req <- httr2::request(entrata_config$base_url) |>
#'     httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
#'     httr2::req_url_path_append("queue") |>
#'     httr2::req_body_json(
#'       list(
#'         auth = list(type = 'basic'),
#'         request_id = request_id,
#'         method = list(
#'           name = "getResponse",
#'           version = "r1",
#'           params = list(
#'             queueId = queue_id,
#'             serviceName = "getReportData"
#'           )
#'         )
#'       )
#'     ) |>
#'     # enable progress
#'     httr2::req_progress() |>
#'     # setup retry logic
#'     httr2::req_retry(
#'       max_tries = 20,
#'       max_seconds = 600,
#'       retry_on_failure = TRUE,
#'       is_transient = entrata_resp_is_transient,
#'       backoff = exponential_backoff
#'     )
#'
#'   # send queue request
#'   queue_resp <- httr2::req_perform(queue_req)
#'
#'   # check response
#'   entrata_resp_check_status(queue_resp)
#'
#'   # parse response to get report data
#'   report_data <- httr2::resp_body_json(queue_resp) |>
#'     pluck("response", "result", "reportData")
#'
#'   # summary & details
#'   summary_data <- report_data |>
#'     pluck("summary") |>
#'     dplyr::bind_rows() |>
#'     tibble::as_tibble() |>
#'     dplyr::mutate(
#'       report_date = as.Date(.env$report_date),
#'       property_id = as.integer(.data$property_id),
#'       avg_sqft = as.numeric(.data$avg_sqft),
#'       avg_advertised_rate = as.numeric(.data$avg_advertised_rate),
#'       units = as.integer(.data$units),
#'       excluded_unit_count = as.integer(.data$excluded_unit_count),
#'       rentable_unit_count = as.integer(.data$rentable_unit_count),
#'       avg_scheduled_rent = as.numeric(.data$avg_scheduled_rent),
#'       occupied_count = as.integer(.data$occupied_count),
#'       variance = as.numeric(.data$variance),
#'       available_count = as.integer(available_count),
#'       scheduled_rent_total = as.numeric(scheduled_rent_total),
#'       dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
#'     )
#'
#'   if (summarize_by == "property") {
#'     summary_data <- summary_data |>
#'       dplyr::select(
#'         "report_date",
#'         "property_id",
#'         "property_name",
#'         "avg_sqft",
#'         "avg_advertised_rate",
#'         "total_unit_count" = "units",
#'         "excluded_unit_count",
#'         "rentable_unit_count",
#'         "occupied_count",
#'         "available_count",
#'         "total_scheduled_rent" = "scheduled_rent_total",
#'         "yoy_variance" = "variance",
#'         tidyselect::starts_with("avg_"),
#'         tidyselect::starts_with("started_"),
#'         tidyselect::starts_with("partially_completed"),
#'         tidyselect::starts_with("completed_"),
#'         tidyselect::starts_with("approved_"),
#'         tidyselect::starts_with("preleased_")
#'       ) |>
#'       dplyr::arrange(
#'         .data$property_name
#'       )
#'   } else if (summarize_by == "unit_type") {
#'     summary_data <- summary_data |>
#'       dplyr::select(
#'         "report_date",
#'         "property_id",
#'         "property_name",
#'         "unit_type",
#'         "avg_sqft",
#'         "avg_advertised_rate",
#'         "total_unit_count" = "units",
#'         "excluded_unit_count",
#'         "rentable_unit_count",
#'         "occupied_count",
#'         "available_count",
#'         "total_scheduled_rent" = "scheduled_rent_total",
#'         "yoy_variance" = "variance",
#'         tidyselect::starts_with("avg_"),
#'         tidyselect::starts_with("started_"),
#'         tidyselect::starts_with("partially_completed"),
#'         tidyselect::starts_with("completed_"),
#'         tidyselect::starts_with("approved_"),
#'         tidyselect::starts_with("preleased_")
#'       ) |>
#'       dplyr::arrange(
#'         .data$property_name,
#'         dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
#'         .data$unit_type
#'       )
#'   } else {
#'     summary_data <- summary_data |>
#'       dplyr::select(
#'         "report_date",
#'         "property_id",
#'         "property_name",
#'         tidyselect::any_of(c("unit_type", "floorplan_name")),
#'         "avg_sqft",
#'         "avg_advertised_rate",
#'         "total_unit_count" = "units",
#'         "excluded_unit_count",
#'         "rentable_unit_count",
#'         "occupied_count",
#'         "available_count",
#'         "total_scheduled_rent" = "scheduled_rent_total",
#'         "yoy_variance" = "variance",
#'         tidyselect::starts_with("avg_"),
#'         tidyselect::starts_with("started_"),
#'         tidyselect::starts_with("partially_completed"),
#'         tidyselect::starts_with("completed_"),
#'         tidyselect::starts_with("approved_"),
#'         tidyselect::starts_with("preleased_")
#'       ) |>
#'       dplyr::arrange(
#'         .data$property_name
#'       )
#'   }
#'
#'   details_data <- report_data |>
#'     pluck("details") |>
#'     dplyr::bind_rows() |>
#'     tibble::as_tibble() |>
#'     dplyr::mutate(
#'       report_date = as.Date(.env$report_date),
#'       property_id = as.integer(.data$property_id),
#'       sqft = as.numeric(sqft),
#'       resident_id = as.integer(resident_id),
#'       dplyr::across(
#'         tidyselect::all_of(
#'           c(
#'             "lease_start",
#'             "lease_end",
#'             "lease_started_on",
#'             "lease_partially_completed_on",
#'             "lease_completed_on",
#'             "lease_approved_on",
#'             "move_in_date"
#'           )
#'         ),
#'         lubridate::mdy
#'       )
#'     ) |>
#'     dplyr::mutate(
#'       dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
#'     ) |>
#'     dplyr::select(
#'       "report_date",
#'       "property_id",
#'       "property_name",
#'       "bldg_unit",
#'       "unit_type",
#'       "unit_status",
#'       "floorplan_name",
#'       "sqft",
#'       "resident_name" = "resident",
#'       "resident_id",
#'       "resident_email" = "email",
#'       "resident_phone" = "phone_number",
#'       "resident_gender" = "gender",
#'       "lease_id" = "lease_id_display",
#'       "lease_status",
#'       "lease_sub_status",
#'       "lease_occupancy_type",
#'       "lease_term_name",
#'       "lease_term_month" = "lease_term",
#'       "space_option_preferred",
#'       "space_option",
#'       "lease_start_date" = "lease_start",
#'       "lease_end_date" = "lease_end",
#'       "lease_started_on_date" = "lease_started_on",
#'       "lease_partially_completed_on_date" = "lease_partially_completed_on",
#'       "lease_completed_on_date" = "lease_completed_on",
#'       "lease_approved_on_date" = "lease_approved_on",
#'       "move_in_date",
#'       "leasing_agent",
#'       "deposit_charged",
#'       "deposit_held",
#'       "market_rent",
#'       "budgeted_rent",
#'       "advertised_rate",
#'       "scheduled_rent",
#'       "actual_charges",
#'       "scheduled_rent_total"
#'     ) |>
#'     dplyr::arrange(
#'       .data$property_name,
#'       dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
#'       .data$unit_type
#'     )
#'
#'   # return
#'   list(
#'     summary = summary_data,
#'     details = details_data,
#'     parameters = list(
#'       report_filter_params = report_filter_params,
#'       request_id = request_id,
#'       queue_id = queue_id,
#'       report_date = report_date,
#'       reports_req = req,
#'       reports_resp = resp,
#'       queue_req = queue_req,
#'       queue_resp = queue_resp
#'     )
#'   )
#'
#' }
#'
#'
#'
#'
#'
#' entrata_pre_lease_report_property_details <- function(summary_data, details_data) {
#'
#'   if (summarize_by != "unit_type") {
#'     details_data_summary_by_property <- NULL
#'   } else {
#'     details_data_summary_by_property <- summary_data_tidy |>
#'       dplyr::select(
#'         "report_date",
#'         "property_id",
#'         "property_name",
#'         "unit_type",
#'         "excluded_units" = "excluded_unit_count",
#'         "rentable_units" = "rentable_unit_count",
#'         "occupied_units" = "occupied_count",
#'         "available_units" = "available_count",
#'         # "avg_scheduled_charges" = "avg_scheduled_rent",
#'         "current_preleased_new_count" = "preleased_new_count",
#'         "prior_preleased_new_count" = "preleased_new_count_prior",
#'         "current_preleased_renewal_count" = "preleased_renewal_count",
#'         "prior_preleased_renewal_count" = "preleased_renewal_count_prior",
#'         "current_preleased_total_count" = "preleased_count",
#'         "prior_preleased_total_count" = "preleased_count_prior",
#'         "current_preleased_percent" = "preleased_percent",
#'         "prior_preleased_percent" = "preleased_percent_prior",
#'         "yoy_variance"
#'       ) |>
#'       dplyr::left_join(
#'         details_data_tidy |>
#'           dplyr::group_by(
#'             report_date,
#'             property_id,
#'             property_name,
#'             unit_type
#'           ) |>
#'           dplyr::summarize(
#'             avg_market_rent = mean(.data$market_rent, na.rm = TRUE),
#'             avg_budgeted_rent = mean(.data$budgeted_rent, na.rm = TRUE),
#'             avg_advertised_rate = mean(.data$advertised_rate, na.rm = TRUE),
#'             avg_scheduled_rent = mean(.data$scheduled_rent, na.rm = TRUE),
#'             avg_actual_charges = mean(.data$actual_charges, na.rm = TRUE),
#'             avg_scheduled_charges = mean(.data$scheduled_rent_total, na.rm = TRUE)
#'           ) |>
#'           dplyr::ungroup(),
#'         by = c("report_date", "property_id", "property_name", "unit_type")
#'       ) |>
#'       dplyr::select(
#'         "report_date",
#'         "property_id",
#'         "property_name",
#'         "unit_type",
#'         "excluded_units",
#'         "rentable_units",
#'         "occupied_units",
#'         "available_units",
#'         "avg_market_rent",
#'         "avg_budgeted_rent",
#'         "avg_advertised_rate",
#'         "avg_scheduled_rent",
#'         "avg_actual_charges",
#'         "avg_scheduled_charges",
#'         "current_preleased_new_count",
#'         "prior_preleased_new_count",
#'         "current_preleased_renewal_count",
#'         "prior_preleased_renewal_count",
#'         "current_preleased_total_count",
#'         "prior_preleased_total_count",
#'         "current_preleased_percent",
#'         "prior_preleased_percent",
#'         "yoy_variance"
#'       ) |>
#'       dplyr::mutate(
#'         dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
#'       ) |>
#'       dplyr::arrange(
#'         .data$property_name,
#'         dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
#'         .data$unit_type
#'       )
#'   }
#' }
#'
#' entrata_pre_lease_report_summary <- function(...) {
#'
#'   hold <- entrata_pre_lease_report(summarize_by = "property", ...)
#'
#'   report_date <- hold$parameters$report_date
#'
#'   weeks_left_to_lease <- get_weeks_left_to_lease(report_date)
#'
#'   hold$summary |>
#'     dplyr::transmute(
#'       report_date = .env$report_date,
#'       property_id = .data$property_id,
#'       property_name = .data$property_name,
#'       total_beds = .data$available_count,
#'       current_occupied = .data$occupied_count,
#'       current_occupancy = .data$occupied_count / .data$total_beds,
#'       current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
#'       current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
#'       current_total_leases = .data$current_total_new + .data$current_total_renewals,
#'       current_preleased_percent = .data$current_total_leases / .data$total_beds,
#'       prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
#'       prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
#'       prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
#'       prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
#'       yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
#'       yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
#'       beds_left = .data$total_beds - .data$current_total_leases,
#'       vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
#'       vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
#'       vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
#'     )
#' }
#'
#' get_entrata_pre_lease_summary <- function(pool = NULL) {
#'
#'   if (is.null(pool)) pool <- db_connect()
#'   on.exit(pool::poolClose(pool))
#'
#'   report_data <- entrata_pre_lease_report()
#'   report_date <- report_data$parameters$report_date
#'
#'   report_summary_data <- purrr::pluck(report_data, "summary")
#'
#'   weeks_left_to_lease <- get_weeks_left_to_lease(report_date)
#'
#'   model_beds <- dplyr::tbl(pool, I("gmh.model_beds")) |>
#'     dplyr::select(-gmh_property_id, -updated_at) |>
#'     dplyr::rename(property_id = entrata_property_id) |>
#'     dplyr::select(-property_name) |>
#'     dplyr::collect()
#'
#'   inv_partners <- dplyr::tbl(pool, I("gmh.investment_partner_assignments")) |>
#'     dplyr::select(property_id = entrata_property_id, investment_partner = partner_name) |>
#'     dplyr::collect()
#'
#'   weekly_pre_lease <- entrata_lease_execution_report() |>
#'     dplyr::select(
#'       property_id,
#'       weekly_new,
#'       weekly_renewal,
#'       weekly_total
#'     )
#'
#'   report_summary_data |>
#'     dplyr::left_join(
#'       model_beds,
#'       by = "property_id"
#'     ) |>
#'     dplyr::left_join(
#'       weekly_pre_lease,
#'       by = "property_id"
#'     ) |>
#'     dplyr::left_join(
#'       inv_partners,
#'       by = "property_id"
#'     ) |>
#'     dplyr::transmute(
#'       report_date = .env$report_date,
#'       property_id = .data$property_id,
#'       property_name = .data$property_name,
#'       investment_partner = .data$investment_partner,
#'       total_beds = .data$available_count, # units
#'       model_beds = .data$model_beds,
#'       current_occupied = .data$occupied_count,
#'       current_occupancy = .data$occupied_count / .data$total_beds,
#'       current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
#'       current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
#'       current_total_leases = .data$current_total_new + .data$current_total_renewals,
#'       current_preleased_percent = .data$current_total_leases / .data$total_beds,
#'       prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
#'       prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
#'       prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
#'       prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
#'       yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
#'       yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
#'       weekly_new = .data$weekly_new,
#'       weekly_renewal = .data$weekly_renewal,
#'       weekly_total = .data$weekly_total,
#'       weekly_percent_gained = .data$weekly_total / .data$total_beds,
#'       beds_left = .data$total_beds - .data$current_total_leases,
#'       vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
#'       vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
#'       vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
#'     )
#'
#' }
#'
#' merge_pre_lease_summary_data <- function(
#'     summary_data, # by property
#'     weekly_data, # from lease execution report
#'     model_beds_data,
#'     partners_data,
#'     report_date = Sys.Date()
#' ) {
#'
#'   weeks_left_to_lease <- get_weeks_left_to_lease(report_date)
#'
#'   weekly_data <- weekly_data |>
#'     dplyr::select("property_id", "weekly_new", "weekly_renewal", "weekly_total")
#'
#'   model_beds_data <- model_beds_data |>
#'     dplyr::select("property_id", "medel_beds" = "model_bed_count")
#'
#'   partners_data <- partners_data |>
#'     dplyr::select("property_id", "investment_partner" = "partner_name")
#'
#'   summary_data |>
#'     dplyr::left_join(
#'       model_beds_data,
#'       by = "property_id"
#'     ) |>
#'     dplyr::left_join(
#'       weekly_data,
#'       by = "property_id"
#'     ) |>
#'     dplyr::left_join(
#'       partners_data,
#'       by = "property_id"
#'     ) |>
#'     dplyr::transmute(
#'       report_date = .env$report_date,
#'       property_id = .data$property_id,
#'       property_name = .data$property_name,
#'       investment_partner = .data$investment_partner,
#'       total_beds = .data$available_count, # units
#'       model_beds = .data$model_beds,
#'       current_occupied = .data$occupied_count,
#'       current_occupancy = .data$occupied_count / .data$total_beds,
#'       current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
#'       current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
#'       current_total_leases = .data$current_total_new + .data$current_total_renewals,
#'       current_preleased_percent = .data$current_total_leases / .data$total_beds,
#'       current_preleased_percent_original = .data$preleased_percent,
#'       prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
#'       prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
#'       prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
#'       prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
#'       yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
#'       yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
#'       weekly_new = .data$weekly_new,
#'       weekly_renewal = .data$weekly_renewal,
#'       weekly_total = .data$weekly_total,
#'       weekly_percent_gained = .data$weekly_total / .data$total_beds,
#'       beds_left = .data$total_beds - .data$current_total_leases,
#'       vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
#'       vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
#'       vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
#'     )
#'
#' }
#'
#'
#' # weekly ------------------------------------------------------------------
#'
#' weekly_leasing_data <- entrata_lease_execution_report()
#'
