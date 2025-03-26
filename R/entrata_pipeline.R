
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

  int_cols <- summary_data |>
    dplyr::select(
      tidyselect::all_of(c("property_id", "units", "variance")),
      tidyselect::ends_with("_count"),
      tidyselect::ends_with("_count_prior"),
      tidyselect::starts_with("number_")
    ) |>
    names()

  num_cols <- summary_data |>
    dplyr::select(
      tidyselect::starts_with("avg_"),
      tidyselect::ends_with("_percent"),
      tidyselect::ends_with("_percent_prior"),
      tidyselect::ends_with("_total"),
      tidyselect::ends_with("_rent"),
      tidyselect::ends_with("_rent_total")
    ) |>
    names()

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

  date_cols <- details_data |>
    dplyr::select(
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
    ) |>
    names()

  int_cols <- details_data |>
    dplyr::select(
      tidyselect::any_of(c("property_id", "sqft", "resident_id", "lease_term")),
      tidyselect::starts_with("number_"),
      tidyselect::ends_with("_count"),
      tidyselect::ends_with("_count_prior"),
      tidyselect::ends_with("_variance")
    ) |>
    names()

  num_cols <- details_data |>
    dplyr::select(
      tidyselect::starts_with("deposit_"),
      tidyselect::ends_with("_rent"),
      tidyselect::ends_with("_rate"),
      tidyselect::ends_with("_total"),
      tidyselect::ends_with("_charges")
    ) |>
    names()

  # check for duplicates
  dups <- janitor::get_dupes(details_data, tidyselect::everything())
  if (nrow(dups) > 0) {
    cli::cli_alert_warning(
      c(
        "Detected {.field {nrow(dups)}} duplicate records in the Pre-Lease Details Data!"
      )
    )
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

  entrata_resp_parse_pre_lease_by_property(queue_resp, report_date = report_date)

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
