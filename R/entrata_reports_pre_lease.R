
#  ------------------------------------------------------------------------
#
# Title : Entrata Pre-Lease Report
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata Pre-Lease Report
#'
#' @description
#' Retrieves the summary and details tables for the `pre_lease` report from the
#' Entrata API. The function dynamically constructs the API request and parses
#' the response to return clean summary and details tables.
#'
#' @param property_ids A vector of property IDs to include in the report. Defaults
#'   to all properties.
#' @param report_date The date to use for the report. Defaults to the current date.
#'   This date will be used to derive the target date for the pre-lease season, which
#'   is always `9/1` of the current year or the next year, depending on the current
#'   date's month.
#' @param summarize_by A character value representing the `summarize_by` report filter
#'   parameter for the pre_lease report. Defaults to `"property"`. Other options are
#'   `"unit_type"`, `"floorplan_name"`, or `"do_not_summarize"`.
#' @param group_by A character value representing the `group_by` report filter parameter
#'   for the pre_lease report. Defaults to `"do_not_group"`. Other options are `"unit_type"`,
#'   `"floorplan_name"`, or `"lease_term"`, or `"do_not_group"`.
#' @param consider_pre_leased_on A numeric value representing the `consider_pre_leased_on`
#'   report filter parameter for the pre_lease report. Defaults to `32` which represents
#'   `"Lease:Partially Completed"`. Other options are `33`, `34`, `41`, `42`, `43`, and `44`.
#' @param ... Named parameters to pass as additional pre_lease report filter parameters.
#'   Must be valid parameters for the pre_lease report.
#' @param request_id A unique identifier for the request. Defaults to the current
#'   timestamp.
#' @param max_retries The maximum number of retries to attempt when performing the request to
#'   the `/queue` endpoint. Defaults to `10`.
#' @param entrata_config The Entrata configuration object. Defaults to the global
#'   Entrata configuration object.
#'
#' @returns
#' A list containing:
#' - `summary` - A tibble representing the summary table for the pre_lease report.
#' - `details` - A tibble representing the details table for the pre_lease report.
#' - `parameters` - A list of the parameters used in the request.
#'
#' @export
entrata_pre_lease_report <- function(
    property_ids = NULL,
    report_date = Sys.Date(),
    summarize_by = c("unit_type", "property", "floorplan_name", "do_not_summarize"),
    group_by = c("unit_type", "floorplan_name", "lease_term", "do_not_group"),
    consider_pre_leased_on = "332",
    charge_code_detail = 0,
    space_options = "do_not_show",
    additional_units_shown = "available",
    combine_unit_spaces_with_same_lease = 0,
    consolidate_by = "no_consolidation",
    arrange_by_property = 0,
    subtotals = list("summary", "details"),
    yoy = 1,
    lease_occupancy_types = NULL,
    report_version = c("3.2", "3.3"),
    request_id = NULL,
    max_retries = 10,
    entrata_config = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- get_entrata_config() }
  if (is.null(report_date)) { report_date <- Sys.Date() }
  if (is.null(request_id)) { request_id <- as.integer(Sys.time()) }
  if (is.null(property_ids)) { property_ids <- get_entrata_property_ids() }

  # validation
  group_by <- rlang::arg_match(group_by, multiple = FALSE)
  summarize_by <- rlang::arg_match(summarize_by, multiple = FALSE)
  report_version <- rlang::arg_match(report_version, multiple = FALSE)
  charge_code_detail <- as.integer(charge_code_detail) %||% 1
  space_options <- space_options %||% "do_not_show"
  additional_units_shown <- additional_units_shown %||% "available"
  combine_unit_spaces_with_same_lease <- combine_unit_spaces_with_same_lease %||% 0
  consolidate_by <- consolidate_by %||% "no_consolidation"
  arrange_by_property <- arrange_by_property %||% 0
  subtotals <- subtotals %||% list("summary", "details")
  yoy <- yoy %||% 1
  period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
  period <- list(
    date = period_date,
    period_type = "date"
  )

  # derive request body report filter parameters
  report_filter_params <- list(
    property_group_ids = c(as.character(unlist(unname(property_ids)))),
    period = period,
    summarize_by = summarize_by,
    group_by = group_by,
    consider_pre_leased_on = as.character(consider_pre_leased_on),
    charge_code_detail = charge_code_detail,
    space_options = space_options,
    additional_units_shown = additional_units_shown,
    combine_unit_spaces_with_same_lease = combine_unit_spaces_with_same_lease,
    consolidate_by = consolidate_by,
    arrange_by_property = arrange_by_property,
    subtotals = subtotals,
    yoy = yoy
  )

  # create the report request body
  req_body <- list(
    auth = list(type = 'basic'),
    request_id = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "pre_lease",
        reportVersion = report_version,
        filters = report_filter_params
      )
    )
  )

  # build request
  req <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("reports") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(req_body)

  # send request
  resp <- httr2::req_perform(req)

  # check response
  entrata_resp_check_status(resp)

  # parse response to get queue id
  queue_id <- httr2::resp_body_json(resp) |>
    pluck("response", "result", "queueId", 1)

  cli::cli_alert_success(
    c(
      "Pre-Lease Report Request Submitted\n",
      "Queue ID: {.field {queue_id}}"
    )
  )

  # call queue endpoint with queue id
  queue_req <- httr2::request(entrata_config$base_url) |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_url_path_append("queue") |>
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
    httr2::req_retry(
      max_tries = 20,
      max_seconds = 600,
      retry_on_failure = TRUE,
      is_transient = entrata_resp_is_transient,
      backoff = exponential_backoff
    )

  # send queue request
  queue_resp <- httr2::req_perform(queue_req)

  # check response
  entrata_resp_check_status(queue_resp)

  # parse response to get report data
  report_data <- httr2::resp_body_json(queue_resp) |>
    pluck("response", "result", "reportData")

  # summary & details
  summary_data <- report_data |>
    pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  details_data <- report_data |>
    pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  # return
  list(
    summary = summary_data,
    details = details_data,
    parameters = list(
      report_filter_params = report_filter_params,
      request_id = request_id,
      queue_id = queue_id,
      report_date = report_date,
      reports_req = req,
      reports_resp = resp,
      queue_req = queue_req,
      queue_resp = queue_resp
    )
  )

}

entrata_pre_lease_report_property_details <- function(summary_data, details_data) {

  if (summarize_by != "unit_type") {
    details_data_summary_by_property <- NULL
  } else {
    details_data_summary_by_property <- summary_data_tidy |>
      dplyr::select(
        "report_date",
        "property_id",
        "property_name",
        "unit_type",
        "excluded_units" = "excluded_unit_count",
        "rentable_units" = "rentable_unit_count",
        "occupied_units" = "occupied_count",
        "available_units" = "available_count",
        # "avg_scheduled_charges" = "avg_scheduled_rent",
        "current_preleased_new_count" = "preleased_new_count",
        "prior_preleased_new_count" = "preleased_new_count_prior",
        "current_preleased_renewal_count" = "preleased_renewal_count",
        "prior_preleased_renewal_count" = "preleased_renewal_count_prior",
        "current_preleased_total_count" = "preleased_count",
        "prior_preleased_total_count" = "preleased_count_prior",
        "current_preleased_percent" = "preleased_percent",
        "prior_preleased_percent" = "preleased_percent_prior",
        "yoy_variance"
      ) |>
      dplyr::left_join(
        details_data_tidy |>
          dplyr::group_by(
            report_date,
            property_id,
            property_name,
            unit_type
          ) |>
          dplyr::summarize(
            avg_market_rent = mean(.data$market_rent, na.rm = TRUE),
            avg_budgeted_rent = mean(.data$budgeted_rent, na.rm = TRUE),
            avg_advertised_rate = mean(.data$advertised_rate, na.rm = TRUE),
            avg_scheduled_rent = mean(.data$scheduled_rent, na.rm = TRUE),
            avg_actual_charges = mean(.data$actual_charges, na.rm = TRUE),
            avg_scheduled_charges = mean(.data$scheduled_rent_total, na.rm = TRUE)
          ) |>
          dplyr::ungroup(),
        by = c("report_date", "property_id", "property_name", "unit_type")
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
        "avg_market_rent",
        "avg_budgeted_rent",
        "avg_advertised_rate",
        "avg_scheduled_rent",
        "avg_actual_charges",
        "avg_scheduled_charges",
        "current_preleased_new_count",
        "prior_preleased_new_count",
        "current_preleased_renewal_count",
        "prior_preleased_renewal_count",
        "current_preleased_total_count",
        "prior_preleased_total_count",
        "current_preleased_percent",
        "prior_preleased_percent",
        "yoy_variance"
      ) |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
      ) |>
      dplyr::arrange(
        .data$property_name,
        dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
        .data$unit_type
      )
  }
}

entrata_pre_lease_report_summary <- function(...) {

  hold <- entrata_pre_lease_report(summarize_by = "property", ...)

  report_date <- hold$parameters$report_date

  weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

  hold$summary |>
    dplyr::transmute(
      report_date = .env$report_date,
      property_id = .data$property_id,
      property_name = .data$property_name,
      total_beds = .data$available_count,
      current_occupied = .data$occupied_count,
      current_occupancy = .data$occupied_count / .data$total_beds,
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
      vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
      vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
      vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
    )
}

get_entrata_pre_lease_summary <- function(pool = NULL) {

  if (is.null(pool)) pool <- db_connect()
  on.exit(pool::poolClose(pool))

  report_data <- entrata_pre_lease_report()
  report_date <- report_data$parameters$report_date

  report_summary_data <- purrr::pluck(report_data, "summary")

  weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

  model_beds <- dplyr::tbl(pool, I("gmh.model_beds")) |>
    dplyr::select(-gmh_property_id, -updated_at) |>
    dplyr::rename(property_id = entrata_property_id) |>
    dplyr::select(-property_name) |>
    dplyr::collect()

  inv_partners <- dplyr::tbl(pool, I("gmh.investment_partner_assignments")) |>
    dplyr::select(property_id = entrata_property_id, investment_partner = partner_name) |>
    dplyr::collect()

  weekly_pre_lease <- entrata_lease_execution_report() |>
    dplyr::select(
      property_id,
      weekly_new,
      weekly_renewal,
      weekly_total
    )

  report_summary_data |>
    dplyr::left_join(
      model_beds,
      by = "property_id"
    ) |>
    dplyr::left_join(
      weekly_pre_lease,
      by = "property_id"
    ) |>
    dplyr::left_join(
      inv_partners,
      by = "property_id"
    ) |>
    dplyr::transmute(
      report_date = .env$report_date,
      property_id = .data$property_id,
      property_name = .data$property_name,
      investment_partner = .data$investment_partner,
      total_beds = .data$available_count, # units
      model_beds = .data$model_beds,
      current_occupied = .data$occupied_count,
      current_occupancy = .data$occupied_count / .data$total_beds,
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
      weekly_new = .data$weekly_new,
      weekly_renewal = .data$weekly_renewal,
      weekly_total = .data$weekly_total,
      weekly_percent_gained = .data$weekly_total / .data$total_beds,
      beds_left = .data$total_beds - .data$current_total_leases,
      vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
      vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
      vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
    )

}

merge_pre_lease_summary_data <- function(
    summary_data, # by property
    weekly_data, # from lease execution report
    model_beds_data,
    partners_data,
    report_date = Sys.Date()
) {

  weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

  weekly_data <- weekly_data |>
    dplyr::select("property_id", "weekly_new", "weekly_renewal", "weekly_total")

  model_beds_data <- model_beds_data |>
    dplyr::select("property_id", "medel_beds" = "model_bed_count")

  partners_data <- partners_data |>
    dplyr::select("property_id", "investment_partner" = "partner_name")

  summary_data |>
    dplyr::left_join(
      model_beds_data,
      by = "property_id"
    ) |>
    dplyr::left_join(
      weekly_data,
      by = "property_id"
    ) |>
    dplyr::left_join(
      partners_data,
      by = "property_id"
    ) |>
    dplyr::transmute(
      report_date = .env$report_date,
      property_id = .data$property_id,
      property_name = .data$property_name,
      investment_partner = .data$investment_partner,
      total_beds = .data$available_count, # units
      model_beds = .data$model_beds,
      current_occupied = .data$occupied_count,
      current_occupancy = .data$occupied_count / .data$total_beds,
      current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
      current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
      current_total_leases = .data$current_total_new + .data$current_total_renewals,
      current_preleased_percent = .data$current_total_leases / .data$total_beds,
      current_preleased_percent_original = .data$preleased_percent,
      prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
      prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
      prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
      prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
      yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
      yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
      weekly_new = .data$weekly_new,
      weekly_renewal = .data$weekly_renewal,
      weekly_total = .data$weekly_total,
      weekly_percent_gained = .data$weekly_total / .data$total_beds,
      beds_left = .data$total_beds - .data$current_total_leases,
      vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
      vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
      vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
    )

}

