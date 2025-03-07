
#  ------------------------------------------------------------------------
#
# Title : Entrata Pre-Lease Data Pipeline
#    By : Jimmy Briggs
#  Date : 2025-03-05
#
#  ------------------------------------------------------------------------


# setup -----------------------------------------------------------------------------------------------------------

entrata_config <- config::get("entrata")
report_date <- Sys.Date()
request_id <- 15L
report_version <- "3.2"
property_ids <- get_entrata_property_ids()
period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
period <- list(date = period_date, period_type = "date")


# by property -----------------------------------------------------------------------------------------------------

entrata_reports_pre_lease_by_property <- function(request_id = NULL) {

  entrata_config <- config::get("entrata")
  report_date <- Sys.Date()
  request_id <- request_id %||% as.integer(Sys.time())
  report_version <- "3.2"
  property_ids <- get_entrata_property_ids()
  period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
  period <- list(date = period_date, period_type = "date")

  pre_lease_report_filter_params_by_property <- list(
    property_group_ids = c(as.character(unlist(unname(property_ids)))),
    period = period,
    summarize_by = "property",
    group_by = "do_not_group",
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

  pre_lease_report_filter_params_by_property_json <- pre_lease_report_filter_params_by_property |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_req_body_by_property <- list(
    auth = list(type = 'basic'),
    request_id = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "pre_lease",
        reportVersion = report_version,
        filters = pre_lease_report_filter_params_by_property
      )
    )
  )

  pre_lease_req_by_property <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("reports") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(pre_lease_req_body_by_property)

  pre_lease_req_by_property_json <- pre_lease_req_by_property |>
    purrr::pluck("body", "data") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_resp_by_property <- httr2::req_perform(pre_lease_req_by_property)

  pre_lease_resp_by_property_json <- pre_lease_resp_by_property |>
    httr2::resp_body_json() |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_queue_id_by_property <- pre_lease_resp_by_property |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "queueId", 1)

  pre_lease_queue_req_by_property <- httr2::request(entrata_config$base_url) |>
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
            queueId = pre_lease_queue_id_by_property,
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

  pre_lease_queue_req_by_property_json <- pre_lease_queue_req_by_property |>
    purrr::pluck("body", "data") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_queue_resp_by_property <- httr2::req_perform(pre_lease_queue_req_by_property)

  pre_lease_queue_resp_by_property_json <- pre_lease_queue_resp_by_property |>
    httr2::resp_body_json() |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_report_data_by_property <- pre_lease_queue_resp_by_property |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "reportData")

  pre_lease_summary_by_property <- pre_lease_report_data_by_property |>
    purrr::pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      property_id = as.integer(.data$property_id),
      avg_sqft = as.numeric(.data$avg_sqft),
      avg_advertised_rate = as.numeric(.data$avg_advertised_rate),
      units = as.integer(.data$units),
      excluded_unit_count = as.integer(.data$excluded_unit_count),
      rentable_unit_count = as.integer(.data$rentable_unit_count),
      avg_scheduled_rent = as.numeric(.data$avg_scheduled_rent),
      occupied_count = as.integer(.data$occupied_count),
      variance = as.numeric(.data$variance),
      available_count = as.integer(available_count),
      scheduled_rent_total = as.numeric(scheduled_rent_total),
      dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      "avg_sqft",
      "avg_advertised_rate",
      "avg_scheduled_rent",
      "total_unit_count" = "units",
      "excluded_unit_count",
      "rentable_unit_count",
      "occupied_count",
      "available_count",
      "total_scheduled_rent" = "scheduled_rent_total",
      "yoy_variance" = "variance",
      "started_new_count",
      "started_renewal_count",
      "started_count",
      "started_percent",
      "started_new_count_prior",
      "started_renewal_count_prior",
      "started_count_prior",
      "partially_completed_new_count",
      "partially_completed_renewal_count",
      "partially_completed_count",
      "partially_completed_percent",
      "partially_completed_new_count_prior",
      "partially_completed_renewal_count_prior",
      "partially_completed_count_prior",
      "completed_new_count",
      "completed_renewal_count",
      "completed_count",
      "completed_percent",
      "completed_new_count_prior",
      "completed_renewal_count_prior",
      "completed_count_prior",
      "approved_new_count",
      "approved_renewal_count",
      "approved_count",
      "approved_percent",
      "approved_new_count_prior",
      "approved_renewal_count_prior",
      "approved_count_prior",
      "preleased_new_count",
      "preleased_renewal_count",
      "preleased_count",
      "preleased_percent",
      "preleased_new_count_prior",
      "preleased_renewal_count_prior",
      "preleased_count_prior",
      "preleased_percent_prior"
    ) |>
    dplyr::arrange(
      .data$property_name
    )

  pre_lease_details_data_by_property <- pre_lease_report_data_by_property |>
    purrr::pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      property_id = as.integer(.data$property_id),
      resident_id = as.integer(.data$resident_id),
      charge_code = ifelse(is.na(.data$charge_code), "Unknown", .data$charge_code),
      dplyr::across(
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
        ),
        lubridate::mdy
      )
    ) |>
    dplyr::distinct(
      .data$report_date,
      .data$property_id,
      .data$unit_type,
      .data$lease_id_display,
      .data$charge_code,
      .keep_all = TRUE
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      "bldg_unit",
      "unit_type",
      "unit_status",
      "charge_code",
      "sqft",
      "resident_id",
      "resident_name" = "resident",
      "resident_email" = "email",
      "resident_phone" = "phone_number",
      "resident_gender" = "gender",
      "lease_id" = "lease_id_display",
      "lease_status",
      "lease_sub_status",
      "lease_occupancy_type",
      "lease_term_name",
      "lease_term_month" = "lease_term",
      "space_option_preferred",
      "space_option",
      "lease_start_date" = "lease_start",
      "lease_end_date" = "lease_end",
      "lease_started_on_date" = "lease_started_on",
      "lease_partially_completed_on_date" = "lease_partially_completed_on",
      "lease_completed_on_date" = "lease_completed_on",
      "lease_approved_on_date" = "lease_approved_on",
      "move_in_date",
      "leasing_agent",
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
      .data$charge_code
    )

  return(
    list(
      summary_data = pre_lease_summary_by_property,
      details_data = pre_lease_details_data_by_property,
      report_req_json = pre_lease_req_by_property_json,
      report_resp_json = pre_lease_resp_by_property_json,
      queue_req_json = pre_lease_queue_req_by_property_json,
      queue_resp_json = pre_lease_queue_resp_by_property_json,
      report_params_json = pre_lease_report_filter_params_by_property_json,
      queue_id = pre_lease_queue_id_by_property
    )
  )

}

# by unit ---------------------------------------------------------------------------------------------------------

entrata_reports_pre_lease_by_unit <- function(request_id) {

  entrata_config <- config::get("entrata")
  report_date <- Sys.Date()
  request_id <- request_id %||% as.integer(Sys.time())
  report_version <- "3.2"
  property_ids <- get_entrata_property_ids()
  period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
  period <- list(date = period_date, period_type = "date")

  pre_lease_report_filter_params_by_unit <- list(
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

  pre_lease_report_filter_params_by_unit_json <- pre_lease_report_filter_params_by_unit |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_req_body_by_unit <- list(
    auth = list(type = 'basic'),
    request_id = request_id,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "pre_lease",
        reportVersion = report_version,
        filters = pre_lease_report_filter_params_by_unit
      )
    )
  )

  pre_lease_req_by_unit <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("reports") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(pre_lease_req_body_by_unit)

  pre_lease_req_by_unit_json <- pre_lease_req_by_unit |>
    purrr::pluck("body", "data") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_resp_by_unit <- httr2::req_perform(pre_lease_req_by_unit)

  pre_lease_resp_by_unit_json <- pre_lease_resp_by_unit |>
    httr2::resp_body_json() |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_queue_id_by_unit <- pre_lease_resp_by_unit |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "queueId", 1)

  pre_lease_queue_req_by_unit <- httr2::request(entrata_config$base_url) |>
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
            queueId = pre_lease_queue_id_by_unit,
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

  pre_lease_queue_req_by_unit_json <- pre_lease_queue_req_by_unit |>
    purrr::pluck("body", "data") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_queue_resp_by_unit <- httr2::req_perform(pre_lease_queue_req_by_unit)

  pre_lease_queue_resp_by_unit_json <- pre_lease_queue_resp_by_unit |>
    httr2::resp_body_json() |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_summary_by_property_unit_resp_json <- pre_lease_queue_resp_by_unit |>
    httr2::resp_body_json() |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_summary_by_property_unit_req_json <- pre_lease_req_by_unit |>
    purrr::pluck("body", "data") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_summary_by_property_unit_report_params_json <- pre_lease_report_filter_params_by_unit |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  pre_lease_report_data_by_unit <- pre_lease_queue_resp_by_unit |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "reportData")

  pre_lease_summary_by_property_unit <- pre_lease_report_data_by_unit |>
    purrr::pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      property_id = as.integer(.data$property_id),
      avg_sqft = as.numeric(.data$avg_sqft),
      avg_advertised_rate = as.numeric(.data$avg_advertised_rate),
      units = as.integer(.data$units),
      excluded_unit_count = as.integer(.data$excluded_unit_count),
      rentable_unit_count = as.integer(.data$rentable_unit_count),
      avg_scheduled_rent = as.numeric(.data$avg_scheduled_rent),
      occupied_count = as.integer(.data$occupied_count),
      variance = as.integer(.data$variance),
      available_count = as.integer(available_count),
      scheduled_rent_total = as.numeric(scheduled_rent_total),
      dplyr::across(tidyselect::where(is.integer), ~dplyr::coalesce(.x, 0L)),
      dplyr::across(
        tidyselect::all_of(
          c(
            "avg_sqft",
            "avg_advertised_rate",
            "avg_scheduled_rent",
            "scheduled_rent_total",
            "started_percent",
            "partially_completed_percent",
            "completed_percent",
            "approved_percent",
            "preleased_percent",
            "preleased_percent_prior"
          )
        ),
        ~dplyr::coalesce(.x, 0.00))
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      "unit_type",
      "avg_sqft",
      "avg_advertised_rate",
      "avg_scheduled_rent",
      "total_unit_count" = "units",
      "excluded_unit_count",
      "rentable_unit_count",
      "occupied_count",
      "available_count",
      "total_scheduled_rent" = "scheduled_rent_total",
      "yoy_variance" = "variance",
      "started_new_count",
      "started_renewal_count",
      "started_count",
      "started_percent",
      "started_new_count_prior",
      "started_renewal_count_prior",
      "started_count_prior",
      "partially_completed_new_count",
      "partially_completed_renewal_count",
      "partially_completed_count",
      "partially_completed_percent",
      "partially_completed_new_count_prior",
      "partially_completed_renewal_count_prior",
      "partially_completed_count_prior",
      "completed_new_count",
      "completed_renewal_count",
      "completed_count",
      "completed_percent",
      "completed_new_count_prior",
      "completed_renewal_count_prior",
      "completed_count_prior",
      "approved_new_count",
      "approved_renewal_count",
      "approved_count",
      "approved_percent",
      "approved_new_count_prior",
      "approved_renewal_count_prior",
      "approved_count_prior",
      "preleased_new_count",
      "preleased_renewal_count",
      "preleased_count",
      "preleased_percent",
      "preleased_new_count_prior",
      "preleased_renewal_count_prior",
      "preleased_count_prior",
      "preleased_percent_prior"
    ) |>
    dplyr::arrange(
      .data$property_id,
      .data$unit_type
    )

  pre_lease_details_by_property_unit <- pre_lease_report_data_by_unit |>
    purrr::pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      property_id = as.integer(.data$property_id),
      resident_id = as.integer(.data$resident_id),
      dplyr::across(
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
        ),
        lubridate::mdy
      )
    ) |>
    dplyr::distinct(
      .data$report_date,
      .data$property_id,
      .data$property_name,
      .data$bldg_unit,
      .data$unit_type,
      .data$lease_id_display,
      .data$charge_code,
      .keep_all = TRUE
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      "bldg_unit",
      "unit_type",
      "unit_status",
      "charge_code",
      "resident_id",
      "resident_name" = "resident",
      "resident_email" = "email",
      "resident_phone" = "phone_number",
      "resident_gender" = "gender",
      "lease_id" = "lease_id_display",
      "lease_status",
      "lease_sub_status",
      "lease_term_name",
      "lease_term",
      "lease_start_date" = "lease_start",
      "lease_end_date" = "lease_end",
      "lease_started_on_date" = "lease_started_on",
      "lease_partially_completed_on_date" = "lease_partially_completed_on",
      "lease_completed_on_date" = "lease_completed_on",
      "lease_approved_on_date" = "lease_approved_on",
      "move_in_date",
      "leasing_agent",
      "deposit_charged",
      "deposit_held",
      "market_rent",
      "budgeted_rent",
      "advertised_rate",
      "scheduled_rent",
      "actual_charges",
      "scheduled_rent_total"
    )

  return(
    list(
      summary_data = pre_lease_summary_by_property_unit,
      details_data = pre_lease_details_by_property_unit,
      report_req_json = pre_lease_req_by_unit_json,
      report_resp_json = pre_lease_resp_by_unit_json,
      queue_req_json = pre_lease_queue_req_by_unit_json,
      queue_resp_json = pre_lease_queue_resp_by_unit_json,
      report_params_json = pre_lease_report_filter_params_by_unit_json,
      queue_id = pre_lease_queue_id_by_unit
    )
  )

}


# database --------------------------------------------------------------------------------------------------------




# workflow --------------------------------------------------------------------------------------------------------

pre_lease_by_property_data <- entrata_reports_pre_lease_by_property()

pool <- db_connect()

db_upsert_entrata_pre_lease_by_property(pool, pre_lease_by_property_data)

pre_lease_by_unit_data <- entrata_reports_pre_lease_by_unit()


# global summary table --------------------------------------------------------------------------------------------

weeks_left_to_lease <- get_weeks_left_to_lease()

pre_lease_global_summary_tbl <- pre_lease_report_summary_data_by_property |>
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





#########


db_upsert_entrata_pre_lease_summary_by_property_unit <- function(
    pool,
    summary_data,
    req_json = NULL,
    resp_json = NULL,
    report_params_json = NULL
) {



}


dplyr::transmute(
  report_date = as.Date(.env$report_date),
  property_id = as.integer(.data$property_id),
  property_name = .data$property_name,
  unit_type = .data$unit_type,
  avg_sqft = as.numeric(.data$avg_sqft),
  avg_advertised_rate = as.numeric(.data$avg_advertised_rate),
  avg_scheduled_rent = as.numeric(.data$avg_scheduled_rent),
  total_unit_count = as.integer(.data$units),
  excluded_unit_count = as.integer(.data$excluded_unit_count),
  rentable_unit_count = as.integer(.data$rentable_unit_count),
  occupied_count = as.integer(.data$occupied_count),
  available_count = as.integer(.data$available_count),
  total_scheduled_rent = as.numeric(.data$scheduled_rent_total),
  yoy_variance = as.numeric(.data$variance),

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
  beds_left = .data$total_beds - .data$current_total_leases
)

# CREATE TABLE entrata.pre_lease_summary_by_property_unit (
#   report_date DATE NOT NULL,
#   property_id INTEGER NOT NULL,
#   property_name TEXT NOT NULL,
#   unit_type TEXT NOT NULL,
#   avg_sqft NUMERIC DEFAULT 0.00,
#   avg_advertised_rate NUMERIC DEFAULT 0.00,
#   avg_scheduled_rent NUMERIC(15,2) DEFAULT 0.00,
#   total_unit_count INTEGER DEFAULT 0,
#   excluded_unit_count INTEGER DEFAULT 0,
#   rentable_unit_count INTEGER DEFAULT 0,
#   occupied_count INTEGER DEFAULT 0,
#   available_count INTEGER DEFAULT 0,
#   total_scheduled_rent NUMERIC(15,2) DEFAULT 0.00,
#   yoy_variance INTEGER DEFAULT 0,
#   started_new_count INTEGER DEFAULT 0,
#   started_renewal_count INTEGER DEFAULT 0,
#   started_count INTEGER DEFAULT 0,
#   started_percent NUMERIC DEFAULT 0.00,
#   started_new_count_prior INTEGER DEFAULT 0,
#   started_renewal_count_prior INTEGER DEFAULT 0,
#   started_count_prior INTEGER DEFAULT 0,
#   partially_completed_new_count INTEGER DEFAULT 0,
#   partially_completed_renewal_count INTEGER DEFAULT 0,
#   partially_completed_count INTEGER DEFAULT 0,
#   partially_completed_percent NUMERIC DEFAULT 0.00,
#   partially_completed_new_count_prior INTEGER DEFAULT 0,
#   partially_completed_renewal_count_prior INTEGER DEFAULT 0,
#   partially_completed_count_prior INTEGER DEFAULT 0,
#   completed_new_count INTEGER DEFAULT 0,
#   completed_renewal_count INTEGER DEFAULT 0,
#   completed_count INTEGER DEFAULT 0,
#   completed_percent NUMERIC DEFAULT 0.00,
#   completed_new_count_prior INTEGER DEFAULT 0,
#   completed_renewal_count_prior INTEGER DEFAULT 0,
#   completed_count_prior INTEGER DEFAULT 0,
#   approved_new_count INTEGER DEFAULT 0,
#   approved_renewal_count INTEGER DEFAULT 0,
#   approved_count INTEGER DEFAULT 0,
#   approved_percent NUMERIC DEFAULT 0.00,
#   approved_new_count_prior INTEGER DEFAULT 0,
#   approved_renewal_count_prior INTEGER DEFAULT 0,
#   approved_count_prior INTEGER DEFAULT 0,
#   preleased_new_count INTEGER DEFAULT 0,
#   preleased_renewal_count INTEGER DEFAULT 0,
#   preleased_count INTEGER DEFAULT 0,
#   preleased_percent NUMERIC DEFAULT 0.00,
#   preleased_new_count_prior INTEGER DEFAULT 0,
#   preleased_renewal_count_prior INTEGER DEFAULT 0,
#   preleased_count_prior INTEGER DEFAULT 0,
#   preleased_percent_prior NUMERIC DEFAULT 0.00,
#   created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
#   updated_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP,
#   PRIMARY KEY (report_date, property_id, unit_type)
# );

pre_lease_per_property_unit_summary_tbl <- pre_lease_report_data_by_unit |>
  purrr::pluck("summary") |>
  dplyr::bind_rows() |>
  tibble::as_tibble() |>
  dplyr::filter(.data$unit_type != "Not Selected") |>
  dplyr::select(
    "property_id",
    "property_name",
    "unit_type",
    "units",
    "excluded_unit_count",
    "rentable_unit_count",
    "avg_sqft",
    "occupied_count",
    "available_count",
    dplyr::starts_with("approved_"),
    dplyr::starts_with("partially_completed"),
    dplyr::starts_with("completed_"),
    dplyr::starts_with("preleased_"),
    dplyr::starts_with("started_"),
    dplyr::starts_with("avg_"),
    "variance",
    "scheduled_rent_total"
  ) |>
  dplyr::transmute(
    report_date = .env$report_date,
    property_id = as.integer(.data$property_id),
    property_name = .data$property_name,
    unit_type = .data$unit_type,
    excluded_units = .data$excluded_unit_count,
    rentable_units = .data$rentable_unit_count,
    avg_scheduled_charges = .data$scheduled_rent_total,
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
    beds_left = .data$total_beds - .data$current_total_leases
  )



db_upsert_entrata_pre_lease_details_by_property_unit <- function(
    pool,
    details_data,
    req_json = NULL,
    resp_json = NULL,
    report_params_json = NULL
) {



}


pre_lease_per_property_unit_summary_tbl_avg_charges <- pre_lease_per_property_lease_details_tbl |>
  dplyr::filter(.data$charge_code == "Base Rent") |>
  dplyr::group_by(
    report_date,
    property_id,
    unit_type
  ) |>
  dplyr::summarize(
    avg_scheduled_charges = mean(.data$scheduled_rent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::ungroup()

pre_lease_per_property_unit_summary_tbl <- pre_lease_per_property_unit_summary_tbl |>
  dplyr::select(-avg_scheduled_charges) |>
  dplyr::left_join(
    pre_lease_per_property_unit_summary_tbl_avg_charges,
    by = c("report_date", "property_id", "unit_type")
  ) |>
  dplyr::mutate(
    avg_scheduled_charges = dplyr::coalesce(avg_scheduled_charges, 0)
  ) |>
  dplyr::select(
    "report_date",
    "property_id",
    "property_name",
    "unit_type",
    "excluded_units",
    "rentable_units",
    "avg_scheduled_charges",
    "total_beds",
    "current_occupied",
    "current_occupancy",
    "current_total_new",
    "current_total_renewals",
    "current_total_leases",
    "current_preleased_percent",
    "prior_total_new",
    "prior_total_renewals",
    "prior_total_leases",
    "prior_preleased_percent",
    "yoy_variance_count",
    "yoy_variance_percent",
    "beds_left"
  )

# finalize --------------------------------------------------------------------------------------------------------

# global summary table
dplyr::glimpse(pre_lease_global_summary_tbl)

