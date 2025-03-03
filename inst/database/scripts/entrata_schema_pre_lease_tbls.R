
entrata_config <- config::get("entrata")

report_date <- Sys.Date()

request_id <- 15L
report_version <- "3.2"

property_ids <- get_entrata_property_ids()

period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
period <- list(date = period_date, period_type = "date")


# by property -------------------------------------------------------------

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

pre_lease_resp_by_property <- httr2::req_perform(pre_lease_req_by_property)

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

pre_lease_queue_resp_by_property <- httr2::req_perform(pre_lease_queue_req_by_property)

pre_lease_report_data_by_property <- pre_lease_queue_resp_by_property |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData")

pre_lease_report_summary_data_by_property <- pre_lease_report_data_by_property |>
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
    "total_unit_count" = "units",
    "excluded_unit_count",
    "rentable_unit_count",
    "occupied_count",
    "available_count",
    "total_scheduled_rent" = "scheduled_rent_total",
    "yoy_variance" = "variance",
    tidyselect::starts_with("avg_"),
    tidyselect::starts_with("started_"),
    tidyselect::starts_with("partially_completed"),
    tidyselect::starts_with("completed_"),
    tidyselect::starts_with("approved_"),
    tidyselect::starts_with("preleased_")
  ) |>
  dplyr::arrange(
    .data$property_name
  )

weeks_left_to_lease <- get_weeks_left_to_lease()

pre_lease_report_summary_data_by_property_working <- pre_lease_report_summary_data_by_property |>
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


# by unit -----------------------------------------------------------------


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

pre_lease_resp_by_unit <- httr2::req_perform(pre_lease_req_by_unit)

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

pre_lease_queue_resp_by_unit <- httr2::req_perform(pre_lease_queue_req_by_unit)

pre_lease_report_data_by_unit <- pre_lease_queue_resp_by_unit |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData")

pre_lease_report_summary_data_by_unit <- pre_lease_report_data_by_unit |>
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
    "unit_type",
    "avg_sqft",
    "avg_advertised_rate",
    "total_unit_count" = "units",
    "excluded_unit_count",
    "rentable_unit_count",
    "occupied_count",
    "available_count",
    "total_scheduled_rent" = "scheduled_rent_total",
    "yoy_variance" = "variance",
    tidyselect::starts_with("avg_"),
    tidyselect::starts_with("started_"),
    tidyselect::starts_with("partially_completed"),
    tidyselect::starts_with("completed_"),
    tidyselect::starts_with("approved_"),
    tidyselect::starts_with("preleased_")
  ) |>
  dplyr::arrange(
    .data$property_name,
    dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
    .data$unit_type
  )

pre_lease_report_summary_data_by_unit_working <- pre_lease_report_summary_data_by_unit |>
  dplyr::transmute(
    report_date = .env$report_date,
    property_id = .data$property_id,
    property_name = .data$property_name,
    unit_type = .data$unit_type,
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
    yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent
  )

pre_lease_report_details_data <- pre_lease_report_data_by_unit |>
  purrr::pluck("details") |>
  dplyr::bind_rows() |>
  tibble::as_tibble() |>
  dplyr::mutate(
    report_date = as.Date(.env$report_date),
    property_id = as.integer(.data$property_id),
    sqft = as.numeric(sqft),
    resident_id = as.integer(resident_id),
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
  dplyr::mutate(
    dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
  ) |>
  dplyr::select(
    "report_date",
    "property_id",
    "property_name",
    "bldg_unit",
    "unit_type",
    "unit_status",
    "floorplan_name",
    "sqft",
    "resident_name" = "resident",
    "resident_id",
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
    .data$unit_type
  )


pre_lease_report_details_data_summarized <- pre_lease_report_details_data |>
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
  dplyr::ungroup()


pre_lease_report_property_summary_tables_by_unit <- pre_lease_report_summary_data_by_unit |>
  dplyr::select(
    "report_date",
    "property_id",
    "property_name",
    "unit_type",
    "excluded_units" = "excluded_unit_count",
    "rentable_units" = "rentable_unit_count",
    "occupied_units" = "occupied_count",
    "available_units" = "available_count",
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
    pre_lease_report_details_data_summarized,
    by = c("report_date", "property_id", "property_name", "unit_type")
  ) |>
  dplyr::mutate(
    dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
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
    "avg_scheduled_charges",
    "avg_actual_charges",
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
  dplyr::arrange(
    .data$property_name,
    dplyr::desc(.data$unit_type %in% c("Not Selected", "Studio")),
    .data$unit_type
  )

# weekly
weekly_leasing_data <- entrata_lease_execution_report()

# database
db_entrata_pre_lease_summary_by_property <- db_read_tbl(pool, "entrata.pre_lease_summary_by_property") |>
  dplyr::select(-created_at, -updated_at)
db_entrata_pre_lease_summary_by_unit <- db_read_tbl(pool, "entrata.pre_lease_summary_by_unit")
db_entrata_pre_lease_details <- db_read_tbl(pool, "entrata.pre_lease_report_details")
db_entrata_pre_lease_weekly <- db_read_tbl(pool, "entrata.pre_lease_weekly") |>
  dplyr::select(-created_at)

db_entrata_pre_lease_summary_by_property_current <- db_entrata_pre_lease_summary_by_property |>
  dplyr::filter(report_date == max(report_date))

db_entrata_pre_lease_summary_by_unit_current <- db_entrata_pre_lease_summary_by_unit |>
  dplyr::filter(report_date == max(report_date))

db_entrata_pre_lease_details_current <- db_entrata_pre_lease_details |>
  dplyr::filter(report_date == max(report_date))

pre_lease_report_summary_data_by_property <- pre_lease_report_summary_data_by_property |>
  dplyr::mutate(
    dplyr::across(
      c(
        "property_id",
        tidyselect::ends_with("_count"),
        tidyselect::ends_with("_count_prior"),
        tidyselect::ends_with("_variance")
      ),
      as.integer
    ),
    dplyr::across(
      c(
        tidyselect::ends_with("_percent"),
        tidyselect::ends_with("_percent_prior"),
        tidyselect::ends_with("_rent"),
        tidyselect::ends_with("_rate")
      ),
      as.numeric
    )
  )

waldo::compare(
  pre_lease_report_summary_data_by_property,
  db_entrata_pre_lease_summary_by_property_current,
  x_arg = "pre_lease_report_summary_data_by_property",
  y_arg = "db_entrata_pre_lease_summary_by_property_current"
)

dplyr::glimpse(pre_lease_report_summary_data_by_property)
dplyr::glimpse(db_entrata_pre_lease_summary_by_property_current)

# transform non-current pre_lease_summary_by_property data to match current data
# and merge with current data
pre_lease_report_summary_data_by_property_for_db <- db_entrata_pre_lease_summary_by_property |>
  dplyr::filter(report_date != max(report_date)) |>
  dplyr::bind_rows(pre_lease_report_summary_data_by_property) |>
  dplyr::select(
    "report_date",
    "property_id",
    "property_name",
    "avg_sqft",
    "avg_advertised_rate",
    "avg_scheduled_rent",
    "total_unit_count",
    "excluded_unit_count",
    "rentable_unit_count",
    "occupied_count",
    "available_count",
    "total_scheduled_rent",
    "yoy_variance",
    tidyselect::starts_with("started_") & !tidyselect::ends_with("_prior"),
    tidyselect::starts_with("started_") & tidyselect::ends_with("_prior"),
    tidyselect::starts_with("partially_completed") & !tidyselect::ends_with("_prior"),
    tidyselect::starts_with("partially_completed") & tidyselect::ends_with("_prior"),
    tidyselect::starts_with("completed_") & !tidyselect::ends_with("_prior"),
    tidyselect::starts_with("completed_") & tidyselect::ends_with("_prior"),
    tidyselect::starts_with("approved_") & !tidyselect::ends_with("_prior"),
    tidyselect::starts_with("approved_") & tidyselect::ends_with("_prior"),
    tidyselect::starts_with("preleased_") & !tidyselect::ends_with("_prior"),
    tidyselect::starts_with("preleased_") & tidyselect::ends_with("_prior")
  ) |>
  dplyr::arrange(
    .data$report_date,
    .data$property_name
  )

dplyr::glimpse(pre_lease_report_summary_data_by_unit_working)
dplyr::glimpse(db_entrata_pre_lease_summary_by_unit)

db_inv_partners <- db_read_tbl(pool, "gmh.properties") |>
  dplyr::select(
    property_id,
    partner_id
  ) |>
  dplyr::left_join(
    db_read_tbl(pool, "gmh.partners") |>
      dplyr::select(
        partner_id,
        partner_name
      ),
    by = "partner_id"
  )

db_model_beds <- db_read_tbl(pool, "gmh.model_beds") |>
  dplyr::select(
    property_id,
    model_bed_count
  )

global_summary_tbl <- pre_lease_report_summary_data_by_property_working |>
  dplyr::left_join(
    db_inv_partners,
    by = "property_id"
  ) |>
  dplyr::left_join(
    db_model_beds,
    by = "property_id"
  ) |>
  dplyr::left_join(
    weekly_leasing_data |>
      dplyr::select(
        report_date,
        property_id,
        weekly_new,
        weekly_renewal,
        weekly_total
      ),
    by = c("report_date", "property_id")
  ) |>
  dplyr::select(
    report_date,
    property_id,
    property_name,
    investment_partner = partner_name,
    total_beds,
    model_beds = model_bed_count,
    tidyselect::starts_with("current_"),
    tidyselect::starts_with("prior_"),
    tidyselect::starts_with("yoy_"),
    tidyselect::starts_with("weekly_"),
    beds_left,
    tidyselect::starts_with("vel_")
  ) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      ~dplyr::coalesce(.x, 0)
    )
  )


pre_lease_metrics <- list(
  ""
)
