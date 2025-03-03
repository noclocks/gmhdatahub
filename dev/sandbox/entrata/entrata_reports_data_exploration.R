entrata_config <- get_entrata_config()

property_ids <- memoise::memoise(get_entrata_property_ids)()

# weekly performance ------------------------------------------------------

report_name <- "weekly_performance"
report_date <- Sys.Date()
report_version <- "3.0"

# period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
period <- list(period_type = "lastwk")
request_id <- request_id %||% as.integer(Sys.time())

req_filter_params <- list(
  property_group_ids = c(as.character(unlist(unname(property_ids)))),
  period = period,
  summary = "property"
)

req_body <- list(
  auth = list(type = 'basic'),
  request_id = request_id,
  method = list(
    name = "getReportData",
    version = "r3",
    params = list(
      reportName = report_name,
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

weekly_performance_report_resp_data <- queue_resp |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData") |>
  dplyr::bind_rows()


# pricing_lease_performance_analysis --------------------------------------


report_name <- "pricing_lease_performance_analysis"
report_date <- Sys.Date()
report_version <- get_latest_report_version(report_name)

# period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
# period <- list(period_type = "lastwk")
request_id <- request_id %||% as.integer(Sys.time())

req_filter_params <- list(
  property_group_ids = c(as.character(unlist(unname(property_ids))))
)

req_body <- list(
  auth = list(type = 'basic'),
  request_id = request_id,
  method = list(
    name = "getReportData",
    version = "r3",
    params = list(
      reportName = report_name,
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

pricing_lease_performance_analysis_resp_data <- queue_resp |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData")


# lease_activity ----------------------------------------------------------


report_name <- "lease_activity"
report_date <- Sys.Date()
report_version <- get_latest_report_version(report_name)
request_id <- request_id %||% as.integer(Sys.time())

# "period": {
#   "daterange": {
#     "daterange-start": "m/d/Y",
#     "daterange-end": "m/d/Y"
#   },
#   "date": "m/d/Y",
#   "period_type": "today,yesterday,currentwk,lastwk,currentcm,priorcm,nextcm,currentcq,priorcq,nextcq,currentcyr,priorcyr,nextcyr,daterange,date"
# },

# period_date <- get_entrata_custom_pre_lease_date() |> entrata_date()
period <- list(period_type = "lastwk")

charge_code_details <- 1L
count_transfer_as_move_ins <- 0L

req_filter_params <- list(
  property_group_ids = c(as.character(unlist(unname(property_ids)))),
  period = period,
  charge_code_details = charge_code_details,
  count_transfer_as_move_ins = count_transfer_as_move_ins
)

req_body <- list(
  auth = list(type = 'basic'),
  request_id = request_id,
  method = list(
    name = "getReportData",
    version = "r3",
    params = list(
      reportName = report_name,
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

lease_activity_report_resp_data <- queue_resp |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData") |>
  dplyr::bind_rows()


# buildings_and_units -----------------------------------------------------

report_name <- "buildings_and_units"
report_date <- Sys.Date()
report_version <- get_latest_report_version(report_name)
request_id <- request_id %||% as.integer(Sys.time())

sort_by <- "unit_type"
group_by <- "do_not_group"
expand_by <- "do_not_expand"
summarize_units <- 0L
consolidate_by <- "no_consolidation"
arrange_by_property <- 0L
subtotals <- 0L

req_filter_params <- list(
  property_group_ids = c(as.character(unlist(unname(property_ids)))),
  sort_by = sort_by,
  group_by = group_by,
  expand_by = expand_by,
  summarize_units = summarize_units,
  consolidate_by = consolidate_by,
  arrange_by_property = arrange_by_property,
  subtotals = subtotals
)

req_body <- list(
  auth = list(type = 'basic'),
  request_id = request_id,
  method = list(
    name = "getReportData",
    version = "r3",
    params = list(
      reportName = report_name,
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

buildings_and_units_report_resp_data <- queue_resp |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData") |>
  dplyr::bind_rows()


# occupancy_vacancy -------------------------------------------------------


report_name <- "occupancy_vacancy"
report_date <- Sys.Date()
report_version <- get_latest_report_version(report_name)
request_id <- request_id %||% as.integer(Sys.time())

period <- list(period_type = "today")
report_metric <- "occupancy_rate"
summarize_by <- "unit_type"
group_by <- "unit_type"
show_historical <- 1L
excluded_units <- 0L
lease_end_date_as_move_out <- 0L
summarize_units <- 1L
consolidate_by <- "no_consolidation"
subtotals <- 0L

req_filter_params <- list(
  property_group_ids = c(as.character(unlist(unname(property_ids)))),
  period = period,
  report_metric = report_metric,
  summarize_by = summarize_by,
  group_by = group_by,
  show_historical = show_historical,
  excluded_units = excluded_units,
  lease_end_date_as_move_out = lease_end_date_as_move_out,
  summarize_units = summarize_units,
  consolidate_by = consolidate_by,
  subtotals = subtotals
)

req_body <- list(
  auth = list(type = 'basic'),
  request_id = request_id,
  method = list(
    name = "getReportData",
    version = "r3",
    params = list(
      reportName = report_name,
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

occupancy_report_resp_data <- queue_resp |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData") |>
  dplyr::bind_rows()


# forecasted_occupancy ----------------------------------------------------

report_name <- "forecasted_occupancy"
report_date <- Sys.Date()
report_version <- get_latest_report_version(report_name)
request_id <- request_id %||% as.integer(Sys.time())

period <- list(
  period_type = "currentcm",
  trailing_periods = 12L,
  future_periods = 12L
)

expand_by <- "property"
consider_excluded_units <- 0L

req_filter_params <- list(
  property_group_ids = c(as.character(unlist(unname(property_ids)))),
  period = period,
  expand_by = expand_by,
  consider_excluded_units = consider_excluded_units
)

req_body <- list(
  auth = list(type = 'basic'),
  request_id = request_id,
  method = list(
    name = "getReportData",
    version = "r3",
    params = list(
      reportName = report_name,
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

forecasted_occupancy_resp_data <- queue_resp |>
  httr2::resp_body_json() |>
  purrr::pluck("response", "result", "reportData") |>
  dplyr::bind_rows()


