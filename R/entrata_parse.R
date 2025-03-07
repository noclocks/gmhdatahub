
#  ------------------------------------------------------------------------
#
# Title : Entrata Response Parsers
#    By : Jimmy Briggs
#  Date : 2025-03-03
#
#  ------------------------------------------------------------------------


# reports ---------------------------------------------------------------------------------------------------------


# pre-lease -------------------------------------------------------------------------------------------------------

entrata_resp_parse_pre_lease_by_property <- function(resp, report_date = NULL) {

  check_response(resp)

  # parse out original request
  req <- purrr::pluck(resp, "request")
  check_request(req)

  # set report date
  if (is.null(report_date)) {
    report_date <- lubridate::today()
  }

  # parse out request metadata
  req_meta <- parse_pre_lease_request(req)

  # parse response
  resp_lst <- parse_pre_lease_response(resp)

  # parse response data
  resp_data <- purrr::pluck(resp_lst, "data", "response", "result", "reportData")

  # summary
  summary_data <- resp_data |>
    purrr::pluck("summary") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    process_pre_lease_summary_data()

  # details
  details_data <- resp_data |>
    purrr::pluck("details") |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    process_pre_lease_details_data()

  # create metadata list
  metadata <- list(
    report_date = report_date,
    request = req_meta,
    response = resp_lst
  )

  return(
    list(
      summary_data = summary_data,
      details_data = details_data,
      metadata = metadata
    )
  )
}

parse_pre_lease_request <- function(req) {

  check_request(req)

  req_url <- req |> purrr::pluck("url")
  req_endpoint <- basename(req_url)
  req_headers <- req |> purrr::pluck("headers") |> httr2:::headers_redact()
  req_policies <- req |> purrr::pluck("policies")
  req_options <- req |> purrr::pluck("options")
  req_body <- req |> purrr::pluck("body")
  req_content_type <- req_body |> purrr::pluck("content_type")
  req_data <- req_body |> purrr::pluck("data")
  req_id <- req_data |> purrr::pluck("request_id")
  req_method_name <- req_data |> purrr::pluck("method", "name")
  req_method_version <- req_data |> purrr::pluck("method", "version")
  req_method_params <- req_data |> purrr::pluck("method", "params")
  req_queue_id <- req_method_params |> purrr::pluck("queueId")
  req_service_name <- req_method_params |> purrr::pluck("serviceName")

  req_json <- req_body |> jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  list(
    request_id = req_id,
    url = req_url,
    endpoint = req_endpoint,
    method_name = req_method_name,
    method_version = req_method_version,
    method_params = req_method_params,
    content_type = req_content_type,
    headers = req_headers,
    policies = req_policies,
    options = req_options,
    service_name = req_service_name,
    queue_id = req_queue_id,
    json = req_json
  )

}

parse_pre_lease_response <- function(resp) {

  check_response(resp)

  # parse response
  resp_status_code <- resp |> purrr::pluck("status_code")
  resp_method <- resp |> purrr::pluck("method")
  resp_url <- resp |> purrr::pluck("url")
  resp_headers <- resp |> purrr::pluck("headers") |> httr2:::headers_redact()

  # parse respone data / JSON
  resp_data <- resp |> httr2::resp_body_json()
  resp_json <- resp_data |> jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  # parse out status code & queue metadata
  status_code <- resp_data |> purrr::pluck("response", "code")
  queue_start <- resp_data |> purrr::pluck("response", "result", "queueStartedOn") |> lubridate::mdy_hms()
  queue_end <- resp_data |> purrr::pluck("response", "result", "queueCompletedOn") |> lubridate::mdy_hms()
  queue_duration <- as.integer(difftime(queue_end, queue_start, units = "secs"))

  list(
    status_code = status_code,
    method = resp_method,
    url = resp_url,
    headers = resp_headers,
    json = resp_json,
    queue_start = queue_start,
    queue_end = queue_end,
    queue_duration = queue_duration,
    data = resp_data,
    json = resp_json
  )

}

process_pre_lease_summary_data <- function(summary_data, report_date = Sys.Date()) {

  int_cols <- function() {
    c(
      tidyselect::all_of(c("property_id", "units", "variance")),
      tidyselect::ends_with("_count"),
      tidyselect::ends_with("_count_prior"),
      tidyselect::starts_with("number_")
    )
  }

  num_cols <- function() {
    c(
      tidyselect::starts_with("avg_"),
      tidyselect::ends_with("_percent"),
      tidyselect::ends_with("_percent_prior"),
      tidyselect::ends_with("_total"),
      tidyselect::ends_with("_rent"),
      tidyselect::ends_with("_rent_total")
    )
  }

  summary_data |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      dplyr::across(int_cols(), function(n) dplyr::coalesce(as.integer(n), 0L)),
      dplyr::across(num_cols(), function(n) dplyr::coalesce(as.numeric(n), 0.00))
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
      "total_scheduled_rent" = "scheduled_rent_total",
      tidyselect::starts_with("avg_"),
      tidyselect::starts_with("started_"),
      tidyselect::starts_with("partially_completed_"),
      tidyselect::starts_with("completed_"),
      tidyselect::starts_with("approved_"),
      tidyselect::starts_with("preleased_"),
      "yoy_variance" = "variance"
    ) |>
    dplyr::distinct(
      .data$report_date,
      .data$property_id,
      .data$property_name,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(.data$property_name)

}

transform_pre_lease_summary_data <- function(summary_data_processed) {

  validate_col_names(
    summary_data_processed,
    req_cols = c(
      "report_date",
      "property_id",
      "property_name",
      "excluded_unit_count",
      "rentable_unit_count",
      "occupied_unit_count",
      "available_unit_count",
      "approved_new_count",
      "partially_completed_new_count",
      "completed_new_count",
      "approved_renewal_count",
      "partially_completed_renewal_count",
      "completed_renewal_count",
      "approved_new_count_prior",
      "partially_completed_new_count_prior",
      "completed_new_count_prior",
      "approved_renewal_count_prior",
      "partially_completed_renewal_count_prior",
      "completed_renewal_count_prior"
    ),
    optional_cols = c(
      "unit_type"
    )
  )

  rept_date <- summary_data_processed |> dplyr::pull(report_date) |> unique()
  weeks_left_to_lease <- get_weeks_left_to_lease(rept_date)

  summary_data_processed |>
    dplyr::transmute(
      report_date = .data$report_date,
      property_id = .data$property_id,
      property_name = .data$property_name,
      dplyr::across(tidyselect::any_of(c("unit_type", "floorplan_name")), ~.x, .names = "{.col}"),
      excluded_units = .data$excluded_unit_count,
      rentable_units = .data$rentable_unit_count,
      occupied_units = .data$occupied_unit_count,
      available_units = .data$available_unit_count,
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
        function(x) dplyr::if_else(is.na(x), as(x, class(x)), x)
      )
    )

}


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

  coalesce_cols <- function() {
    c(
      tidyselect::any_of(c("bldg_unit", "unit", "unit_type", "floorplan_name", "bldg", "charge_code"))
    )
  }

  details_data |>
    dplyr::mutate(
      report_date = as.Date(.env$report_date),
      dplyr::across(coalesce_cols(), function(x) dplyr::coalesce(x, "Missing")),
      dplyr::across(date_cols(), lubridate::mdy),
      dplyr::across(int_cols(), function(n) dplyr::coalesce(as.integer(n), 0L)),
      dplyr::across(num_cols(), function(n) dplyr::coalesce(as.numeric(n), 0.00))
    ) |>
    dplyr::select(
      "report_date",
      "property_id",
      "property_name",
      tidyselect::any_of(c("bldg_unit", "unit_type", "charge_code")),
      tidyselect::any_of(c("bldg", "unit", "unit_status", "floorplan_name")),
      "resident_id",
      "resident_name" = "resident",
      "resident_email" = "email",
      "resident_phone" = "phone_number",
      "resident_gender" = "gender",
      tidyselect::any_of(c("student_id_number")),
      "lease_id" = "lease_id_display",
      "lease_status",
      "lease_sub_status",
      "lease_occupancy_type",
      "lease_term_name",
      "lease_term_month" = "lease_term",
      tidyselect::any_of(c("space_option", "leasing_agent", "ledger_name")),
      "lease_start_date" = "lease_start",
      "lease_end_date" = "lease_end",
      "lease_started_on_date" = "lease_started_on",
      "lease_partially_completed_on_date" = "lease_partially_completed_on",
      "lease_completed_on_date" = "lease_completed_on",
      "lease_approved_on_date" = "lease_approved_on",
      "move_in_date",
      tidyselect::starts_with("deposit_"),
      tidyselect::ends_with("_rent"),
      tidyselect::ends_with("_rate"),
      tidyselect::ends_with("_charges"),
      tidyselect::ends_with("_total")
    ) |>
    dplyr::distinct(
      .data$report_date,
      .data$property_id,
      .data$property_name,
      .data$unit_type,
      .data$bldg_unit,
      .data$charge_code,
      .data$resident_id,
      .data$lease_id,
      .keep_all = TRUE
    ) |>
    dplyr::arrange(
      .data$property_name,
      .data$unit_type,
      .data$bldg_unit,
      .data$charge_code
    )

}


# lease execution -------------------------------------------------------------------------------------------------



# properties ------------------------------------------------------------------------------------------------------

#' Parse Entrata Properties Response
#'
#' @name entrata_resp_parse_properties
#'
#' @description
#' These functions parse the response data from the Entrata API `/properties` endpoint's
#' `getProperties` method into various tibbles containing the response's property-level
#' information and data.
#'
#' The primary parsing function is `entrata_resp_parse_properties()` which
#' calls the other parsing functions to extract the various property data components.
#'
#' @details
#' The `entrata_resp_parse_properties()` function is the main parsing function that
#' calls the other parsing functions to extract the various property data components.
#'
#' The other parsing functions are as follows:
#'
#' - `entrata_resp_parse_properties_tbl()` - Parse the base property data
#' - `entrata_resp_parse_properties_addresses_tbl()` - Parse property address data
#' - `entrata_resp_parse_properties_post_months_tbl()` - Parse property post months
#' - `entrata_resp_parse_properties_hours_tbl()` - Parse property hours
#' - `entrata_resp_parse_properties_space_options_tbl()` - Parse property space options
#' - `entrata_resp_parse_properties_lease_terms()` - Parse property lease terms
#' - `entrata_resp_parse_properties_lease_term_windows()` - Parse property lease term windows
#'
#' These functions all use the `resp_data` tibble as the initial data source, where
#' `resp_data` is generated from the following operations on the extracted, nested JSON data:
#'
#' ```R
#' resp_data <- resp |>
#'   httr2::resp_body_json() |>
#'   purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
#'   jsonlite::toJSON(auto_unbox = TRUE) |>
#'   jsonlite::fromJSON(flatten = TRUE) |>
#'   tibble::as_tibble() |>
#'   janitor::clean_names()
#' ```
#'
#' which flattens the nested JSON data into a tibble ready to be parsed into
#' the various property data components.
#'
#' @param resp The response object from the Entrata API `/properties` endpoint. Must be a valid
#'   [httr2::response()] object.
#' @param resp_data The initial data tibble containing the extracted, flattened JSON data
#'   from the response object. This argument is only used by the second-level parsing functions.
#'
#' @returns
#' `entrata_resp_parse_properties()` returns a list containing the following tibbles:
#'
#' - `properties` - Base property data
#' - `property_addresses` - Property address data
#' - `property_post_months` - Property post months
#' - `property_hours` - Property hours
#' - `property_space_options` - Property space options
#' - `property_lease_terms` - Property lease terms
#' - `property_lease_term_windows` - Property lease term windows
#'
#' `entrata_resp_parse_properties_tbl()`: A tibble containing the base property data
#'
#' `entrata_resp_parse_properties_addresses_tbl()`: A tibble containing the property address data
#'
#' `entrata_resp_parse_properties_post_months_tbl()`: A tibble containing the property post months
#'
#' `entrata_resp_parse_properties_hours_tbl()`: A tibble containing the property hours
#'
#' `entrata_resp_parse_properties_space_options_tbl()`: A tibble containing the property space options
#'
#' `entrata_resp_parse_properties_lease_terms_tbl()`: A tibble containing the property lease terms
#'
#' `entrata_resp_parse_properties_lease_term_windows_tbl()`: A tibble containing the property lease term windows
#'
#' @export
#'
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom tibble as_tibble
#' @importFrom janitor clean_names
#'
#' @examples
#' \dontrun{
#' # create and perform the API request and get response
#' req <- entrata_req_properties()
#' resp <- entrata_req_perform(req)
#'
#' # parse the response data into tibbles
#' resp_parsed <- entrata_resp_parse_properties(resp)
#'
#' # extract the property data tibble
#' properties_tbl <- resp_parsed$properties
#'
#' # extract the property address data tibble
#' property_addresses_tbl <- resp_parsed$property_addresses
#'
#' # etc.
#' }
entrata_resp_parse_properties <- function(resp) {

  check_response(resp)

  resp_json <- resp |> httr2::resp_body_json()

  resp_data <- resp_json |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    janitor::clean_names()

  resp_properties_tbl <- resp_data |>
    entrata_resp_parse_properties_tbl()

  resp_addresses_tbl <- resp_data |>
    entrata_resp_parse_properties_addresses_tbl()

  resp_post_months_tbl <- resp_data |>
    entrata_resp_parse_properties_post_months_tbl()

  resp_hours_tbl <- resp_data |>
    entrata_resp_parse_properties_hours_tbl()

  resp_space_options_tbl <- resp_data |>
    entrata_resp_parse_properties_space_options_tbl()

  resp_lease_terms_tbl <- resp_data |>
    entrata_resp_parse_properties_lease_terms_tbl()

  resp_lease_term_windows_tbl <- resp_data |>
    entrata_resp_parse_properties_lease_term_windows_tbl()

  list(
    properties = resp_properties_tbl,
    property_addresses = resp_addresses_tbl,
    property_post_months = resp_post_months_tbl,
    property_hours = resp_hours_tbl,
    property_space_options = resp_space_options_tbl,
    property_lease_terms = resp_lease_terms_tbl,
    property_lease_term_windows = resp_lease_term_windows_tbl
  )
}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dplyr transmute
#' @importFrom glue glue
entrata_resp_parse_properties_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "marketing_name",
      "type",
      "web_site",
      "address_email",
      "phone_phone_number",
      "address_address",
      "address_city",
      "address_state",
      "address_postal_code",
      "is_disabled",
      "is_featured_property",
      "parent_property_id",
      "year_built",
      "short_description",
      "long_description"
    )
  )

  resp_data |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      property_name = as.character(.data$marketing_name),
      property_type = as.character(.data$type),
      property_website = as.character(.data$web_site),
      property_email = as.character(.data$address_email),
      property_phone = as.character(.data$phone_phone_number),
      property_address = as.character(
        glue::glue(
          "{.data$address_address} {.data$address_city}, {.data$address_state} {.data$address_postal_code}"
        )
      ),
      is_disabled = as.logical(.data$is_disabled),
      is_featured = as.logical(.data$is_featured_property),
      parent_property_id = as.integer(.data$parent_property_id),
      year_built = as.integer(.data$year_built),
      short_description = as.character(.data$short_description),
      long_description = as.character(.data$long_description)
    )
}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dplyr transmute arrange select distinct bind_rows
#' @importFrom glue glue
#' @importFrom janitor clean_names
#' @importFrom tidyr unnest
entrata_resp_parse_properties_addresses_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "address_address",
      "address_city",
      "address_state",
      "address_postal_code",
      "addresses_address"
    )
  )

  property_addresses <- resp_data |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      address_type = "Property",
      address_full = as.character(
        glue::glue(
          "{.data$address_address} {.data$address_city}, {.data$address_state} {.data$address_postal_code}"
        )
      ),
      address_street = as.character(.data$address_address),
      address_city = as.character(.data$address_city),
      address_state = as.character(.data$address_state),
      address_postal_code = as.character(.data$address_postal_code)
    )

  property_primary_mailing_addresses <- resp_data |>
    dplyr::select(
      property_id,
      addresses_address
    ) |>
    tidyr::unnest(addresses_address) |>
    janitor::clean_names() |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      address_type = as.character(.data$address_type),
      address_full = as.character(
        glue::glue(
          "{.data$address} {.data$city}, {.data$state_code} {.data$postal_code}"
        )
      ),
      address_street = as.character(.data$address),
      address_city = as.character(.data$city),
      address_state = as.character(.data$state_code),
      address_postal_code = as.character(.data$postal_code)
    ) |>
    dplyr::arrange(
      .data$property_id,
      .data$address_type
    )

  dplyr::bind_rows(
    property_addresses,
    property_primary_mailing_addresses
  ) |>
    dplyr::distinct() |>
    dplyr::arrange(
      .data$property_id,
      .data$address_type
    )

}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dint date_ym
#' @importFrom dplyr select transmute case_when
#' @importFrom lubridate make_date
#' @importFrom stringr str_sub
#' @importFrom tidyr pivot_longer
entrata_resp_parse_properties_post_months_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "post_months_ar_post_month",
      "post_months_ap_post_month",
      "post_months_gl_post_month"
    )
  )

  resp_data |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      ar_post_month = as.character(.data$post_months_ar_post_month),
      ap_post_month = as.character(.data$post_months_ap_post_month),
      gl_post_month = as.character(.data$post_months_gl_post_month)
    ) |>
    tidyr::pivot_longer(
      cols = c("ar_post_month", "ap_post_month", "gl_post_month"),
      names_to = "post_month_type",
      values_to = "post_month"
    ) |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      post_month_type = dplyr::case_when(
        .data$post_month_type == "ar_post_month" ~ "Accounts Receivable (AR)",
        .data$post_month_type == "ap_post_month" ~ "Accounts Payable (AP)",
        .data$post_month_type == "gl_post_month" ~ "General Ledger (GL)"
      ),
      post_month_year = as.integer(stringr::str_sub(.data$post_month, 4, 7)),
      post_month_month = as.integer(stringr::str_sub(.data$post_month, 1, 2)),
      post_month_date = lubridate::make_date(year = .data$post_month_year, month = .data$post_month_month, day = 1L),
      post_month = dint::date_ym(.data$post_month_year, .data$post_month_month) |> dint:::format.date_ym(format = "%Y-%m")
    ) |>
    dplyr::select(
      property_id,
      post_month_type,
      post_month,
      post_month_date
    )

}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dplyr transmute select case_when arrange bind_rows
#' @importFrom janitor clean_names
#' @importFrom tidyr unnest
entrata_resp_parse_properties_hours_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "property_hours_office_hours_office_hour",
      "property_hours_pool_hours_pool_hour"
    )
  )

  office_hours <- resp_data |>
    dplyr::select(
      property_id,
      property_hours_office_hours_office_hour
    ) |>
    tidyr::unnest(
      cols = c("property_hours_office_hours_office_hour")
    ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      hours_type = "Office",
      day_of_week = as.character(.data$day),
      open_time = ifelse(
        is.na(.data$open_time),
        .data$availability_type,
        as.character(.data$open_time)
      ),
      close_time = ifelse(
        is.na(.data$close_time),
        .data$availability_type,
        as.character(.data$close_time)
      ),
      interval = ifelse(
        is.na(.data$open_time) | is.na(.data$close_time),
        .data$availability_type,
        paste0(
          .data$open_time,
          " - ",
          .data$close_time
        )
      ),
      order = dplyr::case_when(
        .data$day == "Monday" ~ 1,
        .data$day == "Tuesday" ~ 2,
        .data$day == "Wednesday" ~ 3,
        .data$day == "Thursday" ~ 4,
        .data$day == "Friday" ~ 5,
        .data$day == "Saturday" ~ 6,
        .data$day == "Sunday" ~ 7
      )
    )

  pool_hours <- resp_data |>
    dplyr::select(
      "property_id",
      "property_hours_pool_hours_pool_hour"
    ) |>
    tidyr::unnest(
      cols = c("property_hours_pool_hours_pool_hour")
    ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      hours_type = "Pool",
      day_of_week = as.character(.data$day),
      open_time = ifelse(
        is.na(.data$open_time),
        .data$availability_type,
        as.character(.data$open_time)
      ),
      close_time = ifelse(
        is.na(.data$close_time),
        .data$availability_type,
        as.character(.data$close_time)
      ),
      interval = ifelse(
        is.na(.data$open_time) | is.na(.data$close_time),
        .data$availability_type,
        paste0(
          .data$open_time,
          " - ",
          .data$close_time
        )
      ),
      order = dplyr::case_when(
        .data$day == "Monday" ~ 1,
        .data$day == "Tuesday" ~ 2,
        .data$day == "Wednesday" ~ 3,
        .data$day == "Thursday" ~ 4,
        .data$day == "Friday" ~ 5,
        .data$day == "Saturday" ~ 6,
        .data$day == "Sunday" ~ 7
      )
    )

  dplyr::bind_rows(
    office_hours,
    pool_hours
  ) |>
    dplyr::arrange(
      .data$property_id,
      .data$hours_type,
      .data$order
    ) |>
    dplyr::select(
      "property_id",
      "hours_type",
      "day_of_week",
      "open_time",
      "close_time",
      "interval"
    )

}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dplyr arrange transmute select
#' @importFrom janitor clean_names
#' @importFrom tidyr unnest
entrata_resp_parse_properties_space_options_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "space_options_space_option"
    )
  )

  resp_data |>
    dplyr::select(
      "property_id",
      "space_options_space_option"
    ) |>
    tidyr::unnest(
      cols = c("space_options_space_option")
    ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      space_option_id = as.integer(.data$id),
      space_option_name = as.character(.data$name)
    ) |>
    dplyr::arrange(
      .data$property_id,
      .data$space_option_id
    )

}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dplyr arrange transmute select
#' @importFrom janitor clean_names
#' @importFrom tidyr unnest
entrata_resp_parse_properties_lease_terms_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "lease_terms_lease_term"
    )
  )

  resp_data |>
    dplyr::select(
      "property_id",
      "lease_terms_lease_term"
    ) |>
    tidyr::unnest(
      cols = c("lease_terms_lease_term")
    ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      lease_term_id = as.integer(.data$id),
      lease_term_name = as.character(.data$name),
      lease_term_duration_months = as.integer(.data$term_months),
      is_prospect = as.logical(.data$is_prospect),
      is_renewal = as.logical(.data$is_renewal)
    ) |>
    dplyr::arrange(
      .data$property_id,
      .data$lease_term_id
    )
}

#' @rdname entrata_resp_parse_properties
#' @export
#' @importFrom dplyr arrange transmute select
#' @importFrom janitor clean_names
#' @importFrom lubridate mdy
#' @importFrom tidyr unnest
entrata_resp_parse_properties_lease_term_windows_tbl <- function(resp_data) {

  validate_col_names(
    resp_data,
    c(
      "property_id",
      "lease_terms_lease_term"
    )
  )

  resp_data |>
    dplyr::select(
      "property_id",
      "lease_terms_lease_term"
    ) |>
    tidyr::unnest(
      cols = c("lease_terms_lease_term")
    ) |>
    janitor::clean_names() |>
    dplyr::select(
      "property_id",
      "lease_term_id" = "id",
      "lease_start_windows" = "lease_start_windows_lease_start_window"
    ) |>
    tidyr::unnest(
      cols = c("lease_start_windows")
    ) |>
    janitor::clean_names() |>
    dplyr::transmute(
      property_id = as.integer(.data$property_id),
      lease_term_id = as.integer(.data$lease_term_id),
      window_id = as.integer(.data$id),
      window_start_date = lubridate::mdy(.data$window_start_date),
      window_end_date = lubridate::mdy(.data$window_end_date)
    ) |>
    dplyr::arrange(
      .data$property_id,
      .data$lease_term_id,
      .data$window_id
    )

}

# floorplans ------------------------------------------------------------------------------------------------------

#' Parse Entrata Floor Plans Response
#'
#' @description
#' This function parses the response data from the Entrata API `/properties` endpoint's
#' `getFloorPlans` method into various tibbles containing the response's floor plan-level
#' information and data.
#'
#' @param resp The Entrata API response object.
#'
#' @returns
#' A tibble containing the parsed floor plan data.
#'
#' @export
#'
#' @importFrom purrr pluck map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr left_join
#' @importFrom janitor clean_names
#' @importFrom tidyr pivot_wider
entrata_resp_parse_floorplans <- function(resp) {

  check_response(resp)

  resp_floor_plans <- entrata_resp_body(resp) |>
    purrr::pluck("response", "result", "FloorPlans", "FloorPlan")

  # extract floor plans table
  floor_plans_tbl <- purrr::map_dfr(
    resp_floor_plans,
    function(x) {
      tibble::tibble(
        property_id = purrr::pluck(x, "PropertyId"),
        floorplan_id = purrr::pluck(x, "Identification", "IDValue"),
        floorplan_name = purrr::pluck(x, "Name"),
        total_units_count = purrr::pluck(x, "UnitCount") |> as.integer(),
        available_units_count = purrr::pluck(x, "UnitsAvailable") |> as.integer(),
        is_disabled = purrr::pluck(x, "IsDisabled")
      )
    }
  )

  # extract unit types table
  unit_types_tbl <- purrr::map_dfr(
    resp_floor_plans,
    ~ {
      property_id <- purrr::pluck(.x, "PropertyId")
      floorplan_id <- purrr::pluck(.x, "Identification", "IDValue")
      purrr::map_dfr(
        purrr::pluck(.x, "UnitTypes", "UnitType"),
        ~ {
          tibble::tibble(
            property_id = property_id,
            floorplan_id = floorplan_id,
            unit_type_id = purrr::pluck(.x, "@attributes", "Id") |> as.integer(),
            unit_type_name = purrr::pluck(.x, "@value")
          )
        }
      )
    }
  )

  # extract rooms table
  rooms_tbl <- purrr::map_dfr(
    resp_floor_plans,
    ~ {
      floorplan_id <- purrr::pluck(.x, "Identification", "IDValue")
      property_id <- purrr::pluck(.x, "PropertyId")
      purrr::map_dfr(
        purrr::pluck(.x, "Room"),
        ~ {
          tibble::tibble(
            property_id = property_id,
            floorplan_id = floorplan_id,
            room_type = purrr::pluck(.x, "@attributes", "RoomType"),
            room_count = purrr::pluck(.x, "Count") |> as.integer()
          )
        }
      )
    }
  ) |>
    # tidy by spreading the room counts by room type and label as:
    # number_of_bedrooms and number_of_bathrooms
    tidyr::pivot_wider(
      names_from = room_type,
      values_from = room_count,
      names_prefix = "number_of_"
    ) |>
    janitor::clean_names()

  # extract square feet table
  square_feet_tbl <- purrr::map_dfr(
    resp_floor_plans,
    ~ {
      tibble::tibble(
        property_id = purrr::pluck(.x, "PropertyId"),
        floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
        min_square_feet = purrr::pluck(.x, "SquareFeet", "@attributes", "Min") |> as.numeric(),
        max_square_feet = purrr::pluck(.x, "SquareFeet", "@attributes", "Max") |> as.numeric()
      )
    }
  ) |>
    dplyr::mutate(
      avg_square_feet = (min_square_feet + max_square_feet) / 2
    )

  # extract market rent table
  market_rent_tbl <- purrr::map_dfr(
    resp_floor_plans,
    ~ {
      tibble::tibble(
        property_id = purrr::pluck(.x, "PropertyId"),
        floorplan_id = purrr::pluck(.x, "Identification", "IDValue"),
        min_market_rent = purrr::pluck(.x, "MarketRent", "@attributes", "Min") |> as.numeric(),
        max_market_rent = purrr::pluck(.x, "MarketRent", "@attributes", "Max") |> as.numeric()
      )
    }
  ) |>
    dplyr::mutate(
      avg_market_rent = (min_market_rent + max_market_rent) / 2
    )

  # extract files table
  images_tbl <- purrr::map_dfr(
    resp_floor_plans,
    ~ {
      property_id <- purrr::pluck(.x, "PropertyId")
      floorplan_id <- purrr::pluck(.x, "Identification", "IDValue")
      purrr::map_dfr(
        purrr::pluck(.x, "File"),
        ~ {
          tibble::tibble(
            property_id = property_id,
            floorplan_id = floorplan_id,
            image_type = purrr::pluck(.x, "FileType"),
            image_file_id = purrr::pluck(.x, "@attributes", "FileID") |> as.integer(),
            image_url = purrr::pluck(.x, "Src"),
            image_caption = purrr::pluck(.x, "Caption"),
            image_description = purrr::pluck(.x, "Description"),
            image_format = purrr::pluck(.x, "Format"),
            image_width = purrr::pluck(.x, "Width") |> as.integer(),
            image_height = purrr::pluck(.x, "Height") |> as.integer(),
            image_is_default = purrr::pluck(.x, "IsDefault") |> as.logical(),
            image_is_active = purrr::pluck(.x, "@attributes", "Active") |> as.logical()
          )
        }
      )
    }
  )

  # merge & return
  floor_plans_tbl |>
    dplyr::left_join(
      unit_types_tbl,
      by = c("property_id", "floorplan_id")
    ) |>
    dplyr::left_join(
      rooms_tbl,
      by = c("property_id", "floorplan_id")
    ) |>
    dplyr::left_join(
      square_feet_tbl,
      by = c("property_id", "floorplan_id")
    ) |>
    dplyr::left_join(
      market_rent_tbl,
      by = c("property_id", "floorplan_id")
    ) |>
    dplyr::left_join(
      images_tbl,
      by = c("property_id", "floorplan_id")
    )

}

# A/R codes / charge codes --------------------------------------------------

#' Parse Entrata A/R Codes Response
#'
#' @description
#' This function parses the response data from the Entrata API `/arcodes` endpoint's `getArCodes` method.
#'
#' @param resp The Entrata API response object.
#'
#' @returns
#' A tibble containing the parsed and merged A/R codes data.
#'
#' @export
#'
#' @importFrom dplyr select bind_rows
#' @importFrom purrr pluck
entrata_resp_parse_arcodes <- function(resp) {

  check_response(resp)

  resp_json <- entrata_resp_body(resp)

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
