parse_properties_response <- function(res_data) {

  # extract base properties tibble
  property_tbl_init <- res_data |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble()

  # extract base properties tibble
  properties_tbl_base <- parse_property_base_data(property_tbl_init)

  # parse nested address data (primary/main property address + mailing sub-addresses):
  property_addresses <- parse_property_address_data(property_tbl_init)

  # parse nested post months
  property_post_months <- parse_property_post_months(property_tbl_init)

  # parse nested property hours (office/pool)
  property_hours <- parse_property_hours(property_tbl_init)

  # parse nested space options
  property_space_options <- parse_property_space_options(property_tbl_init)

  # parse nested lease terms
  property_lease_term_windows <- parse_property_lease_term_windows(property_tbl_init)

  # parse nested phone data
  property_phones <- parse_property_phones(property_tbl_init)

  # parse nested custom keys data
  property_custom_keys <- parse_property_custom_keys(property_tbl_init)

  list(
    property_tbl_base = properties_tbl_base,
    property_addresses = property_addresses,
    property_post_months = property_post_months,
    property_hours = property_hours,
    property_space_options = property_space_options,
    property_lease_term_windows = property_lease_term_windows,
    property_phones = property_phones,
    property_custom_keys = property_custom_keys
  )

}

#' Entrata Property Parsers
#'
#' @name entrata_property_parsers
#'
#' @description
#' These functions parse the response data from the Entrata API `/properties` endpoint's
#' `getProperties` method into various tibbles containing the response's property-level
#' information and data.
#'
#' @details
#' These functions all use the `property_tbl_init` tibble as the initial data source
#' for parsing the various property data components.
#'
#' This tibble is generated from performing the following operations on the
#' extracted, nested JSON data from the response data:
#'
#' ```R
#' property_tbl_init <- res_data |>
#'   jsonlite::toJSON(auto_unbox = TRUE) |>
#'   jsonlite::fromJSON(flatten = TRUE) |>
#'   tibble::as_tibble()
#' ```
#'
#' which flattens the nested JSON data into a tibble ready to be parsed.
#'
#' @section Functions:
#' - `parse_property_base_data()` - Parse base property data
#' - `parse_property_address_data()` - Parse property address data
#' - `parse_property_post_months()` - Parse property post months
#' - `parse_property_hours()` - Parse property hours
#' - `parse_property_space_options()` - Parse property space options
#' - `parse_property_lease_term_windows()` - Parse property lease term windows
#' - `parse_property_phones()` - Parse property phone numbers
#' - `parse_property_custom_keys()` - Parse property custom keys
#'
#' @param property_tbl_init Initial property data tibble (see details).
#'
#' @seealso [parse_properties_response()] for the main parsing function.
#'
#' @return A tibble containing the parsed property data.
NULL

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dplyr select rename
#' @importFrom purrr keep
#' @importFrom cli cli_alert_warning
#' @importFrom tidyselect all_of any_of
parse_property_base_data <- function(property_tbl_init) {

  # determine which columns are available to extract
  expected_cols <- c(
    "PropertyID",
    "MarketingName",
    "Type",
    "webSite",
    "ShortDescription",
    "LongDescription",
    "IsDisabled",
    "IsFeaturedProperty",
    "ParentPropertyID",
    "YearBuilt"
  )

  select_cols <- intersect(
    expected_cols,
    colnames(property_tbl_init)
  )

  excluded_cols <- setdiff(
    expected_cols,
    colnames(property_tbl_init)
  )

  if (length(excluded_cols) > 0) {
    cli::cli_alert_warning(
      c(
        "The following columns are not available in the property data: ",
        "{.field {paste(excluded_cols, collapse = ', ')}}"
      )
    )
  }

  rename_vec <- c(
    "property_id" = "PropertyID",
    "property_name" = "MarketingName",
    "property_type" = "Type",
    "property_website" = "webSite",
    "property_short_description" = "ShortDescription",
    "property_long_description" = "LongDescription",
    "property_is_disabled" = "IsDisabled",
    "property_is_featured" = "IsFeaturedProperty",
    "parent_property_id" = "ParentPropertyID",
    "property_year_built" = "YearBuilt"
  ) |>
    purrr::keep(~ .x %in% select_cols)

  property_tbl_init |>
    dplyr::select(tidyselect::any_of(expected_cols)) |>
    dplyr::rename(tidyselect::all_of(rename_vec))

}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dplyr select filter mutate left_join
#' @importFrom tidyr unnest
parse_property_address_data <- function(property_tbl_init) {

  property_addresses_init <- property_tbl_init |>
    dplyr::select(
      property_id = "PropertyID",
      primary_address_street = "Address.Address",
      primary_address_city = "Address.City",
      primary_address_state = "Address.State",
      primary_address_zip = "Address.PostalCode",
      primary_address_country = "Address.Country",
      primary_address_email = "Address.Email",
      primary_address_type = "Address.@attributes.AddressType",
      sub_addresses = "Addresses.Address"
    ) |>
    tidyr::unnest(cols = c("sub_addresses")) |>
    tidyr::unnest(cols = c("Address")) |>
    dplyr::select(
      property_id,
      primary_address_street:primary_address_type,
      sub_address_type = "AddressType",
      sub_address_street = "Address",
      sub_address_city = "City",
      sub_address_state = "StateCode",
      sub_address_zip = "PostalCode",
      sub_address_country = "Country"
    )

  property_mailing_addresses <- property_addresses_init |>
    dplyr::filter(
      .data$primary_address_type == "property",
      .data$sub_address_type == "Mailing"
    ) |>
    dplyr::select(
      "property_id",
      mailing_address_street = sub_address_street,
      mailing_address_city = sub_address_city,
      mailing_address_state = sub_address_state,
      mailing_address_zip = sub_address_zip,
      mailing_address_country = sub_address_country
    )

  property_primary_addresses <- property_addresses_init |>
    dplyr::filter(
      .data$primary_address_type == "property",
      .data$sub_address_type == "Primary"
    ) |>
    dplyr::select(
      "property_id",
      "primary_address_street",
      "primary_address_city",
      "primary_address_state",
      "primary_address_zip",
      "primary_address_country",
      "primary_address_email"
    )

  property_primary_addresses |>
    dplyr::left_join(
      property_mailing_addresses,
      by = "property_id"
    ) |>
    dplyr::mutate(
      primary_address_full = ifelse(
        is.na(primary_address_street),
        NA_character_,
        paste0(
          .data$primary_address_street,
          ", ",
          .data$primary_address_city,
          ", ",
          .data$primary_address_state,
          " ",
          .data$primary_address_zip,
          ", ",
          .data$primary_address_country
        )
      ),
      mailing_address_full = ifelse(
        is.na(.data$mailing_address_street),
        NA_character_,
        paste0(
          .data$mailing_address_street,
          ", ",
          .data$mailing_address_city,
          ", ",
          .data$mailing_address_state,
          " ",
          .data$mailing_address_zip,
          ", ",
          .data$mailing_address_country
        )
      )
    ) |>
    dplyr::select(
      "property_id",
      "primary_address_full",
      "primary_address_street",
      "primary_address_city",
      "primary_address_state",
      "primary_address_zip",
      "primary_address_country",
      "primary_address_email",
      "mailing_address_full",
      "mailing_address_street",
      "mailing_address_city",
      "mailing_address_state",
      "mailing_address_zip",
      "mailing_address_country"
    )
}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dint date_ym is_date_ym
#' @importFrom dplyr mutate_if select mutate
#' @importFrom stringr str_sub
parse_property_post_months <- function(property_tbl_init) {
  property_tbl_init |>
    dplyr::select(
      property_id = "PropertyID",
      ar_post_month = "PostMonths.ArPostMonth",
      ap_post_month = "PostMonths.ApPostMonth",
      gl_post_month = "PostMonths.GlPostMonth"
    ) |>
    dplyr::mutate(
      ar_post_month_year = as.integer(stringr::str_sub(.data$ar_post_month, 4, 7)),
      ar_post_month_month = as.integer(stringr::str_sub(.data$ar_post_month, 1, 2)),
      ap_post_month_year = as.integer(stringr::str_sub(.data$ap_post_month, 4, 7)),
      ap_post_month_month = as.integer(stringr::str_sub(.data$ap_post_month, 1, 2)),
      gl_post_month_year = as.integer(stringr::str_sub(.data$gl_post_month, 4, 7)),
      gl_post_month_month = as.integer(stringr::str_sub(.data$gl_post_month, 1, 2)),
      ar_post_month = dint::date_ym(.data$ar_post_month_year, .data$ar_post_month_month),
      ap_post_month = dint::date_ym(.data$ap_post_month_year, .data$ap_post_month_month),
      gl_post_month = dint::date_ym(.data$gl_post_month_year, .data$gl_post_month_month)
    ) |>
    dplyr::select(
      property_id,
      ar_post_month,
      ap_post_month,
      gl_post_month
    ) |>
    dplyr::mutate_if(
      dint::is_date_ym,
      dint:::format.date_ym,
      format = "%Y-%m"
    )
}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dplyr select mutate arrange left_join join_by
#' @importFrom tidyr unnest
parse_property_hours <- function(property_tbl_init) {

  property_office_hours <- property_tbl_init |>
    dplyr::select(
      "PropertyID",
      office_hours = "PropertyHours.OfficeHours.OfficeHour"
    ) |>
    tidyr::unnest(cols = c("office_hours")) |>
    dplyr::select(
      property_id = "PropertyID",
      office_hours_day = "Day",
      office_hours_availability_type = "AvailabilityType",
      office_hours_open_time = "OpenTime",
      office_hours_close_time = "CloseTime"
    ) |>
    dplyr::mutate(
      office_hours_interval = ifelse(
        is.na(.data$office_hours_open_time) | is.na(.data$office_hours_close_time),
        .data$office_hours_availability_type,
        paste0(
          .data$office_hours_open_time,
          " - ",
          .data$office_hours_close_time
        )
      )
    ) |>
    dplyr::select(
      "property_id",
      day_of_week = office_hours_day,
      office_hours = office_hours_interval
    )

  property_pool_hours <- property_tbl_init |>
    dplyr::select(
      "PropertyID",
      pool_hours = "PropertyHours.PoolHours.PoolHour"
    ) |>
    tidyr::unnest(cols = c("pool_hours")) |>
    dplyr::select(
      property_id = "PropertyID",
      pool_hours_day = "Day",
      pool_hours_open_time = "OpenTime",
      pool_hours_close_time = "CloseTime"
    ) |>
    dplyr::mutate(
      pool_hours_interval = ifelse(
        is.na(.data$pool_hours_open_time) | is.na(.data$pool_hours_close_time),
        NA_character_,
        paste0(
          .data$pool_hours_open_time,
          " - ",
          .data$pool_hours_close_time
        )
      )
    ) |>
    dplyr::select(
      "property_id",
      day_of_week = pool_hours_day,
      pool_hours = pool_hours_interval
    )

  # merge into single property_hours table
  property_office_hours |>
    dplyr::left_join(
      property_pool_hours,
      by = dplyr::join_by(
        "property_id",
        "day_of_week"
      )
    ) |>
    dplyr::mutate(
      day_of_week_num = match(
        .data$day_of_week,
        c(
          "Sunday",
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday"
        )
      )
    ) |>
    dplyr::arrange(
      .data$property_id,
      .data$day_of_week_num
    ) |>
    dplyr::select(
      "property_id",
      "day_of_week",
      "office_hours",
      "pool_hours"
    )
}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dplyr mutate select
#' @importFrom tidyr unnest
parse_property_space_options <- function(property_tbl_init) {
  property_tbl_init |>
    dplyr::select(
      "PropertyID",
      space_options = "SpaceOptions.SpaceOption"
    ) |>
    tidyr::unnest(cols = c("space_options")) |>
    dplyr::select(
      property_id = "PropertyID",
      space_option_id = "Id",
      space_option_name = "Name"
    ) |>
    dplyr::mutate(
      space_option_id = as.integer(space_option_id)
    )
}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dplyr mutate select
#' @importFrom lubridate mdy
#' @importFrom tidyr unnest
parse_property_lease_term_windows <- function(property_tbl_init) {

  property_tbl_init |>
    dplyr::select(
      "PropertyID",
      lease_terms = "LeaseTerms.LeaseTerm"
    ) |>
    tidyr::unnest(cols = c("lease_terms")) |>
    dplyr::select(
      "PropertyID",
      lease_term_id = "Id",
      lease_term_name = "Name",
      lease_term_months = "TermMonths",
      lease_term_is_prospect = "IsProspect",
      lease_term_is_renewal = "IsRenewal",
      "LeaseStartWindows.LeaseStartWindow"
    ) |>
    tidyr::unnest(cols = c("LeaseStartWindows.LeaseStartWindow")) |>
    dplyr::select(
      property_id = "PropertyID",
      "lease_term_id",
      lease_term_window_id = "Id",
      "lease_term_name",
      "lease_term_months",
      "lease_term_is_prospect",
      "lease_term_is_renewal",
      lease_term_window_start_date = "WindowStartDate",
      lease_term_window_end_date = "WindowEndDate"
    ) |>
    dplyr::mutate(
      lease_term_window_start_date = lubridate::mdy(lease_term_window_start_date),
      lease_term_window_end_date = lubridate::mdy(lease_term_window_end_date)
    )
}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dialr phone get_type
#' @importFrom dplyr select mutate filter
parse_property_phones <- function(property_tbl_init) {

  property_tbl_init |>
    dplyr::select(
      property_id = "PropertyID",
      phone_number = "Phone.PhoneNumber"#,
      # phone_description = "Phone.PhoneDescription",
      # phone_type = "Phone.@attributes.PhoneType"
    ) |>
    dplyr::filter(
      !is.na(.data$phone_number)
    ) |>
    dplyr::mutate(
      phone_number = dialr::phone(.data$phone_number, region = "US"),
      phone_number_parsed = format(.data$phone_number),
      phone_number_intl = format(.data$phone_number, format = "INTERNATIONAL", clean = FALSE),
      phone_number_link = format(.data$phone_number, format = "RFC3966", clean = TRUE),
      phone_number_link = paste0("tel:", .data$phone_number_link),
      phone_number_link_html = paste0("<a href='", .data$phone_number_link, "'>", .data$phone_number_intl, "</a>"),
      phone_number_type = dialr::get_type(.data$phone_number)
    ) |>
    dplyr::select(
      "property_id",
      phone_number = phone_number_parsed,
      "phone_number_intl",
      "phone_number_link",
      "phone_number_link_html",
      "phone_number_type"
    )
}

#' @rdname entrata_property_parsers
#' @export
#' @importFrom dplyr select
#' @importFrom tidyr unnest
parse_property_custom_keys <- function(property_tbl_init) {

  property_tbl_init |>
    dplyr::select(
      property_id = "PropertyID",
      custom_keys = "CustomKeysData.CustomKeyData"
    ) |>
    tidyr::unnest(cols = c("custom_keys")) |>
    dplyr::select(
      "property_id",
      key = 2,
      value = 3
    )

}
