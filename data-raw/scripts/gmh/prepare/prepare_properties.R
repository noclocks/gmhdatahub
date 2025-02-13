#  ------------------------------------------------------------------------
#
# Title : GMH Properties Preparation
#    By : Jimmy Briggs
#  Date : 2025-01-15
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

# entrata properties
entrata_properties_req <- entrata_properties_request()
entrata_properties_resp <- entrata_req_perform_save(entrata_properties_req)

entrata_config <- get_entrata_config()
resp_properties <- entrata_properties_request() |>
  entrata_req_perform()
resp_properties_json <- resp_properties |> httr2::resp_body_json()

entrata_properties <- resp_properties_json |>
  purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
  purrr::map(function(x) {
    property_id <- purrr::pluck(x, "PropertyID")
    property_name <- purrr::pluck(x, "MarketingName")
    property_type <- purrr::pluck(x, "Type")
    property_website <- purrr::pluck(x, "webSite")
    browser()
    property_is_disabled <- purrr::pluck(x, "IsDisabled")
    property_is_featured <- purrr::pluck(x, "IsFeaturedProperty")

    property_address <- purrr::pluck(x, "Address") |>
      purrr::map(function(y) {
        property_id <- property_id
        browser()
        address_street <- purrr::pluck(y, "Address")
        address_city <- purrr::pluck(y, "City")
        address_state <- purrr::pluck(y, "State")
        address_postal_code <- purrr::pluck(y, "PostalCode")
        address_country <- purrr::pluck(y, "Country")
        list(
          address_type = address_type,
          address_street = address_street,
          address_city = address_city,
          address_state = address_state,
          address_postal_code = address_postal_code,
          address_country = address_country
        )
      })

    out <- list(name = name, id = id)
  }) |>
  purrr::map_dfr(~ tibble::tibble(name = .x$name, id = .x$id))

db_entrata_properties <- db_read_tbl(pool, "entrata.properties")

entrata_property_address_data <- db_read_tbl(pool, "entrata.property_addresses")
entrata_property_phone_data <- db_read_tbl(pool, "entrata.property_phones")
entrata_property_phone_numbers_data <- db_read_tbl(pool, "entrata.property_phone_numbers")
# entrata_property_hours_data <- db_read_tbl(pool, "entrata.property_hours")
# entrata_property_lease_terms_data <- db_read_tbl(pool, "entrata.property_lease_terms")
# entrata_property_lease_term_windows_data <- db_read_tbl(pool, "entrata.property_lease_term_windows")
# entrata_property_post_months_data <- db_read_tbl(pool, "entrata.property_post_months")
# entrata_property_space_options_data <- db_read_tbl(pool, "entrata.property_space_options")

entrata_property_primary_addresses <- entrata_property_address_data |>
  dplyr::filter(.data$address_type == "Primary") |>
  dplyr::select(property_id, street, city, state, postal_code, country)

source("data-raw/scripts/gmh/prepare/prepare_property_relations.R")
source("data-raw/scripts/gmh/prepare/prepare_property_descriptions.R")

gmh_properties_tbl_prep <- tibble::tibble(
  property_id = entrata_property_data$property_id,
  property_name = entrata_property_data$marketing_name,
  parent_property_id = entrata_property_data$parent_property_id,
  property_type = entrata_property_data$property_type,
  property_status = "Active",
  property_website = entrata_property_data$website,
  property_email = entrata_property_data$email,
  year_built = entrata_property_data$year_built,
  short_description = entrata_property_data$short_description,
  long_description = entrata_property_data$long_description,
) |>
  dplyr::left_join(
    tibble::tibble(
      property_id = entrata_property_phone_data$property_id,
      property_phone_number = entrata_property_phone_data$phone_number
    ),
    by = "property_id"
  ) |>
  dplyr::left_join(
    entrata_property_primary_addresses,
    by = "property_id"
  ) |>
  dplyr::left_join(
    relations,
    by = "property_id"
  ) |>
  dplyr::left_join(
    property_descriptions,
    by = "property_id"
  ) |>
  dplyr::mutate(
    property_address = glue::glue("{street}, {city}, {state} {postal_code}, {country}"),
    property_website = ifelse(property_name == "1047 Commonwealth Avenue", "https://www.1047commonwealth.com/", property_website)
  ) |>
  # put descriptions then relations at the end
  dplyr::select(
    property_id,
    property_name,
    parent_property_id,
    property_type,
    property_status,
    property_website,
    property_email,
    property_phone_number,
    property_address,
    property_description,
    portfolio_id,
    partner_id
  )

readr::write_csv(
  gmh_properties_tbl_prep,
  "data-raw/data/working/gmh/gmh_properties.csv"
)
