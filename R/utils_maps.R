gmaps_properties_map_embed_iframe <- function(width = "100%", height = "500px") {

  url <- "https://www.google.com/maps/d/embed?mid=19tP5Bf66khGcrNnqTBsk879W2fS-u7U&ehbc=2E312F"

  htmltools::tags$iframe(
    src = url,
    width = width,
    height = height
  )

}

get_city_from_address <- function(address) {

  address_parts <- stringr::str_split(address, ", ") |> unlist()

  if (length(address_parts) == 4) {
    return(address_parts[2])
  } else {
    return(NA)
  }

}

get_postal_code_from_address <- function(address) {

  address_parts <- stringr::str_split(address, ", ") |> unlist()

  if (length(address_parts) == 4) {
    return(address_parts[3])
  } else {
    return(NA)
  }

}

# GIS utilities -----------------------------------------------------------

# get coordinates from address:

#' Geocode an Address
#'
#' @description
#' Geocode an address using the Google Maps API.
#'
#' @param address The address to geocode.
#' @param gmaps_api_key The Google Maps API key to use for geocoding.
#'   If not provided, the function will attempt to retrieve the key via
#'   [get_gmaps_api_key()].
#'
#' @returns A data frame containing the geocoded address information.
#'
#' @export
#'
#' @importFrom ggmap geocode register_google has_google_key
#' @importFrom cli cli_abort
#'
#' @examples
#' geocode_address("1600 Amphitheatre Parkway, Mountain View, CA")
geocode_address <- function(
    address,
    gmaps_api_key = get_gmaps_config("api_key")
) {

  # ensure has gmaps api key registered
  if (!ggmap::has_google_key()) {
    if (is.null(gmaps_api_key)) {
      cli::cli_abort("No Google Maps API key provided.")
    }
    ggmap::register_google(key = gmaps_api_key)
  }

  # geocode address
  ggmap::geocode(address)

}

get_gmaps_place_id <- function(
    address,
    gmaps_api_key = get_gmaps_config("api_key")
) {

  googleway::set_key(gmaps_api_key)

  googleway::google_geocode(address) |>
    purrr::pluck("results", "place_id", 1)

}

get_gmaps_rating <- function(
    place_id,
    gmaps_api_key = get_gmaps_config("api_key")
) {

  base_url <- "https://maps.googleapis.com/maps/api/place/details/json"

  req <- httr2::request(base_url) |>
    httr2::req_method("GET") |>
    httr2::req_url_query(
      place_id = place_id,
      fields = "name,rating,user_ratings_total",
      key = gmaps_api_key
    )

  resp <- httr2::req_perform(req)
  data <- resp |> httr2::resp_body_json()

  num_ratings <- purrr::pluck(data, "result", "user_ratings_total")
  rating <- purrr::pluck(data, "result", "rating")

  return(
    list(
      rating = rating,
      num_ratings = num_ratings
    )

  )
}

get_gmaps_data <- function(
    address,
    gmaps_api_key = get_gmaps_config("api_key")
) {

  googleway::set_key(gmaps_api_key)

  query <- paste0("place at ", address)

  place_candidates <- googleway::google_find_place(
    query,
    fields = c(
      "name",
      "formatted_address",
      "id",
      "place_id",
      "plus_code",
      "types",
      "icon",
      "photos",
      "geometry",
      "price_level",
      "opening_hours",
      "rating",
      "user_ratings_total"
    )
  ) |>
    purrr::pluck("candidates")

  # If no candidates found, return a row of NAs
  if (nrow(place_candidates) == 0) {
    return(
      tibble::tibble(
        gmaps_formatted_address = NA_character_,
        gmaps_latitude = NA_real_,
        gmaps_longitude = NA_real_,
        gmaps_name = NA_character_,
        gmaps_place_id = NA_character_,
        gmaps_rating = NA_real_,
        gmaps_user_ratings_total = NA_integer_,
        gmaps_types = NA_character_,
        gmaps_icon = NA_character_,
        gmaps_open_now = NA,
        gmaps_phone_number = NA_character_,
        gmaps_website = NA_character_,
        gmaps_url = NA_character_
      )
    )
  }

  # Assume the first candidate is the best match
  place <- place_candidates[1, ]

  # Extract fields from place candidate
  formatted_address <- place$formatted_address %||% NA_character_
  lat <- place$geometry$location$lat %||% NA_real_
  lng <- place$geometry$location$lng %||% NA_real_
  name <- place$name %||% NA_character_
  place_id <- place$place_id %||% NA_character_
  rating <- place$rating %||% NA_real_
  user_ratings_total <- place$user_ratings_total %||% NA_integer_
  types <- if (!is.null(place$types)) paste0(place$types, collapse = ", ") else NA_character_
  icon <- place$icon %||% NA_character_
  open_now <- place$opening_hours$open_now %||% NA

  # Get place details using place_id
  if (!is.na(place_id)) {
    details <- googleway::google_place_details(place_id = place_id) |>
      purrr::pluck("result")
    formatted_phone_number <- details$formatted_phone_number %||% NA_character_
    website <- details$website %||% NA_character_
    gmaps_url <- details$url %||% NA_character_
  } else {
    formatted_phone_number <- NA_character_
    website <- NA_character_
    gmaps_url <- NA_character_
  }

  tibble::tibble(
    original_address = address,
    gmaps_formatted_address = formatted_address,
    gmaps_latitude = lat,
    gmaps_longitude = lng,
    gmaps_name = name,
    gmaps_place_id = place_id,
    gmaps_rating = rating,
    gmaps_user_ratings_total = user_ratings_total,
    gmaps_types = types,
    gmaps_icon = icon,
    gmaps_open_now = open_now,
    gmaps_phone_number = formatted_phone_number,
    gmaps_website = website,
    gmaps_url = gmaps_url
  )
}

enrich_property_data <- function(
    property_data,
    gmaps_api_key = get_gmaps_config("api_key")
) {

  names <- property_data$marketing_name
  addresses <- property_data$address

  gmaps_data_lst <- purrr::map(addresses, purrr::safely(get_gmaps_data)) |>
    setNames(property_data$marketing_name)

  gmaps_info <- purrr::map_dfr(gmaps_data_lst, function(dat) {
    if (is.null(dat$error)) {
      return(dat$result)
    }
  })

  errored_addresses <- purrr::map(gmaps_data_lst, function(dat) {
    if (!is.null(dat$error)) {
      return(dat$error)
    }
  }) |>
    purrr::compact() |>
    names()

  cli::cli_alert_danger(
    "Failed to fetch Google Maps data for the following addresses: {.field {errored_addresses}}"
  )

  dplyr::left_join(
    property_data,
    gmaps_info,
    by = c("address" = "original_address")
  )

}

parse_address <- function(address) {
  parts <- stringr::str_split(address, ",\\s*")[[1]]

  # Handle missing parts gracefully
  # Expected at least: address_line_1, city, state_province, postal_code
  # If fewer parts, fill with NA
  parts <- c(parts, rep(NA, 4 - length(parts)))

  list(
    address_line_1 = parts[1],
    city = parts[2],
    state_province = parts[3],
    postal_code = parts[4],
    country = "USA"
  )
}

crete_map_popup <- function(property_data, ...) {

  paste0(
    "<div style='min-width: 200px;'>",
    "<h4>", property_data$property_name, "</h4>",
    "<p><strong>Property Details:</strong><br>",
    "Type: ", property_data$property_type, " (", property_data$product_type, ")<br>",
    "Status: ", property_data$property_status, "<br>",
    "Year Built: ", property_data$year_built, "<br>",
    "Rating: ", property_data$property_rating, " stars<br>",
    "Distance from Campus: ", property_data$distance_from_campus, " miles</p>",

    "<p><strong>Contact:</strong><br>",
    "Phone: ", property_data$phone_number, "<br>",
    "<a href='", property_data$website, "' target='_blank'>Visit Website</a></p>",

    "<p><strong>Management:</strong><br>",
    "Developer: ", property_data$developer, "<br>",
    "Manager: ", manager, "<br>",
    "Owner: ", owner, "</p>",

    ifelse(property_image != "",
           paste0("<img src='", property_image,
                  "' style='max-width: 200px; margin-top: 10px;'>"),
           ""),
    "</div>"
  )

}


prepare_property_locations_data <- function(enriched_data) {

}
