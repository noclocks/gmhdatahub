get_gmaps_data <- function(
    name,
    address,
    gmaps_api_key = get_gmaps_config("api_key")) {
  googleway::set_key(gmaps_api_key)

  query_name <- name
  query_address <- paste0("place at ", address)

  place_candidates <- googleway::google_find_place(
    query_name,
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
    original_name = name,
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
    gmaps_api_key = get_gmaps_config("api_key")) {
  names <- property_data$marketing_name
  addresses <- property_data$address

  gmaps_data_lst <- purrr::map2(names, addresses, purrr::safely(get_gmaps_data)) |>
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

  if (length(errored_addresses) > 0) {
    cli::cli_alert_danger(
      "Failed to fetch Google Maps data for the following addresses: {.field {errored_addresses}}"
    )
  }

  dplyr::left_join(
    property_data,
    gmaps_info,
    by = c(
      "marketing_name" = "original_name",
      "address" = "original_address"
    )
  )
}
