
#  ------------------------------------------------------------------------
#
# Title : Location Data
#    By : Jimmy Briggs
#  Date : 2025-01-14
#
#  ------------------------------------------------------------------------


# get entrata properties
entrata_property_data <- db_read_tbl(pool, "entrata.properties")
entrata_property_address_data <- db_read_tbl(pool, "entrata.property_addresses")

# enrich
entrata_property_data_enriched <- enrich_property_data(entrata_property_data)

# transform
entrata_property_data_transformed <- entrata_property_data_enriched |>
  dplyr::transmute(
    property_id = property_id,
    property_name = marketing_name,
    address = address,
    country = "USA",
    latitude = gmaps_latitude,
    longitude = gmaps_longitude,
    phone_number = gmaps_phone_number,
    email = NA_character_,
    website = dplyr::coalesce(website, gmaps_website),
    image_url = NA_character_,
    rating = gmaps_rating,
    gmaps_url = gmaps_url,
    gmaps_place_id = gmaps_place_id,
    gmaps_rating = gmaps_rating,
    gmaps_reviews_count = gmaps_user_ratings_total,
    map_layer = "properties",
    map_marker_icon = "home",
    map_marker_color = "blue",
    map_popup_html = paste0(
      "<div style='min-width: 200px;'>",
      "<h4>", property_name, "</h4>",
      "<p><strong>Property Details:</strong><br>",
      "Type: ", property_type, "<br>",
      "Year Built: ", year_built, "<br>",
      "Rating: ", rating, " stars<br>",
      "<p><strong>Contact:</strong><br>",
      "Phone: ", phone_number, "<br>",
      "<a href='", website, "' target='_blank'>Visit Website</a></p>",
      ifelse(
        image_url != "",
        paste0("<img src='", image_url, "' style='max-width: 200px; margin-top: 10px;'>"),
        ""
      ),
      "</div>"
    )
  ) |>
  dplyr::left_join(
    entrata_property_address_data |>
      dplyr::filter(
        address_type == "Primary"
      ) |>
      dplyr::select(
        property_id,
        street,
        city,
        state,
        postal_code
      ),
    by = "property_id"
  ) |>
  dplyr::select(
    property_id,
    property_name,
    address,
    city,
    state,
    postal_code,
    country,
    latitude,
    longitude,
    phone_number,
    email,
    website,
    image_url,
    rating,
    gmaps_url,
    gmaps_place_id,
    gmaps_rating,
    gmaps_reviews_count,
    map_layer,
    map_marker_icon,
    map_marker_color,
    map_popup_html
  )

# database
db_gmh_locations_tbl <- db_read_tbl(pool, "gmh.locations")

db_entrata_locations_tbl <- entrata_property_data_transformed |>
  dplyr::filter(
    !.data$property_name %in% db_gmh_locations_tbl$location_name
  ) |>
  dplyr::rename(location_name = property_name) |>
  dplyr::select(-property_id)

pool::dbAppendTable(
  pool,
  DBI::SQL("gmh.locations"),
  value = db_entrata_locations_tbl,
  overwrite = FALSE
)

db_gmh_locations_tbl_new <- db_read_tbl(pool, "gmh.locations")

# need to update gmh.properties with new location ids
db_gmh_properties_tbl <- db_read_tbl(pool, "gmh.properties")

db_gmh_properties_tbl_updated <- db_gmh_properties_tbl |>
  dplyr::select(-location_id) |>
  dplyr::left_join(
    db_gmh_locations_tbl_new,
    by = c("property_name" = "location_name")
  ) |>
  dplyr::transmute(
    property_id = property_id,
    property_name = property_name,
    property_type = property_type,
    property_status = property_status,
    property_description = property_description,
    property_url = property_url,
    portfolio_id = portfolio_id,
    location_id = location_id
  )

# database
pool::dbWriteTable(
  pool,
  DBI::SQL("gmh.properties"),
  value = db_gmh_properties_tbl_updated,
  overwrite = TRUE
)

# get updated properties
db_gmh_properties_tbl_updated_ <- db_read_tbl(pool, "gmh.properties")
waldo::compare(
  db_gmh_properties_tbl_updated,
  db_gmh_properties_tbl_updated_
)

# update mkt
db_mkt_properties_tbl <- db_read_tbl(pool, "mkt.properties")

db_mkt_entrata_properties_to_add <- tibble::tibble(
  property_id = as.character(entrata_property_data$property_id),
  entrata_property_id = as.character(entrata_property_data$property_id),
  property_name = entrata_property_data$marketing_name,
  is_competitor = FALSE
) |>
  dplyr::filter(
    !property_id %in% db_mkt_properties_tbl$property_id
  )

pool::dbAppendTable(
  pool,
  DBI::SQL("mkt.properties"),
  value = db_mkt_entrata_properties_to_add,
  overwrite = FALSE
)
