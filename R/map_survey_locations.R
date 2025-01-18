map_survey_locations <- function(properties = NULL, competitors = NULL, universities = NULL) {
  merged_data <- dplyr::bind_rows(
    properties,
    competitors,
    universities
  )

  if (nrow(merged_data) == 0) {
    return(leaflet::leaflet() |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::setView(lng = -98.35, lat = 39.5, zoom = 4))
  }

  if (nrow(competitors) == 0) {
    merged_data <- merged_data |> dplyr::filter(.data$map_layer == "properties")
    competitors <- NULL
    universities <- NULL
  }

  bounds <- merged_data |>
    dplyr::filter(
      !identify_outliers(.data$latitude) & !identify_outliers(.data$longitude)
    ) |>
    dplyr::summarise(
      lng1 = min(longitude),
      lng2 = max(longitude),
      lat1 = min(latitude),
      lat2 = max(latitude)
    )

  center <- dplyr::summarise(
    merged_data,
    lng = mean(longitude, na.rm = TRUE),
    lat = mean(latitude, na.rm = TRUE)
  )

  lat_rng <- bounds$lat2 - bounds$lat1
  lng_rng <- bounds$lng2 - bounds$lng1
  zoom <- min(18, max(3, floor(log2(360 / max(lat_rng, lng_rng)))))

  property_icons <- leaflet::makeAwesomeIcon(icon = "home", library = "fa", markerColor = "blue", iconColor = "white")
  competitor_icons <- leaflet::makeAwesomeIcon(icon = "industry", library = "fa", markerColor = "red", iconColor = "white")
  university_icons <- leaflet::makeAwesomeIcon(icon = "graduation-cap", library = "fa", markerColor = "green", iconColor = "white")

  hold <- leaflet::leaflet(data = merged_data) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  if (nrow(properties) > 0) {
    hold <- hold |>
      leaflet::addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = "properties",
        group = "Properties",
        icon = property_icons,
        label = ~location_name,
        labelOptions = leaflet::labelOptions(
          interactive = TRUE,
          clickable = TRUE,
          noHide = TRUE
        ),
        popup = ~map_popup_html,
        popupOptions = leaflet::popupOptions(
          closeButton = TRUE,
          autoPan = TRUE,
          maxWidth = 300,
          maxHeight = 500
        ),
        data = properties
      )
  }

  if (!is.null(competitors) && nrow(competitors) > 0) {
    hold <- hold |>
      leaflet::addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = "competitors",
        group = "Competitors",
        icon = competitor_icons,
        label = ~location_name,
        labelOptions = leaflet::labelOptions(
          interactive = TRUE,
          clickable = TRUE,
          noHide = TRUE
        ),
        popup = ~map_popup_html,
        popupOptions = leaflet::popupOptions(
          closeButton = TRUE,
          autoPan = TRUE,
          maxWidth = 300,
          maxHeight = 500
        ),
        data = competitors
      )
  }

  if (!is.null(universities) && nrow(universities) > 0) {
    hold <- hold |>
      leaflet::addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = "universities",
        group = "Universities",
        icon = university_icons,
        label = ~location_name,
        labelOptions = leaflet::labelOptions(
          interactive = TRUE,
          clickable = TRUE,
          noHide = TRUE
        ),
        popup = ~map_popup_html,
        popupOptions = leaflet::popupOptions(
          closeButton = TRUE,
          autoPan = TRUE,
          maxWidth = 300,
          maxHeight = 500
        ),
        data = universities
      )
  }

  hold |>
    leaflet::addLayersControl(
      overlayGroups = c("Properties", "Competitors", "Universities"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("blue", "red", "green"),
      labels = c("Properties", "Competitors", "Universities"),
      opacity = 1
    ) |>
    leaflet::clearBounds() |>
    leaflet::fitBounds(
      lng1 = bounds$lng1,
      lng2 = bounds$lng2,
      lat1 = bounds$lat1,
      lat2 = bounds$lat2
    ) |>
    leaflet::setView(
      lng = center$lng,
      lat = center$lat,
      zoom = zoom
    )
}


identify_outliers <- function(x, factor = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - factor * iqr
  upper_bound <- q3 + factor * iqr
  x < lower_bound | x > upper_bound
}
