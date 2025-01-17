map_survey_locations <- function(properties = NULL, competitors = NULL, universities = NULL) {

  universities <- universities |>
    dplyr::mutate(
      map_popup_html = paste0(
        "<b>",
        university_name,
        "</b><br>",
        university_address,
        "<br>",
        "<a href='",
        university_website,
        "' target='_blank'>Website</a>"
      )
    )

  property_icons <- leaflet::makeIcon(
    iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
    iconWidth = 25, iconHeight = 41,
    iconAnchorX = 12, iconAnchorY = 41,
    shadowUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-shadow.png",
    shadowWidth = 41, shadowHeight = 41
  )

  competitor_icons <- leaflet::makeIcon(
    iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
    iconWidth = 25, iconHeight = 41,
    iconAnchorX = 12, iconAnchorY = 41,
    shadowUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-shadow.png",
    shadowWidth = 41, shadowHeight = 41
  )

  university_icons <- leaflet::makeIcon(
    iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
    iconWidth = 25, iconHeight = 41,
    iconAnchorX = 12, iconAnchorY = 41,
    shadowUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-shadow.png",
    shadowWidth = 41, shadowHeight = 41
  )

  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addMarkers(
      data = properties,
      lng = ~longitude,
      lat = ~latitude,
      icon = property_icons,
      popup = ~map_popup_html,
      group = "Properties"
    ) |>
    leaflet::addMarkers(
      data = competitors,
      lng = ~longitude,
      lat = ~latitude,
      icon = competitor_icons,
      popup = ~map_popup_html,
      group = "Competitors"
    ) |>
    leaflet::addMarkers(
      data = universities,
      lng = ~longitude,
      lat = ~latitude,
      icon = university_icons,
      popup = ~map_popup_html,
      group = "Universities"
    ) |>
    leaflet::addLayersControl(
      overlayGroups = c("Properties", "Competitors", "Universities"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors = c("blue", "red", "green"),
      labels = c("Properties", "Competitors", "Universities"),
      opacity = 1
    )

}
