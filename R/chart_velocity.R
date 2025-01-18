chart_velocity_comparison <- function(data, id = NULL, ...) {

  validate_col_names(
    data,
    c(
      "property_name",
      "vel_90",
      "vel_95",
      "vel_100"
    )
  )

  chart_data <- data |>
    dplyr::select(
      property_name,
      vel_90,
      vel_95,
      vel_100
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("vel_"),
      names_to = "velocity_type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      velocity_type = paste0(stringr::str_remove(velocity_type, "vel_"), "%")
    )

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = property_name,
      y = value,
      fill = velocity_type
    ),
    type = "bar"
  ) |>
    apexcharter::ax_title(
      text = "Velocity Comparison"
    ) |>
    apexcharter::ax_subtitle(
      text = "90, 95, 100% Velocity"
    ) |>
    apexcharter::ax_xaxis(
      labels = list(rotate = -45),
      title = list("Property")
    ) |>
    apexcharter::ax_yaxis(
      title = list("Leases Per Day"),
      labels = list(
        formatter = apexcharter::JS(
          "function(val) {return val.toFixed(1)}"
        )
      )
    ) |>
    apexcharter::ax_colors(
      chart_colors(1:3)
    ) |>
    apexcharter::ax_plotOptions(
      bar = list(
        horizontal = FALSE,
        columnWidth = "55%",
        endingShape = "rounded"
      )
    ) |>
    apexcharter::ax_legend(
      position = "top"
    ) |>
    apexcharter::ax_grid(
      show = TRUE
    )

}

plot_velocity <- function(data, ...) {

  plotly::plot_ly(data) |>
    plotly::add_trace(
      x = ~property_name,
      y = ~vel_90,
      name = "90% Velocity",
      type = "bar"
    ) |>
    plotly::add_trace(
      x = ~property_name,
      y = ~vel_95,
      name = "95% Velocity",
      type = "bar"
    ) |>
    plotly::add_trace(
      x = ~property_name,
      y = ~vel_100,
      name = "100% Velocity",
      type = "bar"
    ) |>
    plotly::layout(
      title = "Velocity by Property",
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "Velocity % (Beds/Week Needed)"),
      margin = list(b = 120)
    )

}
