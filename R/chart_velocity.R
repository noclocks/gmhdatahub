
prep_velocity_chart_data <- function(data, metrics) {

  if (length(metrics) == 0) return(NULL)

  data |>
    dplyr::select(
      property_name,
      tidyselect::all_of(metrics)
    ) |>
    tidyr::pivot_longer(
      cols = -property_name,
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == "occupancy" ~ "Occupancy %",
        metric == "vacancy" ~ "Vacancy %",
        metric == "prelease" ~ "Pre-lease %",
        metric == "total_beds" ~ "Total Beds",
        metric == "total_units" ~ "Total Units",
        metric == "total_renewals" ~ "Total Renewals",
        metric == "available_units" ~ "Available Units",
        metric == "weekly_new_leases" ~ "Weekly New Leases",
        metric == "weekly_renewals" ~ "Weekly Renewals",
        TRUE ~ metric
      )
    )
}

prep_rates_chart_data <- function(data, rent_type, metrics) {

  rent_cols <- if(rent_type == "bed") {
    c("market_rent_per_bed", "effective_rent_per_bed", "bundled_rent_per_bed")[metrics %in% c("market", "effective", "bundled")]
  } else {
    c("market_rent_per_unit", "effective_rent_per_unit", "bundled_rent_per_unit")[metrics %in% c("market", "effective", "bundled")]
  }

  if(length(rent_cols) == 0) return(NULL)

  data |>
    dplyr::select(
      property_name,
      tidyselect::all_of(rent_cols)
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(rent_cols),
      names_to = "rent_type",
      values_to = "amount"
    )

}

chart_velocity <- function(data) {

  if (is.null(data)) return(NULL)

  apexcharter::apex(
    data,
    type = "line",
    mapping = apexcharter::aes(
      x = property_name,
      y = value,
      group = metric
    )
  ) |>
    apexcharter::ax_series(
      list(
        name = unique(data$metric)
      )
    ) |>
    apexcharter::ax_yaxis(
      decimalsInFloat = 1,
      labels = list(
        formatter = apexcharter::JS("function(val) {
          if (this.seriesName && this.seriesName.includes('%')) {
            return val.toFixed(1) + '%';
          }
          return Math.round(val);
        }")
      )
    ) |>
    apexcharter::ax_xaxis(
      type = "category"
    ) |>
    apexcharter::ax_tooltip(
      shared = TRUE,
      y = list(
        formatter = apexcharter::JS("function(val, { seriesIndex, w }) {
          if (w.globals.seriesNames[seriesIndex].includes('%')) {
            return val.toFixed(1) + '%';
          }
          return Math.round(val);
        }")
      )
    ) |>
    apexcharter::ax_stroke(
      width = 2,
      curve = "smooth"
    ) |>
    apexcharter::ax_markers(
      size = 4
    ) |>
    apexcharter::ax_grid(
      show = TRUE,
      borderColor = "#e0e0e0",
      strokeDashArray = 0,
      position = "back"
    ) |>
    apexcharter::ax_title(
      text = "Leasing Velocity Metrics",
      align = "center",
      style = list(
        fontSize = "16px"
      )
    ) |>
    apexcharter::ax_legend(
      position = "top",
      horizontalAlign = "center"
    )

}

chart_rates_comparison <- function(data, rent_type) {

  if(is.null(data)) return(NULL)

  apexcharter::apex(
    data = data,
    type = "bar",
    mapping = apexcharter::aes(x = property_name, y = amount, fill = rent_type)
  ) |>
    apexcharter::ax_title(
      text = paste("Rent Analysis by Property -",
                   if (rent_type == "bed") "Per Bed" else "Per Unit"),
      align = "center",
      style = list(fontSize = "16px")
    ) |>
    apexcharter::ax_yaxis(
      labels = list(
        formatter = apexcharter::JS("function(val) { return '$' + val.toFixed(0) }")
      )
    ) |>
    apexcharter::ax_tooltip(
      shared = TRUE,
      y = list(
        formatter = apexcharter::JS("function(val) { return '$' + val.toFixed(0) }")
      )
    ) |>
    apexcharter::ax_legend(
      position = "top",
      horizontalAlign = "center"
    )
}

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
