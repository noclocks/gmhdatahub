chart_weekly_leasing_breakdown <- function(
    data,
    id = NULL,
    height = 500,
    ...
) {

  validate_col_names(
    data,
    c(
      "property_name",
      "weekly_new",
      "weekly_renewal"
    )
  )

  chart_data <- data |>
    dplyr::select(
      property_name,
      weekly_new,
      weekly_renewal
    ) |>
    tidyr::pivot_longer(
      cols = c(weekly_new, weekly_renewal),
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        type,
        "weekly_new" = "New",
        "weekly_renewal" = "Renewal"
      )
    )

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = property_name,
      y = value,
      fill = type
    ),
    type = "column",
    height = height,
    elementId = id,
    ...
  ) |>
    apexcharter::ax_title(
      text = "Weekly Leasing Activity Breakdown",
      align = "center"
    ) |>
    apexcharter::ax_subtitle(
      text = "New vs Renewal Leases",
      align = "center"
    ) |>
    apexcharter::ax_yaxis(
      title = list(text = "Number of Leases")
    ) |>
    apexcharter::ax_xaxis(
      title = list(text = "Property Name"),
      labels = list(rotate = -45)
    ) |>
    apexcharter::ax_legend(position = "top") |>
    apexcharter::ax_chart(toolbar = list(show = TRUE)) |>
    apexcharter::ax_colors(
      c(chart_colors("primary"), chart_colors("secondary"))
    )

}

chart_weekly_activity <- function(data, id = NULL, height = 500, ...) {

  validate_col_names(
    data,
    c(
      "property_name",
      "weekly_new",
      "weekly_renewal",
      "weekly_total"
    )
  )

  chart_data <- data |>
    dplyr::select(
      property_name,
      weekly_new,
      weekly_renewal,
      weekly_total
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("weekly"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        metric,
        "weekly_new" = "New Leases",
        "weekly_renewal" = "Renewal Leases",
        "weekly_total" = "Total Leases"
      )
    )

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = property_name,
      y = value,
      group = metric
    ),
    type = "line",
    elementId = id,
    ...
  ) |>
    apexcharter::ax_title(
      text = "Weekly Leasing Activity",
      align = "center"
    ) |>
    apexcharter::ax_subtitle(
      text = "New, Renewal, and Total Leases by Property",
      align = "center"
    ) |>
    apexcharter::ax_yaxis(
      title = list(text = "Number of Leases")
    ) |>
    apexcharter::ax_xaxis(
      title = list(text = "Property Name"),
      labels = list(rotate = -45)
    ) |>
    apexcharter::ax_legend(position = "top") |>
    apexcharter::ax_chart(toolbar = list(show = TRUE)) |>
    apexcharter::ax_colors(
      c(chart_colors(1:3))
    )

}

plot_weekly <- function(data, ...) {

  plotly::plot_ly(data) |>
    plotly::add_trace(
      x = ~property_name,
      y = ~weekly_new,
      name = "New Leases",
      type = "bar"
    ) |>
    plotly::add_trace(
      x = ~property_name,
      y = ~weekly_renewal,
      name = "Renewals",
      type = "bar"
    ) |>
    plotly::layout(
      barmode = "stack",
      yaxis = list(title = "Weekly Leases"),
      xaxis = list(title = "", tickangle = 45),
      margin = list(b = 120)
    )

}
