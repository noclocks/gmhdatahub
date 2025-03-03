chart_pre_lease_rates <- function(data, id = NULL, ...) {
  validate_col_names(
    data,
    c(
      "property_name",
      "current_preleased_percent",
      "prior_preleased_percent"
    )
  )

  chart_data <- data |>
    dplyr::select(
      property_name,
      current_preleased_percent,
      prior_preleased_percent
    ) |>
    tidyr::pivot_longer(
      cols = c(
        current_preleased_percent,
        prior_preleased_percent
      ),
      names_to = "period",
      values_to = "preleased_percent"
    ) |>
    dplyr::mutate(
      period = dplyr::case_when(
        period == "current_preleased_percent" ~ "Current",
        period == "prior_preleased_percent" ~ "Prior"
      )
    )

  apexcharter::apex(
    data = chart_data,
    type = "line",
    mapping = apexcharter::aes(
      x = property_name,
      y = preleased_percent,
      group = period,
      color = period
    )
  ) |>
    apexcharter::ax_title(text = "Pre-lease Rate Comparison", align = "center") |>
    apexcharter::ax_yaxis(
      title = list(text = "Pre-Lease Rate (%)"),
      labels = list(
        formatter = apexcharter::JS(
          "function(value) { return (value * 100).toFixed(1) + '%' }"
        )
      ),
      tickAmount = 10,
      min = 0,
      max = 1
    ) |>
    apexcharter::ax_xaxis(
      title = list(text = "Property Name"),
      categories = unique(chart_data$property_name),
      labels = list(rotate = -45)
    ) |>
    apexcharter::ax_dataLabels(
      enabled = TRUE,
      formatter = apexcharter::JS("function(value) { return (value * 100).toFixed(1) + '%' }")
    ) |>
    apexcharter::ax_chart(toolbar = list(show = TRUE)) |>
    apexcharter::ax_colors(c(chart_colors("primary"), chart_colors("secondary"))) |>
    apexcharter::ax_stroke(width = 2) |>
    apexcharter::ax_legend(position = "top")
}
