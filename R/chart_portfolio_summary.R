chart_portfolio_summary <- function(data, ...) {

  validate_col_names(
    data,
    c(
      "investment_partner",
      "current_occupancy",
      "current_preleased_percent",
      "current_total_renewals",
      "current_total_new",
      "current_occupied",
      "total_beds",
      "vel_95"
    )
  )

  chart_data <- data |>
    dplyr::group_by(.data$investment_partner) |>
    dplyr::summarise(
      avg_occupancy = mean(.data$current_occupancy, na.rm = TRUE) * 100,
      avg_preleased = mean(.data$current_preleased_percent, na.rm = TRUE) * 100,
      renewal_rate = mean(.data$current_total_renewals / (.data$current_total_new + .data$current_total_renewals), na.rm = TRUE) * 100,
      beds_utilization = mean(.data$current_occupied / .data$total_beds, na.rm = TRUE) * 100,
      velocity_score = (100 - mean(.data$vel_95, na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = -investment_partner,
      names_to = "metric",
      values_to = "value"
    )

  x_categories <- list(
    "Average Occupancy",
    "Pre-Leased Percent",
    "Renewal Rate",
    "Bed Utilization",
    "Velocity Score"
  )

  apexcharter::apex(
    data = chart_data,
    mapping = aes(
      x = metric,
      y = value,
      group = investment_partner
    ),
    type = "radar"
  ) |>
    apexcharter::ax_title(
      text = "Portfolio Performance Metrics"
    ) |>
    apexcharter::ax_subtitle(
      text = "Average Performance Metrics by Investment Partner"
    ) |>
    apexcharter::ax_xaxis(
      categories = x_categories
    ) |>
    apexcharter::ax_yaxis(
      min = 0,
      max = 100,
      labels = list(
        formatter = apexcharter::JS("function(val) { return val.toFixed(1) + '%' }")
      )
    ) |>
    apexcharter::ax_stroke(width = 2) |>
    apexcharter::ax_markers(size = 5)

}
