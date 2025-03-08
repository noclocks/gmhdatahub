#  ------------------------------------------------------------------------
#
# Title : Shiny App Charts
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' App Charts
#'
#' @name app_charts
#'
#' @description
#' Chart functions for the GMH DataHub Shiny App.
#'
#' These charts are used throughout the application to visualize data.
#'
#' @details
#' The following functions are available:
#'
#' - `chart_current_vs_prior()`: Create a chart comparing current vs prior metrics.
#' - `chart_occupancy()`: Create a chart comparing occupancy rates.
#' - `chart_velocity()`: Create a chart comparing pre-lease velocity rates.
#'
#' @param chart An [apexcharter::apex()] chart object.
#' @param data A data frame containing the data to plot.
#' @param metric The metric to compare.
#' @param target The target occupancy rate. Defaults to 0.9.
#' @param by The grouping variable for the occupancy chart. Defaults to "property".
#' @param id The element ID for the chart. Defaults to `NULL`.
#' @param ... Additional arguments to pass to the [apexcharter::apex()] function.
#'
#' @seealso [utils_charts] for utility functions related to each chart.
#' @seealso [apexcharter::apex()] for the main charting function.
#'
#' @returns
#' Each chart function returns an [apexcharter::apex()] chart object.
NULL

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_yaxis ax_colors ax_xaxis ax_subtitle ax_title apex aes
#' @importFrom rlang arg_match
chart_current_vs_prior <- function(
    data,
    metric = c("total_new", "total_renewals", "total_leases", "preleased_percent"),
    id = NULL,
    ...
) {

  metric <- rlang::arg_match(metric)

  chart_data <- prep_current_vs_prior_chart_data(data, metric)

  metric_map <- list(
    "total_new" = list(
      cols = c("property_name", "current_total_new", "prior_total_new"),
      title = "Current vs Prior Total New Leases",
      subtitle = "Comparison of current and prior total new leases by property.",
      y_axis = "Number of New Leases"
    ),
    "total_renewals" = list(
      cols = c("property_name", "current_total_renewals", "prior_total_renewals"),
      title = "Current vs Prior Total Renewals",
      subtitle = "Comparison of current and prior total renewals by property.",
      y_axis = "Number of Renewals"
    ),
    "total_leases" = list(
      cols = c("property_name", "current_total_leases", "prior_total_leases"),
      title = "Current vs Prior Total Leases",
      subtitle = "Comparison of current and prior total leases by property.",
      y_axis = "Number of Leases"
    ),
    "preleased_percent" = list(
      cols = c("property_name", "current_preleased_percent", "prior_preleased_percent"),
      title = "Current vs Prior Pre-Lease Percent",
      subtitle = "Comparison of current and prior pre-leased percent by property.",
      y_axis = "Pre-Lease Percent (%)"
    )
  )

  title <- metric_map[[metric]]$title
  subtitle <- metric_map[[metric]]$subtitle
  y_axis <- metric_map[[metric]]$y_axis
  y_pct <- metric == "preleased_percent"

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(x = .data$property_name, y = .data$value, fill = .data$type),
    type = "column",
    elementId = id,
    ...
  ) |>
    apexcharter::ax_title(text = title) |>
    apexcharter::ax_subtitle(text = subtitle) |>
    apexcharter::ax_xaxis(title = list(text = "Property Name")) |>
    apexcharter::ax_colors(chart_colors(1:2)) |>
    apexcharter::ax_yaxis(
      title = list(text = y_axis),
      labels = list(formatter = format_yaxis_labels(y_pct))
    ) |>
    default_chart_options()

}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_colors ax_plotOptions ax_dataLabels ax_tooltip ax_annotations ax_xaxis ax_yaxis ax_subtitle ax_title apex aes format_num JS
#' @importFrom rlang arg_match
#' @importFrom scales percent
chart_occupancy <- function(
    data,
    target = 0.9,
    by = c("property", "partner"),
    id = NULL,
    ...
) {

  by <- rlang::arg_match(by)
  chart_data <- prep_occupancy_chart_data(data, target, by)

  if (by == "partner") {
    chart_subtitle <- "Current Occupancy Rates by Investment Partner"
    chart_xaxis_title <- "Investment Partners"
    chart_xaxis_categories <- unique(chart_data$investment_partner)
    aes_x <- "investment_partner"
  } else {
    chart_subtitle <- "Current Occupancy Rates by Property"
    chart_xaxis_title <- "Property Name"
    chart_xaxis_categories <- unique(chart_data$property_name)
    aes_x <- "property_name"
  }

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = .data[[aes_x]],
      y = .data$current_occupancy
    ),
    type = "column",
    elementId = id,
    ...
  ) |>
    apexcharter::ax_title(
      text = "Occupancy %",
      align = "center"
    ) |>
    apexcharter::ax_subtitle(
      text = chart_subtitle,
      align = "center"
    ) |>
    apexcharter::ax_yaxis(
      title = list(text = "Occupancy Rate (%)"),
      labels = list(formatter = apexcharter::format_num(".0%")),
      tickAmount = 10,
      min = 0,
      max = 1
    ) |>
    apexcharter::ax_xaxis(
      title = list(text = chart_xaxis_title),
      categories = chart_xaxis_categories,
      labels = list(rotate = -45)
    ) |>
    apexcharter::ax_annotations(
      yaxis = list(
        list(
          y = target,
          borderColor = gmh_colors("danger"),
          borderWidth = 2,
          borderDash = c(5, 5),
          label = list(
            text = paste0("🎯 Target: ", scales::percent(target)),
            position = "right",
            style = list(color = gmh_colors("danger"))
          )
        )
      )
    ) |>
    apexcharter::ax_tooltip(
      y = list(
        title = list(
          formatter = apexcharter::JS("function(val) { return 'Occupancy Rate:' }")
        ),
        formatter = apexcharter::format_num(".0%")
      )
    ) |>
    apexcharter::ax_dataLabels(
      enabled = TRUE,
      formatter = apexcharter::format_num(".0%")
    ) |>
    apexcharter::ax_plotOptions(
      bar = list(
        colors = list(
          ranges = list(
            list(
              from = 0,
              to = target,
              color = gmh_colors("danger")
            ),
            list(
              from = target,
              to = 1,
              color = gmh_colors("primary")
            ),
            list(
              from = 1,
              to = 1,
              color = gmh_colors("success")
            )
          )
        )
      )
    ) |>
    apexcharter::ax_colors(unique(chart_data$bar_color)) |>
    default_chart_options()
}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_plotOptions ax_colors ax_yaxis ax_xaxis ax_subtitle ax_title apex aes JS
chart_velocity <- function(data, id = NULL, ...) {

  chart_data <- prep_velocity_chart_data(data)

  apexcharter::apex(
    data = chart_data,
    type = "bar",
    elementId = id,
    mapping = apexcharter::aes(
      x = .data$property_name,
      y = .data$value,
      fill = .data$velocity_type
    ),
    ...
  ) |>
    apexcharter::ax_title(text = "Pre-Lease Velocity Rates by Property") |>
    apexcharter::ax_subtitle(text = "90%, 95%, and 100% Velocities") |>
    apexcharter::ax_xaxis(
      labels = list(rotate = -45),
      title = list(text = "Property")
    ) |>
    apexcharter::ax_yaxis(
      title = list(text = "Leases Per Day"),
      labels = list(formatter = apexcharter::JS("function(val) {return val.toFixed(1)}"))
    ) |>
    apexcharter::ax_colors(chart_colors(1:3)) |>
    apexcharter::ax_plotOptions(
      bar = list(
        horizontal = FALSE,
        columnWidth = "55%",
        endingShape = "rounded"
      )
    ) |>
    default_chart_options()
}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_stroke ax_colors ax_dataLabels ax_xaxis ax_yaxis ax_title apex aes JS
chart_pre_lease_rates <- function(data, id = NULL, ...) {

  chart_data <- prep_pre_lease_rates_chart_data(data)

  apexcharter::apex(
    data = chart_data,
    type = "line",
    elementId = id,
    mapping = apexcharter::aes(
      x = .data$property_name,
      y = .data$preleased_percent,
      group = .data$period,
      color = .data$period
    ),
    ...
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
    apexcharter::ax_colors(c(chart_colors("primary"), chart_colors("secondary"))) |>
    apexcharter::ax_stroke(width = 2) |>
    default_chart_options()

}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_colors ax_plotOptions ax_dataLabels ax_title apex aes JS
#' @importFrom snakecase to_title_case
chart_partner_distribution <- function(data, metric = "total_beds", id = NULL, ...) {

  chart_data <- prep_partner_distribution_chart_data(data, metric)

  tit <- paste0(snakecase::to_title_case(metric), " by Investment Partner")

  apexcharter::apex(
    data = chart_data,
    type = "donut",
    elementId = id,
    mapping = apexcharter::aes(
      x = .data$investment_partner,
      y = .data$total
    ),
    ...
  ) |>
    apexcharter::ax_title(text = tit) |>
    apexcharter::ax_dataLabels(enabled = TRUE) |>
    apexcharter::ax_plotOptions(
      pie = list(
        dataLabels = list(
          formatter = apexcharter::JS("function(value, { seriesIndex, dataPointIndex, w }) {
              return w.config.labels[dataPointIndex] + ': ' + value.toLocaleString() + ' beds'
            }")
        )
      )
    ) |>
    apexcharter::ax_colors(chart_colors(1:5, 8)) |>
    default_chart_options()

}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_markers ax_stroke ax_yaxis ax_xaxis ax_subtitle ax_title apex aes JS
chart_portfolio_summary <- function(data, id = NULL, ...) {

  chart_data <- prep_portfolio_summary_chart_data(data)

  x_categories <- list(
    "Average Occupancy",
    "Pre-Leased Percent",
    "Renewal Rate",
    "Bed Utilization",
    "Velocity Score"
  )

  apexcharter::apex(
    data = chart_data,
    type = "radar",
    elementId = id,
    mapping = apexcharter::aes(
      x = metric,
      y = value,
      group = investment_partner
    ),
    ...
  ) |>
    apexcharter::ax_title(text = "Portfolio Performance Metrics") |>
    apexcharter::ax_subtitle(text = "Average Performance Metrics by Investment Partner") |>
    apexcharter::ax_xaxis(categories = x_categories) |>
    apexcharter::ax_yaxis(
      min = 0,
      max = 100,
      labels = list(
        formatter = apexcharter::JS("function(val) { return val.toFixed(1) + '%' }")
      )
    ) |>
    apexcharter::ax_stroke(width = 2) |>
    apexcharter::ax_markers(size = 5) |>
    default_chart_options()

}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_colors ax_xaxis ax_yaxis ax_subtitle ax_title apex aes
chart_weekly_activity <- function(data, id = NULL, ...) {

  chart_data <- prep_weekly_activity_chart_data(data)

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = .data$property_name,
      y = .data$value,
      group = .data$metric
    ),
    type = "column",
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
    apexcharter::ax_colors(c(chart_colors(1:3))) |>
    default_chart_options()
}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_colors ax_xaxis ax_yaxis ax_subtitle ax_title apex aes
chart_weekly_breakdown <- function(data, id = NULL, ...) {

  chart_data <- prep_weekly_breakdown_chart_data(data)

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = .data$property_name,
      y = .data$value,
      fill = .data$type
    ),
    type = "column",
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
    apexcharter::ax_colors(
      c(chart_colors("primary"), chart_colors("secondary"))
    ) |>
    default_chart_options()
}

#' @rdname app_charts
#' @export
#' @importFrom apexcharter ax_legend ax_dataLabels ax_tooltip ax_colors ax_plotOptions ax_yaxis ax_xaxis
#' @importFrom apexcharter ax_subtitle ax_title apex aes format_num JS
#' @importFrom rlang arg_match
chart_yoy_variance <- function(data, by = c("property", "partner"), id = NULL, ...) {

  by <- rlang::arg_match(by)
  chart_data <- prep_yoy_variance_chart_data(data, by)

  if (by == "property") {
    aes_x <- "property_name"
    chart_subtitle <- "Year-Over-Year Variance by Property"
    x_axis_title <- "Property"
    x_axis_categories <- unique(chart_data$property_name)
  } else {
    aes_x <- "investment_partner"
    chart_subtitle <- "Year-Over-Year Variance by Investment Partner"
    x_axis_title <- "Investment Partner"
    x_axis_categories <- unique(chart_data$investment_partner)
  }

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = .data[[aes_x]],
      y = .data$yoy_variance_percent
    ),
    type = "bar",
    elementId = id,
    ...
  ) |>
    apexcharter::ax_title(
      text = "Year-Over-Year Variance",
      align = "center"
    ) |>
    apexcharter::ax_subtitle(
      text = chart_subtitle,
      align = "center"
    ) |>
    apexcharter::ax_xaxis(
      title = list(
        text = x_axis_title
      ),
      labels = list(
        rotate = -45
      )
    ) |>
    apexcharter::ax_yaxis(
      categories = x_axis_categories,
      title = list(
        text = "YoY Variance (%)"
      ),
      labels = list(
        formatter = apexcharter::format_num(".0%")
      ),
    ) |>
    apexcharter::ax_plotOptions(
      bar = list(
        horizontal = FALSE,
        columnWidth = "55%",
        endingShape = "rounded",
        distributed = TRUE
      )
    ) |>
    apexcharter::ax_colors(
      colors = unlist(lapply(chart_data$yoy_variance_percent, function(x) {
        if (x < 0) gmh_colors("danger") else gmh_colors("success")
      }))
    ) |>
    apexcharter::ax_tooltip(
      y = list(
        title = list(
          formatter = apexcharter::JS("function(val) { return 'YoY Variance:' }")
        ),
        formatter = apexcharter::format_num(".0%")
      )
    ) |>
    apexcharter::ax_dataLabels(
      enabled = TRUE,
      formatter = apexcharter::format_num(".0%")
    ) |>
    default_chart_options() |>
    apexcharter::ax_legend(show = FALSE)
}

