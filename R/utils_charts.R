
#  ------------------------------------------------------------------------
#
# Title : Charting Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------


# topic -----------------------------------------------------------------------------------------------------------

#' Charting Utilities
#'
#' @name utils_charts
#'
#' @description
#' Utility Functions for Charting and Data Visualization.
#'
#' @details
#' Chart Data Preparation Functions:
#'
#' - `prep_current_vs_prior_chart_data()`: Prepares data for the [chart_current_vs_prior()] chart.
#' - `prep_occupancy_chart_data()`: Prepares data for the [chart_occupancy()] chart.
#' - `prep_velocity_chart_data()`: Prepares data for the [chart_velocity()] chart.
#' - `prep_pre_lease_rates_chart_data()`: Prepares data for the [chart_pre_lease_rates()] chart.
#' - `prep_partner_distribution_chart_data()`: Prepares data for the [chart_partner_distribution()] chart.
#' - `prep_portfolio_summary_chart_data()`: Prepares data for the [chart_portfolio_summary()] chart.
#' - `prep_weekly_activity_chart_data()`: Prepares data for the [chart_weekly_activity()] chart.
#' - `prep_weekly_breakdown_chart_data()`: Prepares data for the [chart_weekly_leasing_breakdown()] chart.
#' - `prep_yoy_variance_chart_data()`: Prepares data for the [chart_yoy_variance()] chart.
#'
#' Miscellaneous Functions:
#'
#' - `default_chart_options()`: Default chart options for consistent formatting.
#' - `format_yaxis_labels()`: Format Y-axis labels with appropriate number formatting.
#'
#' @param chart An [apexcharter::apex()] chart object.
#' @param is_percentage A logical indicating if the Y-axis labels should be formatted as percentages.
#' @param data A tibble containing the data to be prepped for the chart.
#' @param metric A character vector specifying the metric to use for the chart.
#' @param target A numeric value representing the target occupancy rate.
#' @param by A character vector specifying the grouping variable for the chart.
#' @param type A character vector specifying the type of data to be prepped.
#'
#' @returns
#' Each `prep_` function returns a tibble with the data prepped for the corresponding chart.
#'
#' - `default_chart_options()`: An [apexcharter::apex()] chart object with default options applied.
#' - `format_yaxis_labels()`: A JavaScript function for formatting Y-axis labels.
#'
#' @seealso [app_charts] for the main charting functions.
NULL

#' @rdname utils_charts
#' @export
#' @importFrom apexcharter ax_chart ax_legend ax_grid
default_chart_options <- function(chart) {
  check_chart(chart)
  chart |>
    apexcharter::ax_chart(toolbar = list(show = TRUE)) |>
    apexcharter::ax_legend(show = TRUE, position = "top", horizontalAlign = "center") |>
    apexcharter::ax_grid(show = TRUE)
}

#' @rdname utils_charts
#' @export
#' @importFrom apexcharter JS
format_yaxis_labels <- function(is_percentage = FALSE) {
  if (is_percentage) {
    apexcharter::JS("function(value) { return (value * 100).toFixed(1) + '%' }")
  } else {
    apexcharter::JS("function(value) { return value.toFixed(0) }")
  }
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr case_when mutate select case_match
#' @importFrom rlang arg_match
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
prep_current_vs_prior_chart_data <- function(
    data,
    metric = c("total_new", "total_renewals", "total_leases", "preleased_percent")
) {

  check_tibble(data)
  metric <- rlang::arg_match(metric)

  req_cols <- dplyr::case_when(
    metric == "total_new" ~ c("property_name", "current_total_new", "prior_total_new"),
    metric == "total_renewals" ~ c("property_name", "current_total_renewals", "prior_total_renewals"),
    metric == "total_leases" ~ c("property_name", "current_total_leases", "prior_total_leases"),
    metric == "preleased_percent" ~ c("property_name", "current_preleased_percent", "prior_preleased_percent")
  )

  validate_col_names(data, req_cols)

  data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    tidyr::pivot_longer(cols = -"property_name", names_to = "type", values_to = "value") |>
    dplyr::mutate(
      type = dplyr::case_match(
        .data$type,
        "current_total_new" ~ "Current New",
        "prior_total_new" ~ "Prior New",
        "current_total_renewals" ~ "Current Renewals",
        "prior_total_renewals" ~ "Prior Renewals",
        "current_total_leases" ~ "Current Leases",
        "prior_total_leases" ~ "Prior Leases",
        "current_preleased_percent" ~ "Current Pre-Lease %",
        "prior_preleased_percent" ~ "Prior Pre-Lease %"
      )
    )
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr select ungroup summarize group_by mutate case_when
#' @importFrom rlang arg_match
#' @importFrom tidyselect all_of
prep_occupancy_chart_data <- function(data, target = 0.9, by = c("property", "partner")) {

  check_tibble(data)
  check_percent(target)
  by <- rlang::arg_match(by)
  req_cols <- if (by == "property") c("property_name", "current_occupancy") else c("investment_partner", "current_occupancy")
  validate_col_names(data, req_cols)
  chart_data <- data |> dplyr::select(tidyselect::all_of(req_cols))

  if (by == "partner") {
    chart_data <- chart_data |>
      dplyr::group_by(.data$investment_partner) |>
      dplyr::summarize(current_occupancy = mean(.data$current_occupancy, na.rm = TRUE), .groups = "drop") |>
      dplyr::ungroup()
  }

  chart_data |>
    dplyr::mutate(
      bar_color = dplyr::case_when(
        .data$current_occupancy == 1 ~ gmh_colors("success"),
        .data$current_occupancy < .env$target ~ gmh_colors("danger"),
        TRUE ~ gmh_colors("primary")
      )
    )
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr mutate select
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of starts_with
prep_velocity_chart_data <- function(data) {

  req_cols <- c("property_name", "vel_90", "vel_95", "vel_100")
  validate_col_names(data, req_cols)

  data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("vel_"),
      names_to = "velocity_type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      velocity_type = paste0(stringr::str_remove(.data$velocity_type, "vel_"), "%")
    )
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr mutate select case_when
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
prep_pre_lease_rates_chart_data <- function(data) {

  check_tibble(data)
  req_cols <- c("property_name", "current_preleased_percent", "prior_preleased_percent")
  validate_col_names(data, req_cols)
  data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    tidyr::pivot_longer(cols = -"property_name", names_to = "period", values_to = "preleased_percent") |>
    dplyr::mutate(
      period = dplyr::case_when(
        period == "current_preleased_percent" ~ "Current",
        period == "prior_preleased_percent" ~ "Prior"
      )
    )

}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr arrange ungroup summarize group_by select desc
#' @importFrom rlang arg_match0
#' @importFrom tidyselect all_of
prep_partner_distribution_chart_data <- function(data, metric = NULL) {

  check_tibble(data)

  if (is.null(metric)) metric <- "total_beds" else metric <- rlang::arg_match0(metric, .pre_lease_metrics)

  req_cols <- c("investment_partner", metric)
  validate_col_names(data, req_cols)

  data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    dplyr::group_by(.data$investment_partner) |>
    dplyr::summarize(
      total = sum(.data[[metric]], na.rm = TRUE),
      average = mean(.data[[metric]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$total))

}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr ungroup summarise group_by
#' @importFrom tidyr pivot_longer
prep_portfolio_summary_chart_data <- function(data) {

  check_tibble(data)

  req_cols <- c(
    "investment_partner",
    "current_occupancy",
    "current_preleased_percent",
    "current_total_renewals",
    "current_total_new",
    "current_occupied",
    "total_beds",
    "vel_95"
  )

  validate_col_names(data, req_cols)

  data |>
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
      cols = -"investment_partner",
      names_to = "metric",
      values_to = "value"
    )
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr mutate select recode
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of starts_with
prep_weekly_activity_chart_data <- function(data) {

  check_tibble(data)

  req_cols <- c(
    "property_name",
    "weekly_new",
    "weekly_renewal",
    "weekly_total"
  )

  validate_col_names(data, req_cols)

  data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("weekly"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        .data$metric,
        "weekly_new" = "New Leases",
        "weekly_renewal" = "Renewal Leases",
        "weekly_total" = "Total Leases"
      )
    )
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr mutate select recode
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of starts_with
prep_weekly_breakdown_chart_data <- function(data) {

  check_tibble(data)

  req_cols <- c(
    "property_name",
    "weekly_new",
    "weekly_renewal"
  )

  validate_col_names(data, req_cols)

  data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("weekly"),
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        .data$type,
        "weekly_new" = "New",
        "weekly_renewal" = "Renewal"
      )
    )
}

#' @rdname utils_charts
#' @export
#' @importFrom dplyr arrange select desc ungroup summarize group_by
#' @importFrom rlang arg_match
#' @importFrom tidyselect all_of
prep_yoy_variance_chart_data <- function(data, by = c("property", "partner")) {

  check_tibble(data)
  by <- rlang::arg_match(by)
  req_cols <- if (by == "property") {
    c("property_name", "yoy_variance_count", "yoy_variance_percent")
  } else {
    c("investment_partner", "yoy_variance_count", "yoy_variance_percent")
  }

  validate_col_names(data, req_cols)

  chart_data <- data |>
    dplyr::select(tidyselect::all_of(req_cols)) |>
    dplyr::arrange(.data$yoy_variance_percent)

  if (by == "partner") {
    chart_data <- chart_data |>
      dplyr::group_by(.data$investment_partner) |>
      dplyr::summarize(
        yoy_variance_count = sum(.data$yoy_variance_count, na.rm = TRUE),
        yoy_variance_percent = mean(.data$yoy_variance_percent, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(.data$yoy_variance_percent)
  }

  chart_data

}

# internal --------------------------------------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.pre_lease_metrics <- c(
  "total_beds",
  "model_beds",
  "beds_left",
  "current_occupied",
  "current_occupancy",
  "current_total_new",
  "current_total_renewals",
  "current_total_leases",
  "current_preleased_percent",
  "prior_total_new",
  "prior_total_renewals",
  "prior_total_leases",
  "prior_preleased_percent",
  "yoy_variance_count",
  "yoy_variance_percent",
  "weekly_new",
  "weekly_renewal",
  "weekly_total",
  "vel_90",
  "vel_95",
  "vel_100"
)
