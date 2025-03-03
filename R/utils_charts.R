
#  ------------------------------------------------------------------------
#
# Title : Charting Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------

default_chart_options <- function(chart) {
  chart |>
    apexcharter::ax_chart(toolbar = list(show = TRUE)) |>
    apexcharter::ax_legend(show = TRUE, position = "top", horizontalAlign = "center") |>
    apexcharter::ax_grid(show = TRUE)
}

format_yaxis_labels <- function(is_percentage = FALSE) {
  if (is_percentage) {
    apexcharter::JS("function(value) { return (value * 100).toFixed(1) + '%' }")
  } else {
    apexcharter::JS("function(value) { return value.toFixed(0) }")
  }
}

prep_velocity_chart_data <- function(data, metrics) {
  if (length(metrics) == 0) {
    return(NULL)
  }

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
        metric == "current_occupancy" ~ "Current Occupancy",
        # metric == "current"
        TRUE ~ metric
      )
    )
}

prep_rates_chart_data <- function(data, rent_type, metrics) {
  rent_cols <- if (rent_type == "bed") {
    c("market_rent_per_bed", "effective_rent_per_bed", "bundled_rent_per_bed")[metrics %in% c("market", "effective", "bundled")]
  } else {
    c("market_rent_per_unit", "effective_rent_per_unit", "bundled_rent_per_unit")[metrics %in% c("market", "effective", "bundled")]
  }

  if (length(rent_cols) == 0) {
    return(NULL)
  }

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

pivot_chart_data <- function(
  data,
  id_col,
  value_cols,
  name_col = "type",
  value_col = "value"
) {

  data |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(value_cols),
      names_to = name_col,
      values_to = value_col
    )

}

