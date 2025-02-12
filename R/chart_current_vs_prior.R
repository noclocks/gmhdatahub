chart_current_vs_prior <- function(data, metric = c("leases", "renewals", "prelease"), id = NULL, ...) {
  metric <- match.arg(metric)

  req_cols <- switch(metric,
    leases = c("property_name", "current_total_leases", "prior_total_leases"),
    renewals = c("property_name", "current_total_renewals", "prior_total_renewals"),
    prelease = c("property_name", "current_preleased_percent", "prior_preleased_percent")
  )

  validate_col_names(data, req_cols)

  title <- switch(metric,
    leases = "Current vs Prior Total Leases",
    renewals = "Current vs Prior Total Renewals",
    prelease = "Current vs Prior Preleased Percent"
  )

  subtitle <- switch(metric,
    leases = "Comparison of current and prior total leases by property.",
    renewals = "Comparison of current and prior total renewals by property.",
    prelease = "Comparison of current and prior pre-leased percent by property."
  )

  x_axis_title <- "Property Name"

  y_axis_title <- switch(metric,
    leases = "Number of Leases",
    renewals = "Number of Renewals",
    prelease = "Pre-Lease Percent"
  )

  chart_data <- data |>
    dplyr::select(dplyr::all_of(req_cols)) |>
    tidyr::pivot_longer(
      cols = -property_name,
      names_to = "type",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::recode(
        type,
        current_total_leases = "Current Total Leases",
        prior_total_leases = "Prior Total Leases",
        current_total_renewals = "Current Total Renewals",
        prior_total_renewals = "Prior Total Renewals",
        current_preleased_percent = "Current Preleased Percent",
        prior_preleased_percent = "Prior Preleased Percent"
      )
    )

  hold <- apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = property_name,
      y = value,
      fill = type
    ),
    type = "column",
    elementId = id
  ) |>
    apexcharter::ax_title(
      text = title
    ) |>
    apexcharter::ax_subtitle(
      text = subtitle
    ) |>
    apexcharter::ax_xaxis(
      title = list(text = x_axis_title)
    ) |>
    apexcharter::ax_colors(
      chart_colors(1:2)
    )

  if (metric %in% c("leases", "renewals")) {
    out <- hold |>
      apexcharter::ax_yaxis(
        title = list(text = y_axis_title),
        labels = list(
          formatter = apexcharter::JS(
            "function(val) {return val.toFixed(0)}"
          )
        )
      )
  } else {
    out <- hold |>
      apexcharter::ax_yaxis(
        title = list(text = y_axis_title),
        labels = list(
          formatter = apexcharter::format_num(format = ".0%")
        )
      )
  }

  return(out)
}
