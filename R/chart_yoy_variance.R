chart_yoy_variance <- function(data, by = c("property", "partner"), id = NULL, ...) {
  by <- match.arg(by)

  validate_col_names(
    data,
    c(
      "property_name",
      "investment_partner",
      "yoy_variance_percent"
    )
  )

  if (by == "property") {
    aes_x <- "property_name"
    chart_subtitle <- "Year-Over-Year Variance by Property"
    x_axis_title <- "Property"
    x_axis_categories <- unique(data$property_name)

    chart_data <- dplyr::select(data, property_name, yoy_variance_percent) |>
      dplyr::arrange(dplyr::desc(abs(yoy_variance_percent)))
  } else {
    aes_x <- "investment_partner"
    chart_subtitle <- "Year-Over-Year Variance by Investment Partner"
    x_axis_title <- "Investment Partner"
    x_axis_categories <- unique(data$investment_partner)

    chart_data <- data |>
      dplyr::group_by(investment_partner) |>
      dplyr::summarize(
        yoy_variance_percent = mean(yoy_variance_percent, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(abs(yoy_variance_percent)))
  }

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(x = if (by == "property") property_name else investment_partner, y = yoy_variance_percent),
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
        if (x < 0) "#E74C3C" else "#2ECC71"
      }))
    ) |>
    apexcharter::ax_grid(
      show = TRUE
    ) |>
    apexcharter::ax_tooltip(
      y = list(
        formatter = apexcharter::format_num(".0%")
      )
    ) |>
    apexcharter::ax_dataLabels(
      enabled = TRUE,
      formatter = apexcharter::format_num(".0%")
    ) |>
    apexcharter::ax_legend(
      show = FALSE
    )
}
