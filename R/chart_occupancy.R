chart_occupancy <- function(data, target = 0.9, by = c("property", "partner"), id = NULL, ...) {

  by <- match.arg(by)

  validate_col_names(
    data,
    c(
      "property_name",
      "investment_partner",
      "current_occupancy",
      "current_occupied"
    )
  )

  if (!is.numeric(target)) {
    cli::cli_abort("{.arg target} must be numeric.")
  }

  if (target < 0 | target > 1) {
    cli::cli_abort("{.arg target} must be between 0 and 1.")
  }

  chart_data <- data |>
    dplyr::select(
      property_name,
      investment_partner,
      current_occupancy,
      current_occupied
    )

  if (by == "partner") {

    chart_data <- chart_data |>
      dplyr::group_by(investment_partner) |>
      dplyr::summarize(
        current_occupancy = mean(current_occupancy, na.rm = TRUE),
      ) |>
      dplyr::ungroup()

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

  chart_data <- dplyr::rename(
    chart_data,
    "Current Occupancy" = current_occupancy
  )

  apexcharter::apex(
    data = chart_data,
    mapping = aes(
      x = !!rlang::sym(aes_x),
      y = `Current Occupancy`
    ),
    type = "column",
    elementId = id
  ) |>
    apexcharter::ax_title(
      text = "Occupancy Percentage",
      align = "center"
    ) |>
    apexcharter::ax_subtitle(
      text = chart_subtitle,
      align = "center"
    ) |>
    apexcharter::ax_plotOptions(
      toolbar = list(show = TRUE)
    ) |>
    apexcharter::ax_yaxis(
      title = list("Occupancy Rate (%)"),
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
          borderColor = "red",
          borderWidth = 2,
          borderType = "solid",
          label = list(
            text = paste0("🎯 Target: ", scales::percent(target)),
            style = list(
              color = "red"
            ),
            position = "right"
          )
        )
      )
    ) |>
    apexcharter::ax_legend(
      position = "top",
      align = "center"
    ) |>
    apexcharter::ax_tooltip(
      y = list(
        formatter = apexcharter::format_num(".0%")
      )
    ) |>
    apexcharter::ax_colors(
      chart_colors("primary")
    ) |>
    apexcharter::ax_dataLabels(
      enabled = TRUE,
      formatter = apexcharter::format_num(".0%")
    )
}

plot_occupancy <- function(data, ...) {
  plotly::plot_ly(
    data,
    x = ~property_name,
    y = ~current_occupancy,
    type = "bar",
    name = "Current Occupancy (%)"
  ) |>
    plotly::layout(
      title = "Occupancy Rate by Property",
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "Occupancy Rate (%)", tickformat = ",.1%"),
      margin = list(b = 120)
    )
}
