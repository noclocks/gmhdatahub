chart_partner_distribution <- function(data, metric = c("total_beds"), id = NULL, ...) {

  validate_col_names(
    data,
    c(
      "investment_partner",
      "total_beds"
    )
  )

  chart_data <- data |>
    dplyr::select(
      investment_partner,
      total_beds
    ) |>
    dplyr::group_by(
      investment_partner
    ) |>
    dplyr::summarize(
      total_beds = sum(total_beds, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::desc(total_beds)
    )

  apexcharter::apex(
    data = chart_data,
    mapping = apexcharter::aes(
      x = investment_partner,
      y = total_beds
    ),
    type = "donut"
  ) |>
    apexcharter::ax_title(
      text = list("Total Beds by Investment Partner")
    ) |>
    apexcharter::ax_dataLabels(enabled = TRUE) |>
    apexcharter::ax_legend(position = "right") |>
    apexcharter::ax_chart(toolbar = list(show = TRUE)) |>
    apexcharter::ax_plotOptions(
      pie = list(
        dataLabels = list(
          formatter = apexcharter::JS("function(value, { seriesIndex, dataPointIndex, w }) {
              return w.config.labels[dataPointIndex] + ': ' + value.toLocaleString() + ' beds'
            }")
        )
      )
    ) |>
    apexcharter::ax_colors(
      chart_colors(1:5, 8)
    )

}
