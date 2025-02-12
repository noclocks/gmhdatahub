tbl_property_leasing_summary <- function(data, ...) {
  req_cols <- c(
    "property_name",
    "total_beds",
    "current_occupied",
    "current_preleased_percent",
    "weekly_total",
    "beds_left"
  )

  validate_col_names(data, req_cols)

  tbl_data <- data |>
    dplyr::select(dplyr::all_of(req_cols)) |>
    dplyr::arrange(dplyr::desc(.data$total_beds))

  reactable::reactable(
    tbl_data,
    compact = TRUE,
    searchable = TRUE,
    striped = TRUE,
    defaultPageSize = 5,
    columns = list(
      property_name = reactable::colDef(name = "Property Name"),
      total_beds = reactable::colDef(name = "Total Beds"),
      current_occupied = reactable::colDef(name = "Occupied Beds"),
      current_preleased_percent = reactable::colDef(
        name = "Pre-leased %",
        format = reactable::colFormat(percent = TRUE, digits = 1)
      ),
      weekly_total = reactable::colDef(name = "Weekly Leases"),
      beds_left = reactable::colDef(name = "Beds Available")
    )
  ) |>
    reactablefmtr::add_title("Pre-Lease Property Summary") |>
    reactablefmtr::add_subtitle("Summary of Leasing Metrics by Property") |>
    reactablefmtr::add_source(source = "Entrata API", font_style = "italic", font_color = "gray")
}
