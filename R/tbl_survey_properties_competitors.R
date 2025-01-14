#' Create Properties and Competitors Table
#' @param data Data frame containing properties and competitors
#' @noRd
tbl_survey_properties_competitors <- function(data) {

  validate_col_names(
    data,
    c(
      "name",
      "type",
      "address",
      "last_survey_date",
      "survey_status"
    )
  )

  tbl_data <- data |>
    dplyr::mutate(
      last_survey_date = format(last_survey_date, "%b %d, %Y"),
      last_survey_date = ifelse(is.na(last_survey_date), "N/A", last_survey_date),
      date_colors = ifelse(last_survey_date == "N/A", "darkgray", gmh_colors("primary")),
      status_colors = dplyr::case_when(
        survey_status == "Overdue" ~ "#ff0000",
        survey_status == "Completed" ~ "#00ff00",
        survey_status == "Pending" ~ "#ffff00"
      )
    )

  reactable::reactable(
    tbl_data,
    compact = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    striped = TRUE,
    columns = list(
      name = reactable::colDef(
        name = "Name",
        minWidth = 200,
        cell = reactablefmtr::merge_column(
          data = tbl_data,
          merged_name = "type",
          merged_position = "below",
          merged_size = 14,
          size = 16,
          color = "#333333",
          spacing = 1
        )
      ),
      type = reactable::colDef(show = FALSE),
      address = reactable::colDef(
        name = "Address",
        minWidth = 350
      ),
      last_survey_date = reactable::colDef(
        name = "Last Survey Date",
        minWidth = 150,
        cell = reactablefmtr::pill_buttons(
          data = tbl_data,
          opacity = 0.8,
          color_ref = "date_colors",
        )
      ),
      survey_status = reactable::colDef(
        name = "Survey Status",
        minWidth = 150,
        cell = reactablefmtr::pill_buttons(
          data = tbl_data,
          color_ref = "status_colors",
          box_shadow = TRUE
        )
      ),
      occupancy = reactable::colDef(show = FALSE),
      avg_rent = reactable::colDef(show = FALSE),
      date_colors = reactable::colDef(show = FALSE),
      status_colors = reactable::colDef(show = FALSE)
    )
  )

}
