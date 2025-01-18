
#' Survey Status Table
#'
#' @description
#' This function renders a [reactable::reactable()] with survey status and
#' last survey date information by property and competitor for use
#' in [mod_survey_admin].
#'
#' @param data A data frame containing the following columns: `name`, `type`,
#'   `last_survey_date`, and `survey_status`.
#'
#' @returns
#' A [reactable::reactable()] object.#'
#'
#' @export
#'
#' @importFrom dplyr case_when transmute
#' @importFrom reactable reactable
#' @importFrom reactablefmtr merge_column pill_buttons
tbl_survey_status <- function(data) {

  validate_col_names(
    data,
    c(
      "name",
      "type",
      "last_survey_date",
      "survey_status"
    )
  )

  tbl_data <- data |>
    dplyr::transmute(
      name = .data$name,
      type = .data$type,
      survey_status = ifelse(
        .data$survey_status != "Complete" & difftime(Sys.Date(), .data$last_survey_date, units = "days") > 7,
        "Overdue",
        .data$survey_status
      ),
      last_survey_date = format(.data$last_survey_date, "%b %d, %Y"),
      last_survey_date = ifelse(is.na(.data$last_survey_date), "N/A", .data$last_survey_date),
      date_colors = ifelse(.data$last_survey_date == "N/A", gmh_colors("gray"), gmh_colors("primary")),
      status_colors = dplyr::case_when(
        .data$survey_status == "Overdue" ~ gmh_colors("danger"),
        .data$survey_status == "Initialized" ~ gmh_colors("warning"),
        .data$survey_status == "Draft" ~ gmh_colors("gray"),
        .data$survey_status == "Submitted" ~ gmh_colors("primary"),
        .data$survey_status == "Approved" ~ "yellow",
        .data$survey_status == "Complete" ~ gmh_colors("success")
      )
    )

  # ('Initialized', 'Draft', 'Submitted', 'Approved', 'Complete')

  reactable::reactable(
    tbl_data,
    compact = TRUE,
    filterable = TRUE,
    searchable = TRUE,
    striped = TRUE,
    selection = "single",
    defaultSelected = 1,
    onClick = "select",
    highlight = TRUE,
    bordered = TRUE,
    wrap = TRUE,
    columns = list(
      name = reactable::colDef(
        name = "Name",
        minWidth = 250,
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
      date_colors = reactable::colDef(show = FALSE),
      status_colors = reactable::colDef(show = FALSE)
    )
  )

}
