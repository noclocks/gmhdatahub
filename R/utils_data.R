
default_tbl_survey_utilities <- function() {
  tibble::tibble(
    utility_name = c(
      "Electricity",
      "Gas",
      "Water",
      "Cable/Satellite",
      "Internet",
      "Trash Service",
      "Valet Trash",
      "Recycling"
    ),
    utility_included = FALSE,
    utility_available = FALSE,
    utility_capped = FALSE,
    utility_per = "Bed",
    utility_allowance = 0.00,
    utility_category = c(
      "Core",
      "Core",
      "Core",
      "Other",
      "Other",
      "Other",
      "Other",
      "Other"
    )
  )
}

default_tbl_survey_parking <- function() {
  tibble::tibble(
    parking_type = get_survey_choices("parking", "parking_type"),
    is_required = FALSE,
    is_included = FALSE,
    amount = 0.00
  )
}

default_tbl_survey_notes <- function() {
  tibble::tibble(
    note_id = NA_integer_,
    note_type = NA_character_,
    note_actionable = as.logical(NA),
    note_status = NA_character_,
    note_tags = NA_character_,
    note_content = NA_character_
  )
}

default_tbl_survey_short_term_leases <- function() {
  tibble::tibble(
    term_months = c(5, 10),
    is_available = FALSE,
    premium = 0.00,
    quantity = 0
  )
}

default_tbl_survey_hours <- function() {
  tibble::tibble(
    day_of_week = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    ),
    open_time = c(
      rep("09:00", 5),
      rep("10:00", 2)
    ) |>
      as.POSIXct(format = "%H:%M") |>
      format("%I:%M %p"),
    close_time = c(
      rep("18:00", 5),
      rep("17:00", 2)
    ) |>
      as.POSIXct(format = "%H:%M") |>
      format("%I:%M %p")
  )
}

derive_tbl_totals <- function(data, count_cols = NULL, sum_cols = NULL, avg_cols = NULL) {
  cols <- c(count_cols, sum_cols, avg_cols)
  validate_col_names(data, cols)

  data |>
    dplyr::select(tidyselect::all_of(cols)) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::all_of(count_cols),
        function(x) {
          dplyr::n_distinct(x, na.rm = TRUE)
        }
      ),
      dplyr::across(
        tidyselect::all_of(sum_cols),
        function(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      dplyr::across(
        tidyselect::all_of(avg_cols),
        function(x) {
          mean(x, na.rm = TRUE)
        }
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(count_cols),
        function(x) {
          prettyNum(x, big.mark = ",")
        }
      ),
      dplyr::across(
        tidyselect::all_of(avg_cols),
        function(x) {
          round(x, 2) |>
            prettyNum(big.mark = ",")
        }
      ),
      dplyr::across(
        tidyselect::all_of(sum_cols),
        function(x) {
          prettyNum(x, big.mark = ",")
        }
      )
    )
}
