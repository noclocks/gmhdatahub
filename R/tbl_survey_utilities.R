tbl_survey_utilities <- function(utilities_data) {

  tbl_data <- utilities_data |>
    dplyr::select(
      utility_name,
      utility_included,
      utility_available,
      utility_capped,
      utility_per,
      utility_allowance
    )

  reactable::reactable(
    data = tbl_data,
    defaultPageSize = nrow(tbl_data),
    searchable = TRUE,
    highlight = TRUE,
    striped = TRUE,
    bordered = TRUE,
    theme = reactable_theme(),
    columns = list(
      utility_name = reactable::colDef(
        name = "Utility",
        align = "center",
        cell = reactablefmtr::pill_buttons(data = tbl_data, colors = gmh_colors("primary"))
      ),
      utility_included = reactable::colDef(
        name = "Included?",
        align = "center",
        cell = function(value) format_boolean(value)
      ),
      utility_available = reactable::colDef(
        name = "Available?",
        align = "center",
        cell = function(value) format_boolean(value)
      ),
      utility_capped = reactable::colDef(
        name = "Capped?",
        align = "center",
        cell = function(value) format_boolean(value)
      ),
      utility_per = reactable::colDef(
        name = "Per Bed/Unit",
        align = "center",
        cell = reactablefmtr::pill_buttons(data = tbl_data, colors = gmh_colors("primary"))
      ),
      utility_allowance = reactable::colDef(
        name = "Allowance ($)",
        align = "center",
        cell = reactablefmtr::pill_buttons(
          data = tbl_data,
          colors = c(
            gmh_colors("secondary"),
            gmh_colors("primary")
          ),
          number_fmt = scales::dollar
        )
      )
    )
  )

}
