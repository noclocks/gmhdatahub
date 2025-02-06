
tbl_survey_fees <- function(fees_data) {

  req_cols <- c(
    "fee_name",
    "fee_amount",
    "fee_frequency"
  )

  validate_col_names(fees_data, req_cols)

  count_cols <- c("fee_name")
  sum_cols <- c("fee_amount")
  avg_cols <- c()
  exclude_cols <- c(
    "property_id",
    "competitor_id",
    "property_name",
    "leasing_week_id",
    "property_name",
    "created_at",
    "updated_at",
    "created_by",
    "updated_by"
  )

  totals <- derive_tbl_totals(fees_data, count_cols, sum_cols, avg_cols)
  default_col_def <- reactable_default_col_def(totals = totals)

  prop_name <- unique(fees_data$property_name)[[1]]
  tbl_title <- "Property Fees"
  tbl_subtitle <- paste0("Summary of the Fees Associated with the Property: ", prop_name)
  tbl_src <- "Source: Survey Data"

  excl_col_defs <- purrr::map(
    exclude_cols,
    function(col) {
      if (col %in% names(fees_data)) {
        reactable::colDef(show = FALSE)
      }
    }
  ) |>
    purrr::compact() |>
    setNames(exclude_cols)

  incl_col_defs <- list(
    fee_name = reactable::colDef(
      name = "Fee Name",
      minWidth = 200,
      aggregate = "count",
      align = "center",
      footer = "Total",
      cell = reactablefmtr::pill_buttons(
        data = fees_data,
        colors = c(gmh_colors("primary"))
      )
    ),
    fee_amount = reactable::colDef(
      name = "Amount",
      format = reactable::colFormat(currency = "USD"),
      align = "center",
      aggregate = "sum",
      cell = reactablefmtr::data_bars(
        data = fees_data,
        fill_color = c("#bfe4d3", "#28a745"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      )
    ),
    # Monthly vs. Annual
    fee_frequency = reactable::colDef(
      name = "Frequency",
      align = "center",
      aggregate = "count",
      cell = reactablefmtr::pill_buttons(
        data = fees_data,
        colors = c("#007bff", "#0056b3")
      )
    )
  )

  # merge lists
  col_defs <- purrr::list_flatten(list(excl_col_defs, incl_col_defs))

  reactable::reactable(
    data = fees_data,
    theme = reactable_theme(),
    defaultColDef = reactable_default_col_def(totals = totals),
    columns = col_defs,
    filterable = TRUE,
    searchable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    defaultPageSize = nrow(rents_data),
    defaultSorted = list("fee_name" = "asc"),
    showPagination = FALSE,
    showSortable = TRUE,
    showSortIcon = TRUE
  ) #|>
  # reactablefmtr::add_title(tbl_title) |>
  # reactablefmtr::add_subtitle(tbl_subtitle) |>
  # reactablefmtr::add_source(tbl_src)
}


tbl_survey_fees_empty <- function() {
  tibble::tribble(
    ~"Fees", ~"Amount", ~"Frequency",
    "Application Fee", "$0", "Monthly",
    "Administration Fee", "$0", "Monthly",
    "Fee Structure", "Both Fees Waived", "Monthly",
    "Utility Set Up Fee", "$0", "Monthly",
    "Utility Deposit", "$0", "Monthly",
    "Amenity Fee", "$0", "Monthly",
    "Common Area Fee", "$0", "Monthly",
    "Smart Home Fee", "$0", "Monthly",
    "Restoration Fee", "$0", "Monthly",
    "Security Deposit", "$1000", "Monthly",
    "Pet Fee", "$0", "Monthly",
    "Pet Deposit", "$0", "Monthly",
    "Pet Rent", "$0", "Monthly"
  )
}
