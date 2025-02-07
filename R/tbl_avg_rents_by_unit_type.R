tbl_avg_rents_by_unit_type <- function(rents_data) {
  req_cols <- c(
    "floorplan_type",
    "floorplan_id",
    "square_feet",
    "number_of_beds",
    "number_of_baths",
    "total_units_count",
    "available",
    "market_rent_per_bed",
    "market_rent_per_square_foot",
    # "concessions_gift_card",
    # "concessions_one_time_rent",
    # "concessions_monthly_rent",
    "effective_rent_per_bed",
    "effective_rent_per_square_foot",
    # "expenses_furniture",
    # "expenses_tv",
    # "expenses_electricity_gas",
    # "expenses_water",
    # "expenses_cable_internet",
    # "expenses_trash_valet",
    # "expenses_parking",
    # "expenses_total",
    "bundled_rent_per_bed",
    "bundled_rent_per_square_foot"
  )

  validate_col_names(rents_data, req_cols)

  # derive template/skeleton
  all_unit_types_tbl <- tibble::tibble(
    floorplan_type = c("Studio", "1 Bedroom", "2 Bedroom", "3 Bedroom", "4 Bedroom", "5 Bedroom", "6 Bedroom")
  )

  # calculate unit summaries
  unit_summary <- rents_data |>
    dplyr::group_by(floorplan_type) |>
    dplyr::summarize(
      count = dplyr::n(),
      square_feet_per_bed = mean(square_feet_per_bed, na.rm = TRUE),
      available = if (dplyr::n() == sum(.data$available, na.rm = TRUE)) "Yes" else "No",
      market_rent_per_bed = mean(market_rent_per_bed, na.rm = TRUE),
      market_rent_per_square_foot = mean(market_rent_per_square_foot, na.rm = TRUE),
      effective_rent_per_bed = mean(effective_rent_per_bed, na.rm = TRUE),
      effective_rent_per_square_foot = mean(effective_rent_per_square_foot, na.rm = TRUE),
      bundled_rent_per_bed = mean(bundled_rent_per_bed, na.rm = TRUE),
      bundled_rent_per_square_foot = mean(bundled_rent_per_square_foot, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::right_join(
      all_unit_types_tbl,
      by = "floorplan_type"
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        tidyr::replace_na,
        replace = 0
      ),
      available = tidyr::replace_na(available, "No")
    )

  # calculate property average
  property_avg <- unit_summary |>
    dplyr::summarise(
      floorplan_type = "Property Average",
      square_feet_per_bed = weighted.mean(square_feet_per_bed, count),
      available = if (all(unit_summary$available == "No")) "No" else "Yes",
      market_rent_per_bed = weighted.mean(market_rent_per_bed, count),
      market_rent_per_square_foot = weighted.mean(market_rent_per_square_foot, count),
      effective_rent_per_bed = weighted.mean(effective_rent_per_bed, count),
      effective_rent_per_square_foot = weighted.mean(effective_rent_per_square_foot, count),
      bundled_rent_per_bed = weighted.mean(bundled_rent_per_bed, count),
      bundled_rent_per_square_foot = weighted.mean(bundled_rent_per_square_foot, count),
      count = sum(count)
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        function(x) {
          round(x, 2) |> prettyNum(big.mark = ",")
        }
      ),
      market_rent_per_bed = paste0("$", market_rent_per_bed),
      market_rent_per_square_foot = paste0("$", market_rent_per_square_foot),
      effective_rent_per_bed = paste0("$", effective_rent_per_bed),
      effective_rent_per_square_foot = paste0("$", effective_rent_per_square_foot),
      bundled_rent_per_bed = paste0("$", bundled_rent_per_bed),
      bundled_rent_per_square_foot = paste0("$", bundled_rent_per_square_foot)
    )

  tbl_data <- unit_summary

  header_style <- list(
    background = "#0e2b4c",
    color = "white",
    "&:hover[aria-sort]" = list(background = "#173d6b"),
    "&[aria-sort]" = list(background = "#173d6b"),
    borderRight = "1px solid #173d6b"
  )

  header_group_style <- list(
    background = "#0e2b4c",
    color = "white",
    borderTop = "2px solid #173d6b",
    borderBottom = "2px solid #173d6b"
  )

  footer_style <- list(
    background = "#0e2b4c",
    color = "white",
    fontWeight = "600"
  )

  default_col_def <- reactable::colDef(
    align = "center",
    headerVAlign = "center",
    vAlign = "center",
    format = reactable::colFormat(separators = TRUE),
    headerStyle = htmltools::css(
      font_weight = 600,
      border_bottom = "2px solid black"
    ),
    footerStyle = htmltools::css(
      font_weight = 600,
      border_top = "2px solid black"
    ),
    footer = function(values, col_name) {
      property_avg[[col_name]]
    }
  )

  # prop_name <- unique(rents_data$property_name)[[1]]
  # tbl_title <- "Average Rents by Unit Type/Floorplan"
  # tbl_subtitle <- paste0("Derived Rent Averages for Property: ", prop_name)
  # tbl_src <- "Source: Rent Data"

  col_defs <- list(
    floorplan_type = reactable::colDef(
      name = "Unit Type",
      minWidth = 130,
      style = list(fontWeight = "600", background = "var(--rt-base-background)"),
      footer = "Property Average"
    ),
    count = reactable::colDef(
      name = "Total Units",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      align = "center",
      aggregate = "sum"
    ),
    square_feet_per_bed = reactable::colDef(
      name = "Square Feet/Bed",
      align = "center",
      aggregate = "mean",
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#c5e5fc", "#2C3E50"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("%.0f", value),
        max_value = max(tbl_data$square_feet_per_bed, na.rm = TRUE)
      )
    ),
    available = reactable::colDef(
      name = "Available",
      align = "center",
      cell = reactablefmtr::pill_buttons(data = tbl_data)
    ),
    market_rent_per_bed = reactable::colDef(
      name = "Per Bed",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#bfe4d3", "#28a745"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 150
    ),
    market_rent_per_square_foot = reactable::colDef(
      name = "Per Square Foot",
      format = reactable::colFormat(
        prefix = "$",
        digits = 2,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#bfe4d3", "#28a745"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("$%.2f", value)
      ),
      minWidth = 150
    ),
    effective_rent_per_bed = reactable::colDef(
      name = "Per Bed",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#c5e5fc", "#17a2b8"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 150
    ),
    effective_rent_per_square_foot = reactable::colDef(
      name = "Per Square Foot",
      format = reactable::colFormat(
        prefix = "$",
        digits = 2,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#c5e5fc", "#17a2b8"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("$%.2f", value)
      ),
      minWidth = 150
    ),
    bundled_rent_per_bed = reactable::colDef(
      name = "Per Bed",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#c5e5fc", "#17a2b8"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 150
    ),
    bundled_rent_per_square_foot = reactable::colDef(
      name = "Per Square Foot",
      format = reactable::colFormat(
        prefix = "$",
        digits = 2,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = tbl_data,
        fill_color = c("#c5e5fc", "#17a2b8"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("$%.2f", value)
      ),
      minWidth = 150
    )
  )

  col_groups <- list(
    reactable::colGroup(
      name = "Unit Details",
      columns = c("count", "square_feet_per_bed", "available"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Market Rent",
      columns = c("market_rent_per_bed", "market_rent_per_square_foot"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Effective Rent",
      columns = c("effective_rent_per_bed", "effective_rent_per_square_foot"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Bundled Rent",
      columns = c("bundled_rent_per_bed", "bundled_rent_per_square_foot"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    )
  )

  tbl_theme <- reactable::reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(fontFamily = "Segoe UI", fontSize = "14px"),
    searchInputStyle = list(width = "100%"),
    headerStyle = list(
      background = "#0e2b4c",
      color = "white",
      "&:hover[aria-sort]" = list(background = "#173d6b"),
      "&[aria-sort]" = list(background = "#173d6b"),
      borderRight = "1px solid #173d6b"
    ),
    groupHeaderStyle = list(
      background = "#0e2b4c",
      color = "white",
      borderTop = "2px solid #173d6b",
      borderBottom = "2px solid #173d6b"
    ),
    footerStyle = list(
      background = "#0e2b4c",
      color = "white",
      fontWeight = "600"
    )
  )

  reactable::reactable(
    data = tbl_data,
    theme = tbl_theme,
    # height = "800px",
    defaultColDef = default_col_def,
    columns = col_defs,
    columnGroups = col_groups,
    filterable = TRUE,
    searchable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    defaultPageSize = 50,
    showPagination = FALSE,
    showSortable = TRUE,
    showSortIcon = TRUE
  ) #|>
  # reactablefmtr::add_title(tbl_title) |>
  # reactablefmtr::add_subtitle(tbl_subtitle) |>
  # reactablefmtr::add_source(tbl_src)
}


# rents_by_floorplan_tbl <- db_read_tbl(pool, "survey.rents_by_floorplan")
#
# tbl_avg_rents_by_unit_type(rents_by_floorplan_tbl)
