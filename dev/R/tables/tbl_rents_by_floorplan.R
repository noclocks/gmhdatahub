tbl_rents_by_floorplan <- function(rents_data) {
  req_cols <- c(
    "floorplan_type",
    "floorplan_id",
    "number_of_beds",
    "number_of_baths",
    "total_units_count",
    "available",
    "market_rent_per_bed",
    "market_rent_per_square_foot",
    "concessions_gift_card",
    "concessions_one_time_rent",
    "concessions_monthly_rent",
    "effective_rent_per_bed",
    "effective_rent_per_square_foot",
    "expenses_furniture",
    "expenses_tv",
    "expenses_electricity_gas",
    "expenses_water",
    "expenses_cable_internet",
    "expenses_trash_valet",
    "expenses_parking",
    "expenses_total",
    "bundled_rent_per_bed",
    "bundled_rent_per_square_foot"
  )

  validate_col_names(rents_data, req_cols)

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

  count_cols <- c(
    "floorplan_type",
    "floorplan_id"
  )

  sum_cols <- c(
    "number_of_beds",
    "number_of_baths",
    "total_units_count",
    "concessions_gift_card",
    "concessions_one_time_rent",
    "concessions_monthly_rent",
    "expenses_furniture",
    "expenses_tv",
    "expenses_electricity_gas",
    "expenses_water",
    "expenses_cable_internet",
    "expenses_trash_valet",
    "expenses_parking",
    "expenses_total"
  )

  avg_cols <- c(
    "square_feet_per_bed",
    "market_rent_per_bed",
    "market_rent_per_square_foot",
    "effective_rent_per_bed",
    "effective_rent_per_square_foot",
    "bundled_rent_per_bed",
    "bundled_rent_per_square_foot"
  )

  totals <- derive_tbl_totals(
    data = rents_data,
    count_cols = count_cols,
    sum_cols = sum_cols,
    avg_cols = avg_cols
  )

  exclude_cols <- c(
    "property_id",
    "competitor_id",
    "leasing_week_id",
    "property_name",
    "square_feet",
    "created_at",
    "updated_at",
    "created_by",
    "updated_by"
  )

  # prop_name <- unique(rents_data$property_name)[[1]]
  # tbl_title <- "Rents by Floorplan"
  # tbl_subtitle <- paste0("Summary of Rents by Floorplan for Property: ", prop_name)
  # tbl_src <- "Source: Rent Data"

  col_defs <- list(
    property_id = if ("property_id" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    competitor_id = if ("competitor_id" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    leasing_week_id = if ("leasing_week_id" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    property_name = if ("property_name" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    square_feet = if ("square_feet" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    created_at = if ("created_at" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    updated_at = if ("updated_at" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    created_by = if ("created_by" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    updated_by = if ("updated_by" %in% colnames(rents_data)) {
      reactable::colDef(show = FALSE)
    } else {
      NULL
    },
    floorplan_type = reactable::colDef(
      name = "Unit Type",
      minWidth = 130,
      style = list(fontWeight = "600", background = "var(--rt-base-background)"),
      footer = "Total/Average"
    ),
    floorplan_id = reactable::colDef(
      name = "Floorplan",
      minWidth = 120,
      footer = ""
    ),
    number_of_beds = reactable::colDef(
      name = "Beds",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      align = "center",
      aggregate = "sum"
    ),
    number_of_baths = reactable::colDef(
      name = "Baths",
      format = reactable::colFormat(digits = 1, separators = TRUE),
      align = "center",
      aggregate = "sum"
    ),
    total_units_count = reactable::colDef(
      name = "Total Units",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      align = "center",
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = rents_data, colors = c("#E5F5E0", "#31A354"))
    ),
    square_feet_per_bed = reactable::colDef(
      name = "Square Feet/Bed",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      align = "center",
      aggregate = "mean",
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#c5e5fc", "#2C3E50"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("%.0f", value),
        max_value = max(rents_data$square_feet_per_bed, na.rm = TRUE)
      )
    ),
    available = reactable::colDef(
      name = "Available",
      align = "center",
      cell = function(value) {
        status_color <- if (value) "#28a745" else "#dc3545"
        status_txt <- if (value) "Yes" else "No"
        htmltools::tags$div(
          style = list(
            color = "white",
            background = status_color,
            borderRadius = "20px",
            padding = "4px 12px",
            display = "inline-block",
            fontWeight = "600"
          ),
          status_txt
        )
      }
    ),
    market_rent_per_bed = reactable::colDef(
      name = "Per Bed",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
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
        data = rents_data,
        fill_color = c("#bfe4d3", "#28a745"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("$%.2f", value)
      ),
      minWidth = 150
    ),
    concessions_gift_card = reactable::colDef(
      name = "Gift Card",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    concessions_one_time_rent = reactable::colDef(
      name = "One-Time Rent",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    concessions_monthly_rent = reactable::colDef(
      name = "Monthly Rent",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    effective_rent_per_bed = reactable::colDef(
      name = "Per Bed",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
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
        data = rents_data,
        fill_color = c("#c5e5fc", "#17a2b8"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) sprintf("$%.2f", value)
      ),
      minWidth = 150
    ),
    expenses_furniture = reactable::colDef(
      name = "Furniture",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_tv = reactable::colDef(
      name = "TV",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_electricity_gas = reactable::colDef(
      name = "Electricity/Gas",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_water = reactable::colDef(
      name = "Water",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_cable_internet = reactable::colDef(
      name = "Cable/Internet",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_trash_valet = reactable::colDef(
      name = "Trash/Valet",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_parking = reactable::colDef(
      name = "Parking",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    expenses_total = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
        fill_color = c("#f8d7da", "#dc3545"),
        background = "#f7f9fc",
        text_position = "outside-end",
        number_fmt = function(value) paste0("$", format(round(value), big.mark = ","))
      ),
      minWidth = 100
    ),
    bundled_rent_per_bed = reactable::colDef(
      name = "Per Bed",
      format = reactable::colFormat(
        prefix = "$",
        digits = 0,
        separators = TRUE
      ),
      cell = reactablefmtr::data_bars(
        data = rents_data,
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
        data = rents_data,
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
      columns = c("floorplan_type", "floorplan_id", "number_of_beds", "number_of_baths", "total_units_count", "square_feet_per_bed", "available"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Market Rent",
      columns = c("market_rent_per_bed", "market_rent_per_square_foot"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Concessions",
      columns = c("concessions_gift_card", "concessions_one_time_rent", "concessions_monthly_rent"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Effective Rent",
      columns = c("effective_rent_per_bed", "effective_rent_per_square_foot"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Expenses",
      columns = c("expenses_furniture", "expenses_tv", "expenses_electricity_gas", "expenses_water", "expenses_cable_internet", "expenses_trash_valet", "expenses_parking", "expenses_total"),
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
    data = rents_data,
    theme = tbl_theme,
    # height = "800px",
    defaultColDef = reactable_default_col_def(),
    columns = col_defs,
    columnGroups = col_groups,
    filterable = TRUE,
    searchable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    compact = TRUE,
    defaultPageSize = 50,
    defaultSorted = list("floorplan_type" = "asc"),
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
# tbl_rents_by_floorplan(rents_by_floorplan_tbl)
