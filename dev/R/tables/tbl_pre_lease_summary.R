
#' @importFrom dplyr mutate across coalesce select
#' @importFrom htmltools tags
#' @importFrom reactable colDef colFormat JS colGroup reactable
#' @importFrom reactablefmtr pill_buttons icon_trend_indicator
#' @importFrom scales comma label_percent
#' @importFrom shiny actionButton icon
#' @importFrom tidyselect where everything
tbl_pre_lease_summary <- function(summary_data, ns = base::identity) {

  validate_col_names(
    data = summary_data,
    req_cols = c(
      "report_date",
      "property_id",
      "property_name",
      "investment_partner",
      "total_beds",
      "model_beds",
      "current_occupied",
      "current_occupancy",
      "current_total_new",
      "current_total_renewals",
      "current_total_leases",
      "current_preleased_percent",
      "prior_total_new",
      "prior_total_renewals",
      "prior_total_leases",
      "prior_preleased_percent",
      "yoy_variance_count",
      "yoy_variance_percent",
      "weekly_new",
      "weekly_renewal",
      "weekly_total",
      "weekly_percent_gained",
      "beds_left",
      "vel_90",
      "vel_95",
      "vel_100"
    )
  )

  count_cols <- c(
    "property_name",
    "investment_partner"
  )

  sum_cols <- c(
    "total_beds",
    "model_beds",
    "current_occupied",
    "current_total_new",
    "current_total_renewals",
    "current_total_leases",
    "prior_total_new",
    "prior_total_renewals",
    "prior_total_leases",
    "yoy_variance_count",
    "weekly_new",
    "weekly_renewal",
    "weekly_total",
    "beds_left",
    "vel_90",
    "vel_95",
    "vel_100"
  )

  avg_cols <- c(
    "current_occupancy",
    "current_preleased_percent",
    "prior_preleased_percent",
    "yoy_variance_percent",
    "weekly_percent_gained"
  )

  # coalesce all NA values to 0 for all numeric columns
  summary_data <- summary_data |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.numeric), ~dplyr::coalesce(.x, 0))
    )

  totals <- calculate_pre_lease_summary_totals(summary_data)

  default_col_def <- reactable::colDef(
    align = "center",
    headerVAlign = "center",
    vAlign = "center",
    format = reactable::colFormat(separators = TRUE),
    footer = function(values, col_name) {
      totals[[col_name]]
    }
  )

  col_defs <- list(
    .actions = reactable::colDef(
      name = "",
      sortable = FALSE,
      filterable = FALSE,
      width = 50,
      cell = function(value, index) {
        shiny::actionButton(
          inputId = ns(sprintf("edit_%d", index)),
          label = "",
          icon = shiny::icon("edit"),
          onclick = paste0(
            sprintf("Shiny.setInputValue('%s', %s)", ns("edit_row"), 'null'),
            '; ',
            sprintf("Shiny.setInputValue('%s', %d)", ns("edit_row"), index),
            ';'
          ),
          class = "btn-sm btn-primary"
        )
      },
      style = list(
        cursor = "pointer"
      )
    ),
    report_date = reactable::colDef(show = FALSE),
    property_id = reactable::colDef(show = FALSE),
    weeks_left_to_lease = reactable::colDef(show = FALSE),
    property_name = reactable::colDef(
      name = "Property Name",
      width = 250,
      footer = "Total/Average"#,
      # cell = function(value, index) {
      #   property_id <- summary_data$property_id[index]
      #   property_id <- if (!is.na(property_id)) property_id else "Unknown"
      #   htmltools::tags$div(
      #     htmltools::tags$div(style = "font-weight: 600;", value),
      #     htmltools::tags$div(style = "font-size: 0.75rem; color: #666;", property_id)
      #   )
      # }
    ),
    investment_partner = reactable::colDef(
      name = "Investment Partner",
      footer = "",
      width = 200,
      cell = reactablefmtr::pill_buttons(data = summary_data),
      style = list(borderRight = "1px solid #eee")
    ),
    model_beds = reactable::colDef(
      name = "Model Beds",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    total_beds = reactable::colDef(
      name = "Total Beds",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    current_occupied = reactable::colDef(
      name = "Occupied",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    current_occupancy = reactable::colDef(
      name = "Occupancy %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      width = 150,
      align = "center",
      vAlign = "center"
    ),
    current_total_new = reactable::colDef(
      name = "New",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    current_total_renewals = reactable::colDef(
      name = "Renewals",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    current_total_leases = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    current_preleased_percent = reactable::colDef(
      name = "Pre-Lease %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      width = 150,
      align = "center",
      vAlign = "center",
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
    ),
    prior_total_new = reactable::colDef(
      name = "New",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    prior_total_renewals = reactable::colDef(
      name = "Renewals",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    prior_total_leases = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    prior_preleased_percent = reactable::colDef(
      name = "Pre-Lease %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      width = 150,
      align = "center",
      vAlign = "center",
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
    ),
    yoy_variance_count = reactable::colDef(
      name = "Count",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::icon_trend_indicator(
        data = summary_data,
        icons = "arrow",
        colors = c(gmh_colors("danger"), gmh_colors("dark"), gmh_colors("success")),
        number_fmt = scales::comma,
        bold_text = TRUE,
        tooltip = TRUE
      )
    ),
    yoy_variance_percent = reactable::colDef(
      name = "Percent",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::icon_trend_indicator(
        data = summary_data,
        icons = "arrow",
        colors = c(gmh_colors("danger"), gmh_colors("dark"), gmh_colors("success")),
        number_fmt = scales::label_percent(accuracy = 0.1),
        bold_text = TRUE,
        tooltip = TRUE
      )
    ),
    weekly_new = reactable::colDef(
      name = "New",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    weekly_renewal = reactable::colDef(
      name = "Renewals",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    weekly_total = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    weekly_percent_gained = reactable::colDef(
      name = "Percent Gained",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::icon_trend_indicator(
        data = summary_data,
        icons = "arrow",
        colors = c(gmh_colors("danger"), gmh_colors("dark"), gmh_colors("success")),
        number_fmt = scales::label_percent(accuracy = 0.1),
        bold_text = TRUE,
        tooltip = TRUE
      )
    ),
    beds_left = reactable::colDef(
      name = "Beds Left",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    vel_90 = reactable::colDef(
      name = "90% Velocity",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    vel_95 = reactable::colDef(
      name = "95% Velocity",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    vel_100 = reactable::colDef(
      name = "100% Velocity",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    )
  )

  col_groups <- list(
    reactable::colGroup(
      name = "Property Information",
      columns = c(".actions", "property_name", "investment_partner"),
      sticky = "left",
      headerStyle = list(borderRight = "1px solid #eee")
    ),
    reactable::colGroup(
      name = "Beds",
      columns = c("total_beds", "model_beds"),
      headerStyle = list(borderRight = paste0("2px solid ", gmh_colors("light")))
    ),
    reactable::colGroup(
      name = "Current Year",
      columns = c(
        "current_occupied", "current_occupancy", "current_total_new",
        "current_total_renewals", "current_total_leases", "current_preleased_percent"
      ),
      headerStyle = list(borderRight = paste0("2px solid ", gmh_colors("light")))
    ),
    reactable::colGroup(
      name = "Prior Year",
      columns = c(
        "prior_total_new",
        "prior_total_renewals",
        "prior_total_leases",
        "prior_preleased_percent"
      ),
      headerStyle = list(borderRight = paste0("2px solid ", gmh_colors("light")))
    ),
    reactable::colGroup(
      name = "Year-Over-Year",
      columns = c("yoy_variance_count", "yoy_variance_percent"),
      headerStyle = list(borderRight = paste0("2px solid ", gmh_colors("light")))
    ),
    reactable::colGroup(
      name = "Weekly Activity (Prior Seven Days)",
      columns = c("weekly_new", "weekly_renewal", "weekly_total", "weekly_percent_gained"),
      headerStyle = list(borderRight = paste0("2px solid ", gmh_colors("light")))
    ),
    reactable::colGroup(
      name = "Leasing Velocity %",
      columns = c("beds_left", "vel_90", "vel_95", "vel_100"),
      headerStyle = list(borderRight = paste0("2px solid ", gmh_colors("light")))
    )
  )

  summary_data <- summary_data |>
    dplyr::mutate(
      .actions = NA
    ) |>
    dplyr::select(".actions", tidyselect::everything())

  reactable::reactable(
    data = summary_data,
    theme = pre_lease_reactable_theme(),
    defaultColDef = default_col_def,
    columns = col_defs,
    columnGroups = col_groups,
    filterable = TRUE,
    # searchable = TRUE,
    # resizable = TRUE,
    outlined = TRUE,
    compact = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    # wrap = FALSE,
    defaultPageSize = nrow(summary_data),
    defaultSorted = list("property_name" = "asc"),
    showPagination = FALSE,
    showSortable = TRUE,
    showSortIcon = TRUE
  )
}

#' @importFrom dplyr ungroup summarise group_by across select mutate
#' @importFrom htmltools tags
#' @importFrom reactable colDef colFormat reactable
#' @importFrom tidyselect all_of
tbl_entrata_pre_lease <- function(details_by_property_data, details_data) {

  sum_cols <- c(
    "excluded_units",
    "rentable_units",
    "occupied_units",
    "available_units",
    "current_preleased_new_count",
    "prior_preleased_new_count",
    "current_preleased_renewal_count",
    "prior_preleased_renewal_count",
    "current_preleased_total_count",
    "prior_preleased_total_count"
  )

  pct_cols <- c(
    "current_preleased_percent",
    "prior_preleased_percent",
    "variance"
  )

  summary_by_property <- details_by_property_data |>
    dplyr::group_by(report_date, property_id, property_name, unit_type) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::all_of(sum_cols),
        ~sum(.x, na.rm = TRUE)
      ),
      dplyr::across(
        tidyselect::all_of(pct_cols),
        ~mean(.x, na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup()

  details_data <- dplyr::select(
    details_data,
    "report_date",
    "property_id",
    "property_name",
    "bldg_unit",
    "unit_type",
    "resident_name",
    "lease_status",
    "lease_term_name",
    "lease_start_date",
    "lease_end_date",
    "deposit_charged",
    "deposit_held",
    "market_rent",
    "budgeted_rent",
    "advertised_rate",
    "scheduled_rent",
    "actual_charges"
  )

  default_col_def <- reactable::colDef(
    align = "center",
    headerVAlign = "center",
    vAlign = "center",
    format = reactable::colFormat(separators = TRUE),
    footer = function(values, col_name) {
      totals[[col_name]]
    }
  )

  avg_cols <- c(
    "avg_market_rent",
    "avg_budgeted_rent",
    "avg_scheduled_rent",
    "avg_actual_charges",
    "avg_scheduled_charges",
    "current_preleased_percent",
    "prior_preleased_percent",
    "variance"
  )

  sum_cols <- c(
    "excluded_units",
    "rentable_units",
    "occupied_units",
    "available_units",
    "current_preleased_new_count",
    "prior_preleased_new_count",
    "current_preleased_renewal_count",
    "prior_preleased_renewal_count",
    "current_preleased_total_count",
    "prior_preleased_total_count"
  )

  totals <- details_by_property_data |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::all_of(sum_cols),
        ~sum(.x, na.rm = TRUE)
      ),
      dplyr::across(
        tidyselect::all_of(avg_cols),
        ~mean(.x, na.rm = TRUE)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(avg_cols),
        ~round(.x, 0)
      )
    )

  col_defs <- list(
    report_date = reactable::colDef(show = FALSE),
    property_id = reactable::colDef(show = FALSE),
    property_name = reactable::colDef(
      name = "Property",
      width = 200,
      footer = "Total/Average",
      sticky = "left",
      cell = function(value, index) {
        property_id <- details_by_property_data$property_id[index]
        property_id <- if (!is.na(property_id)) property_id else "Unknown"
        htmltools::tags$div(
          htmltools::tags$div(style = "font-weight: 600;", value),
          htmltools::tags$div(style = "font-size: 0.75rem; color: #666;", property_id)
        )
      }
    ),
    unit_type = reactable::colDef(
      name = "Unit Type",
      width = 150,
      sticky = "left"
    ),
    excluded_units = reactable::colDef(
      name = "Excluded Units",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    rentable_units = reactable::colDef(
      name = "Rentable Units",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    occupied_units = reactable::colDef(
      name = "Occupied Units",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    available_units = reactable::colDef(
      name = "Available Units",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    avg_market_rent = reactable::colDef(
      name = "Average Market Rent",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE),
      aggregate = "mean"
    ),
    avg_budgeted_rent = reactable::colDef(
      name = "Average Budgeted Rent",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE),
      aggregate = "mean"
    ),
    avg_advertised_rate = reactable::colDef(
      name = "Average Advertised Rate",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE),
      aggregate = "mean"
    ),
    avg_scheduled_rent = reactable::colDef(
      name = "Average Scheduled Rent",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE),
      aggregate = "mean"
    ),
    avg_actual_charges = reactable::colDef(
      name = "Average Actual Charges",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE),
      aggregate = "mean"
    ),
    avg_scheduled_charges = reactable::colDef(
      name = "Average Scheduled Charges",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE),
      aggregate = "mean"
    ),
    current_preleased_new_count = reactable::colDef(
      name = "Currrent New",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    prior_preleased_new_count = reactable::colDef(
      name = "Prior New",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    current_preleased_renewal_count = reactable::colDef(
      name = "Current Renewal",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    prior_preleased_renewal_count = reactable::colDef(
      name = "Prior Renewal",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    current_preleased_total_count = reactable::colDef(
      name = "Current Total",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    prior_preleased_total_count = reactable::colDef(
      name = "Prior Total",
      format = reactable::colFormat(digits = 0, separators = TRUE),
      aggregate = "sum"
    ),
    current_preleased_percent = reactable::colDef(
      name = "Pre-Lease %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = "mean",
      style = function(value) {
        color <- if (value >= 0.9) "#088F44" else if (value >= 0.7) "#FFA500" else "#FF4444"
        list(color = color, fontWeight = 600)
      }
    ),
    prior_preleased_percent = reactable::colDef(
      name = "Prior Pre-Lease %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = "mean",
      style = function(value) {
        color <- if (value >= 0.9) "#088F44" else if (value >= 0.7) "#FFA500" else "#FF4444"
        list(color = color, fontWeight = 600)
      }
    ),
    variance = reactable::colDef(
      name = "Variance (%)",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = "mean",
      style = function(value) {
        color <- if (value >= 0) "#088F44" else "#FF4444"
        list(color = color, fontWeight = 600)
      }
    )
  )

  details_col_defs <- list(
    report_date = reactable::colDef(show = FALSE),
    property_id = reactable::colDef(show = FALSE),
    property_name = reactable::colDef(show = FALSE),
    bldg_unit = reactable::colDef(show = FALSE),
    unit_type = reactable::colDef(show = FALSE),
    resident_name = reactable::colDef(name = "Resident"),
    lease_status = reactable::colDef(name = "Lease Status"),
    lease_term_name = reactable::colDef(name = "Lease Term"),
    lease_start_date = reactable::colDef(name = "Lease Start", format = reactable::colFormat(date = TRUE)),
    lease_end_date = reactable::colDef(name = "Lease End", format = reactable::colFormat(date = TRUE)),
    deposit_charged = reactable::colDef(
      name = "Deposit Charged",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    ),
    deposit_held = reactable::colDef(
      name = "Deposit Held",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    ),
    market_rent = reactable::colDef(
      name = "Market Rent",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    ),
    budgeted_rent = reactable::colDef(
      name = "Budgeted Rent",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    ),
    advertised_rate = reactable::colDef(
      name = "Advertised Rate",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    ),
    scheduled_rent = reactable::colDef(
      name = "Scheduled Rent",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    ),
    actual_charges = reactable::colDef(
      name = "Actual Charges",
      format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
    )
  )

  reactable::reactable(
    details_by_property_data,
    theme = pre_lease_reactable_theme(),
    groupBy = c("property_name"),
    striped = TRUE,
    highlight = TRUE,
    bordered = TRUE,
    filterable = TRUE,
    # searchable = TRUE,
    defaultColDef = default_col_def,
    # resizable = TRUE,
    outlined = TRUE,
    compact = TRUE,
    columns = col_defs,
    onClick = "expand",
    details = function(index) {
      property <- details_data$property_name[index]
      unit_type <- details_data$unit_type[index]

      details <- details_data[details_data$property_name == property & details_data$unit_type == unit_type, ]

      if (nrow(details) == 0) {
        return(div("No details available"))
      }

      details_table <- reactable::reactable(
        details,
        outlined = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        columns = details_col_defs,
        defaultPageSize = 5
      )

      details_table
    }
  )
}

#' @importFrom reactable reactableTheme
pre_lease_reactable_theme <- function() {

  header_style <- list(
    background = gmh_colors("primary"),
    color = gmh_colors("light"),
    "&:hover[aria-sort]" = list(background = gmh_colors("secondary")),
    "&[aria-sort]" = list(background = gmh_colors("secondary")),
    borderRight = paste0("1px solid ", gmh_colors("light"))
  )

  header_group_style <- list(
    background = gmh_colors("primary"),
    color = gmh_colors("light"),
    borderTop = paste0("2px solid ", gmh_colors("dark")),
    borderBottom = paste0("2px solid ", gmh_colors("dark"))
  )

  footer_style <- list(
    background = gmh_colors("primary"),
    color = "white",
    fontWeight = "600"
  )

  reactable::reactableTheme(
    headerStyle = header_style,
    groupHeaderStyle = header_group_style,
    footerStyle = footer_style
  )
}

#' @importFrom dplyr mutate summarise across all_of
#' @importFrom scales percent
#' @importFrom tidyselect starts_with
calculate_pre_lease_summary_totals <- function(summary_data) {
  sum_cols <- c(
    "total_beds",
    "model_beds",
    "current_occupied",
    "current_total_new",
    "current_total_renewals",
    "current_total_leases",
    "prior_total_new",
    "prior_total_renewals",
    "prior_total_leases",
    "yoy_variance_count",
    "weekly_new",
    "weekly_renewal",
    "weekly_total",
    "beds_left",
    "vel_90",
    "vel_95",
    "vel_100"
  )

  avg_cols <- c(
    "current_occupancy",
    "current_preleased_percent",
    "prior_preleased_percent",
    "yoy_variance_percent",
    "weekly_percent_gained"
  )

  orig_cols <- names(summary_data)

  summary_data |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_cols), function(x) sum(x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(avg_cols), function(x) mean(x, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::starts_with("vel_"), function(x) round(x, 2)),
      dplyr::across(dplyr::all_of(sum_cols), function(x) prettyNum(x, big.mark = ",")),
      dplyr::across(dplyr::all_of(avg_cols), function(x) scales::percent(x, accuracy = 0.1))
    )
}

#' @importFrom tibble tibble
empty_pre_lease_summary_data <- function() {
  tibble::tibble(
    report_date = as.Date(logical(0)),
    property_id = integer(0),
    property_name = character(0),
    investment_partner = character(0),
    total_beds = integer(0),
    model_beds = integer(0),
    current_occupied = integer(0),
    current_occupancy = numeric(0),
    current_total_new = integer(0),
    current_total_renewals = integer(0),
    current_total_leases = integer(0),
    current_preleased_percent = numeric(0),
    prior_total_new = integer(0),
    prior_total_renewals = integer(0),
    prior_total_leases = integer(0),
    prior_preleased_percent = numeric(0),
    yoy_variance_count = integer(0),
    yoy_variance_percent = numeric(0),
    weekly_new = integer(0),
    weekly_renewal = integer(0),
    weekly_total = integer(0),
    weekly_percent_gained = numeric(0),
    beds_left = integer(0),
    vel_90 = numeric(0),
    vel_95 = numeric(0),
    vel_100 = numeric(0),
  )
}

#' @importFrom dplyr transmute
prep_pre_lease_summary_data <- function(entrata_pre_lease_summary, model_beds, investment_partners) {

  entrata_pre_lease_summary |>
    dplyr::transmute(
      report_date = .data$report_date,
      property_id = as.integer(.data$property_id),
      property_name = .data$property_name,
      total_beds = .data$available_count,
      current_occupied = .data$occupied_count,
      current_occupancy = .data$occupied_count / .data$total_beds,
      current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
      current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
      current_total_leases = .data$current_total_new + .data$current_total_renewals,
      current_preleased_percent = .data$current_total_leases / .data$total_beds,
      prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
      prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
      prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
      prior_preleased_percent = .data$prior_total_leases / .data$total_beds,
      yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
      yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
      beds_left = .data$total_beds - .data$current_total_leases,
      vel_90 = .data$beds_left * .9 / .env$weeks_left_to_lease,
      vel_95 = .data$beds_left * .95 / .env$weeks_left_to_lease,
      vel_100 = .data$beds_left * 1 / .env$weeks_left_to_lease
    )
}
