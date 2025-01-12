tbl_pre_lease_summary <- function(summary_data) {

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

  totals <- calculate_pre_lease_summary_totals(summary_data)

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
      totals[[col_name]]
    }
  )

  col_defs <- list(
    report_date = reactable::colDef(show = FALSE),
    property_id = reactable::colDef(show = FALSE),
    property_name = reactable::colDef(
      name = "Property Name",
      width = 200,
      style = list(fontWeight = 600, background = "var(--rt-base-background)"),
      footer = "Total/Average"
    ),
    investment_partner = reactable::colDef(
      name = "Investment Partner",
      footer = "",
      width = 150
    ),
    model_beds = reactable::colDef(
      name = "Model Beds",
      cell = editable_column("model_beds_edit"),
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum"
    ),
    total_beds = reactable::colDef(
      name = "Total Beds",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E5F5E0", "#31A354"))
    ),
    current_occupied = reactable::colDef(
      name = "Occupied",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E5F5E0", "#31A354"))
    ),
    current_occupancy = reactable::colDef(
      name = "Occupancy %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::gauge_chart(
        data = summary_data,
        fill_color = c("#D7191C", "#FDAE61", "#FFFFBF", "#ABD9E9", "#2C7BB6"),
        show_min_max = FALSE,
        tooltip = TRUE,
        number_fmt = scales::percent,
        bold_text = TRUE
      )
    ),
    current_total_new = reactable::colDef(
      name = "New",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E5F5E0", "#31A354"))
    ),
    current_total_renewals = reactable::colDef(
      name = "Renewals",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E5F5E0", "#31A354"))
    ),
    current_total_leases = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E8F5E9", "#43A047"))
    ),
    current_preleased_percent = reactable::colDef(
      name = "Pre-Lease %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::gauge_chart(
        data = summary_data,
        fill_color = "#173d6b",
        show_min_max = FALSE,
        tooltip = TRUE,
        number_fmt = scales::percent
      )
    ),
    prior_total_new = reactable::colDef(
      name = "New",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E0F7FA", "#00BCD4"))
    ),
    prior_total_renewals = reactable::colDef(
      name = "Renewals",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#FFF3E0", "#FFB300"))
    ),
    prior_total_leases = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E8F5E9", "#43A047"))
    ),
    prior_preleased_percent = reactable::colDef(
      name = "Pre-Lease %",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::gauge_chart(
        data = summary_data,
        fill_color = "#173d6b",,
        show_min_max = FALSE,
        tooltip = TRUE,
        number_fmt = scales::percent
      )
    ),
    yoy_variance_count = reactable::colDef(
      name = "Count",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::icon_trend_indicator(data = summary_data, number_fmt = scales::comma)
    ),
    yoy_variance_percent = reactable::colDef(
      name = "Percent",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::icon_trend_indicator(data = summary_data, number_fmt = scales::label_percent(accuracy = 0.1))
    ),
    weekly_new = reactable::colDef(
      name = "New",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E0F7FA", "#00BCD4"))
    ),
    weekly_renewal = reactable::colDef(
      name = "Renewals",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#FFF3E0", "#FFB300"))
    ),
    weekly_total = reactable::colDef(
      name = "Total",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E8F5E9", "#43A047"))
    ),
    weekly_percent_gained = reactable::colDef(
      name = "Percent Gained",
      format = reactable::colFormat(percent = TRUE, digits = 1),
      aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
      cell = reactablefmtr::gauge_chart(data = summary_data, number_fmt = scales::label_percent(accuracy = 0.1)),
      width = 150
    ),
    beds_left = reactable::colDef(
      name = "Beds Left",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#FFEBEE", "#D32F2F"))
    ),
    vel_90 = reactable::colDef(
      name = "90% Velocity",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E0F7FA", "#00BCD4"))
    ),
    vel_95 = reactable::colDef(
      name = "95% Velocity",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#FFF3E0", "#FFB300"))
    ),
    vel_100 = reactable::colDef(
      name = "100% Velocity",
      format = reactable::colFormat(separators = TRUE, digits = 0),
      aggregate = "sum",
      cell = reactablefmtr::pill_buttons(data = summary_data, colors = c("#E8F5E9", "#43A047"))
    )
  )

  col_groups <- list(
    reactable::colGroup(
      name = "Property Information",
      columns = c("property_name", "investment_partner"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Beds",
      columns = c("total_beds", "model_beds"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Current Year",
      columns = c("current_occupied", "current_occupancy", "current_total_new",
                  "current_total_renewals", "current_total_leases", "current_preleased_percent"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Prior Year",
      columns = c("prior_total_new", "prior_total_renewals", "prior_total_leases",
                  "prior_preleased_percent"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Year-Over-Year",
      columns = c("yoy_variance_count", "yoy_variance_percent"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Weekly Activity (Prior Seven Days)",
      columns = c("weekly_new", "weekly_renewal", "weekly_total", "weekly_percent_gained"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    ),
    reactable::colGroup(
      name = "Leasing Velocity %",
      columns = c("beds_left", "vel_90", "vel_95", "vel_100"),
      headerStyle = list(borderRight = "2px solid #173d6b")
    )
  )

  reactable::reactable(
    data = summary_data,
    theme = pre_lease_reactable_theme(),
    defaultColDef = default_col_def,
    columns = col_defs,
    columnGroups = col_groups,
    filterable = TRUE,
    searchable = TRUE,
    bordered = TRUE,
    striped = TRUE,
    highlight = TRUE,
    defaultPageSize = -1,
    defaultSorted = list("property_name" = "asc"),
    showPagination = FALSE,
    showSortable = TRUE,
    showSortIcon = TRUE
  )
}

pre_lease_reactable_theme <- function() {
  reactable::reactableTheme(
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
}

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

  summary_data |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_cols), function(x) sum(x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(avg_cols), function(x) mean(x, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      # format integers/numbers with commas
      dplyr::across(dplyr::all_of(sum_cols), function(x) prettyNum(x, big.mark = ",")),
      # format percentages
      dplyr::across(dplyr::all_of(avg_cols), function(x) scales::percent(x, accuracy = 0.1))
    )
}


#' @importFrom dplyr tbl filter
#' @importFrom rlang .data .env
db_get_pre_lease_summary_data <- function(pool, report_date = NULL, property_ids = NULL) {

  check_db_conn(pool)
  in_shiny <- env_in_shiny_session()

  tryCatch({

    summary_tbl <- dplyr::tbl(pool, I("gmh.pre_lease_summary"))
    rpt_dates <- dplyr::pull(summary_tbl, "report_date") |> unique()
    prop_ids <- dplyr::pull(summary_tbl, "property_id") |> unique()

    if (is.null(report_date)) {
      report_date <- max(rpt_dates, na.rm = TRUE)
    }

    if (is.null(property_ids)) {
      property_ids <- prop_ids
    }

    if (!report_date %in% rpt_dates) {
      cli::cli_alert_warning("Report date not found in database.")
      report_date <- max(rpt_dates, na.rm = TRUE)
      if (in_shiny) {
        shiny::showNotification(
          ui = glue::glue(
            "Report date not found in database. Using latest available date: {report_date}."
          ),
          duration = 5,
          closeButton = TRUE,
          type = "warning"
        )
      }
    }

    if (!all(property_ids %in% prop_ids)) {
      cli::cli_alert_warning("Property IDs not found in database.")
      property_ids <- prop_ids
      if (in_shiny) {
        shiny::showNotification(
          ui = "Property IDs not found in database. Using all available properties.",
          duration = 5,
          closeButton = TRUE,
          type = "warning"
        )
      }
    }

    out <- dplyr::filter(
      summary_tbl,
      .data$property_id %in% .env$property_ids,
      .data$report_date == .env$report_date
    ) |>
      dplyr::collect()

    cli::cli_alert_success("Successfully retrieved pre-lease summary data.")

    if (in_shiny) {
      shiny::showNotification(
        ui = "Successfully retrieved pre-lease summary data.",
        duration = 5,
        closeButton = TRUE,
        type = "default"
      )
    }

    return(out)

  }, error = function(e) {

    cli::cli_alert_danger("Failed to retrieve pre-lease summary data.")
    cli::cli_alert_info("Error: {e$message}")

    if (in_shiny) {
      shiny::showNotification(
        ui = "Failed to retrieve pre-lease summary data.",
        duration = 5,
        closeButton = TRUE,
        type = "error"
      )
    }

    return(empty_pre_lease_summary_data())

  })

}

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
