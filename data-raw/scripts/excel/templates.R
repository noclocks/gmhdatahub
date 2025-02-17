#  ------------------------------------------------------------------------
#
# Title : Excel Templates
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------

# pre-lease ---------------------------------------------------------------
pool <- db_connect()

pre_lease_data <- db_read_tbl(pool, "gmh.pre_lease_global_summary")

pre_lease_data <- db_read_tbl

gmh_img <- pkg_sys("www/images/gmh/logos/gmh-logo.svg")
noclocks_img <- pkg_sys("www/images/noclocks/logos/noclocks-logo-black.svg")
entrata_img <- pkg_sys("www/images/entrata/logos/entrata-logo-red-transparent.png")




# options
options(
  "openxlsx2.dateFormat" = "yyyy-mm-dd",
  "openxlsx2.datetimeFormat" = "yyyy-mm-dd hh:mm:ss",
  "openxlsx2.numFormat" = "#,##0;[Red](#,##0)",

)

# initialize workbook
wb_init <- openxlsx2::wb_workbook(
  creator = "No Clocks, LLC",
  title = "GMH Communities Pre-Lease Excel Report",
  subject = "Pre-Lease Report",
  category = "Real-Estate",
  keywords = c("GMH", "Pre-Lease", "Summary", "Report"),
  company = "GMH Communities",
  theme = NULL
)

# styles
title_style <- openxlsx::createStyle(
  fontSize = 12,
  textDecoration = "bold",
  fgFill = "#0D1B2D",
  fontColour = "white",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

subtitle_style <- openxlsx::createStyle(
  fontSize = 12,
  textDecoration = "bold",
  fgFill = "#0D1B2D",
  fontColour = "white",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

tbl_pre_header_style <- openxlsx::createStyle(
  fontSize = 11,
  fontColour = "white",
  numFmt = "text",
  border = c("top", "bottom", "left", "right"),
  borderColour = "black",
  borderStyle = "thin",
  bgFill = "#0D1B2D",
  fgFill = "#0D1B2D",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  wrapText = FALSE
)

tbl_header_style <- openxlsx::createStyle(
  fontSize = 11,
  fontColour = "white",
  numFmt = "text",
  border = c("top", "bottom", "left", "right"),
  borderColour = "black",
  borderStyle = "thin",
  bgFill = "#0D1B2D",
  fgFill = "#0D1B2D",
  halign = "center",
  valign = "center",
  textDecoration = "bold",
  wrapText = TRUE
)

tbl_body_style <- openxlsx::createStyle(
  fontSize = 11,
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

tbl_comma_fmt <- openxlsx::createStyle(
  numFmt = "#,##0_);[Red](#,##0)",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

tbl_percent_fmt <- openxlsx::createStyle(
  numFmt = "0.0%;[Red](0.0%)",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

# add worksheets
wb_init$add_worksheet("Cover", tab_color = "black")
wb_init$add_worksheet("Index", tab_color = "black")
wb_init$add_worksheet("Summary", tab_color = "red")
wb_init$add_worksheet("Details", tab_color = "red")
wb_init$add_worksheet("Parameters", tab_color = "blue")
wb_init <- openxlsx2::wb_add_worksheet(wb_init, "Cover", tab_color = "black")
wb_init <- openxlsx2::wb_add_worksheet(wb_init, "Contents", tab_color = "black")
wb_init <- openxlsx2::wb_add_worksheet(wb_init, "Summary", tab_color = "red")
wb_init <- openxlsx2::wb_add_worksheet(wb_init, "Details", tab_color = "red")
wb_init <- openxlsx2::wb_add_worksheet(wb_init, "Parameters", tab_color = "blue")


# dd ----------------------------------------------------------------------



wb_template <- openxlsx2::wb_load(
  pkg_sys("templates/excel/template.xlsx"),
  sheet = "Summary"
)

# data
report_date <- pre_lease_summary_data |>
  dplyr::pull("report_date") |>
  max(na.rm = TRUE)

leasing_week <- get_leasing_week_number(report_date)

weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

leasing_season_ending <- get_pre_lease_season_start_date(report_date)

xl_data <- pre_lease_summary_data |>
  dplyr::select(
    property_name,
    investment_partner,
    total_beds,
    model_beds,
    current_occupied,
    current_occupancy,
    current_total_new,
    current_total_renewals,
    current_total_leases,
    current_preleased_percent,
    prior_total_new,
    prior_total_renewals,
    prior_total_leases,
    prior_preleased_percent,
    yoy_variance_count,
    yoy_variance_percent,
    weekly_new,
    weekly_renewal,
    weekly_total,
    weekly_percent_gained,
    beds_left,
    vel_90,
    vel_95,
    vel_100
  ) |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::where(is.numeric),
      dplyr::coalesce,
      0
    )
  )

comma_cols <- c(
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

pct_cols <- c(
  "current_occupancy",
  "current_preleased_percent",
  "prior_preleased_percent",
  "yoy_variance_percent",
  "weekly_percent_gained"
)

for (col in comma_cols) {
  class(xl_data[[col]]) <- c("comma", class(xl_data[[col]]))
}

for (col in pct_cols) {
  class(xl_data[[col]]) <- c("percentage", class(xl_data[[col]]))
}

wb_init$add_data(
  sheet = "Summary",
  x = report_date,
  start_row = 2,
  start_col = 2,
  col_names = FALSE,
  name = "report_date"
)

wb_init$add_data(
  sheet = "Summary",
  x = leasing_season_ending,
  start_row = 2,
  start_col = 24,
  col_names = FALSE,
  name = "leasing_season_ending"
)

wb_init$add_data(
  sheet = "Summary",
  x = leasing_week,
  start_row = 3,
  start_col = 2,
  col_names = FALSE,
  name = "leasing_week"
)

wb_init$add_data(
  sheet = "Summary",
  x = weeks_left_to_lease,
  start_row = 3,
  start_col = 24,
  col_names = FALSE,
  name = "weeks_left_to_lease"
)

wb_init$add_data(
  sheet = "Summary",
  x = xl_data |>
    dplyr::select(property_name, investment_partner),
  start_row = 6,
  start_col = 1,
  col_names = FALSE
)

wb_init$add_data(
  sheet = "Summary",
  x = xl_data |>
    dplyr::select(
      total_beds:current_total_renewals
    ),
  start_row = 6,
  start_col = 3,
  col_names = FALSE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "SUM(G6:H6)",
  dims = paste0("I6:I", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "I6 / $C6",
  dims = paste0("J6:J", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_data(
  sheet = "Summary",
  x = xl_data |>
    dplyr::select(
      prior_total_new:prior_total_renewals
    ),
  start_row = 6,
  start_col = 11,
  col_names = FALSE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "SUM(K6:L6)",
  dims = paste0("M6:M", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "M6 / $C6",
  dims = paste0("N6:N", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "I6 - M6",
  dims = paste0("O6:O", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "O6 / M6",
  dims = paste0("P6:P", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_data(
  sheet = "Summary",
  x = xl_data |>
    dplyr::select(
      weekly_new:weekly_renewal
    ),
  start_row = 6,
  start_col = 17,
  col_names = FALSE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "SUM(Q6:R6)",
  dims = paste0("S6:S", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "S6 / $U6",
  dims = paste0("T6:T", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "$C6 - $D6",
  dims = paste0("U6:U", nrow(xl_data) + 5),
  shared = TRUE
)

wb_init$add_formula(
  sheet = "Summary",
  x = "($U6 * V$5) / weeks_left_to_lease",
  dims = paste0("V6:X", nrow(xl_data) + 5),
  shared = TRUE
)

# ?wb_add_data_table
#
# xl_data_w_formulas <- xl_data |>
#   dplyr::mutate(
#     current_total_leases = "[[$This Row], [current_total_new]]+[[$This Row], [current_total_renewals]]",
#     current_preleased_percent = "pre_lease_tbl[[$This Row], [current_total_leases]] / pre_lease_tbl[[$This Row], [total_beds]]",
#     prior_total_leases = "pre_lease_tbl[[$This Row], [prior_total_new]] + pre_lease_tbl[[$This Row], [prior_total_renewals]]",
#     prior_preleased_percent = "pre_lease_tbl[[$This Row], [prior_total_leases]] / pre_lease_tbl[[$This Row], [total_beds]]",
#     yoy_variance_count = "pre_lease_tbl[[$This Row], [current_total_leases]] - pre_lease_tbl[[$This Row], [prior_total_leases]]",
#     yoy_variance_percent = "pre_lease_tbl[[$This Row], [yoy_variance_count]] / pre_lease_tbl[[$This Row], [prior_total_leases]]",
#     weekly_total = "pre_lease_tbl[[$This Row], [weekly_new]] + pre_lease_tbl[[$This Row], [weekly_renewal]]",
#     weekly_percent_gained = "pre_lease_tbl[[$This Row], [weekly_total]] / pre_lease_tbl[[$This Row], [beds_left]]",
#     beds_left = "pre_lease_tbl[[$This Row], [total_beds]] - pre_lease_tbl[[$This Row], [current_occupied]]",
#     vel_90 = "(0.90 * pre_lease_tbl[[$This Row], [beds_left]]) / weeks_left_to_lease",
#     vel_95 = "(0.95 * pre_lease_tbl[[$This Row], [beds_left]]) / weeks_left_to_lease",
#     vel_100 = "(1.00 * pre_lease_tbl[[$This Row], [beds_left]]) / weeks_left_to_lease"
#   )
#
# formula_cols <- c(
#   "current_total_leases",
#   "current_preleased_percent",
#   "prior_total_leases",
#   "prior_preleased_percent",
#   "yoy_variance_count",
#   "yoy_variance_percent",
#   "weekly_total",
#   "weekly_percent_gained",
#   "beds_left",
#   "vel_90",
#   "vel_95",
#   "vel_100"
# )
#
# for (col in formula_cols) {
#   class(xl_data_w_formulas[[col]]) <- c(class(xl_data_w_formulas[[col]]), "formula")
# }
#






content = function(file) {
  shiny::withProgress(
    message = "Generating Excel Pre-Lease Report Export...",
    value = 0,
    {
      shiny::setProgress(10, message = "Collecting Data and Information...")

      browser()

      report_date <- pre_lease_summary_data() |>
        dplyr::pull("report_date") |>
        max(na.rm = TRUE)

      leasing_week <- get_leasing_week_number(report_date)

      weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

      leasing_season_ending <- get_pre_lease_season_start_date(report_date)

      xl_data <- pre_lease_summary_data() |>
        dplyr::select(
          property_name,
          investment_partner,
          total_beds,
          model_beds,
          current_occupied,
          current_occupancy,
          current_total_new,
          current_total_renewals,
          current_total_leases,
          current_preleased_percent,
          prior_total_new,
          prior_total_renewals,
          prior_total_leases,
          prior_preleased_percent,
          yoy_variance_count,
          yoy_variance_percent,
          weekly_new,
          weekly_renewal,
          weekly_total,
          weekly_percent_gained,
          beds_left,
          vel_90,
          vel_95,
          vel_100
        ) |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(is.numeric),
            dplyr::coalesce,
            0
          )
        )

      if (input$export_data == "filtered") {
        xl_data <- xl_data |>
          dplyr::filter(
            .data$investment_partner %in% input$partners,
            .data$property_name %in% input$properties
          )
      }

      shiny::setProgress(20, message = "Formatting Data for Excel...")

      comma_cols <- c(
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

      pct_cols <- c(
        "current_occupancy",
        "current_preleased_percent",
        "prior_preleased_percent",
        "yoy_variance_percent",
        "weekly_percent_gained"
      )

      for (col in comma_cols) {
        class(xl_data[[col]]) <- c("comma", class(xl_data[[col]]))
      }

      for (col in pct_cols) {
        class(xl_data[[col]]) <- c("percentage", class(xl_data[[col]]))
      }

      shiny::setProgress(30, message = "Initializing Excel Template...")



      shiny::setProgress(40, message = "Writing Report Date to Excel...")

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = report_date,
        start_row = 2,
        start_col = 2,
        col_names = FALSE
      )

      setProgress(50, message = "Writing Leasing Season Ending Date to Excel...")

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = leasing_season_ending,
        start_row = 2,
        start_col = 24,
        col_names = FALSE
      )

      setProgress(60, message = "Writing Leasing Week to Excel...")

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = leasing_week,
        start_row = 3,
        start_col = 2,
        col_names = FALSE
      )

      setProgress(70, message = "Writing Weeks Left to Lease to Excel...")

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = weeks_left_to_lease,
        start_row = 3,
        start_col = 24,
        col_names = FALSE
      )

      setProgress(80, message = "Writing Data to Excel...")

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = xl_data |>
          dplyr::select(property_name, investment_partner),
        start_row = 6,
        start_col = 1,
        col_names = FALSE
      )

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = xl_data |>
          dplyr::select(
            total_beds:current_total_renewals
          ),
        start_row = 6,
        start_col = 3,
        col_names = FALSE
      )

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = xl_data |>
          dplyr::select(
            prior_total_new:prior_total_renewals
          ),
        start_row = 6,
        start_col = 11,
        col_names = FALSE
      )

      wb_template$add_data(
        sheet = "Pre-Lease Summary",
        x = xl_data |>
          dplyr::select(
            weekly_new:weekly_renewal
          ),
        start_row = 6,
        start_col = 17,
        col_names = FALSE
      )

      # remove rows if filtered / nrows between template and data differ
      nrow_template <- 21
      nrow_data <- nrow(xl_data)

      if (nrow_data < nrow_template) {
        start_row <- 6L
        end_row <- start_row + nrow_data - 1L
        remove_start_row <- end_row + 1L
        remove_end_row <- nrow_template + 5L

        remove_dims <- openxlsx2::wb_dims(
          rows = remove_start_row:remove_end_row,
          cols = 1:10000000
        )

        wb_template$clean_sheet(
          sheet = "Pre-Lease Summary",
          dims = remove_dims
        )
      }

      setProgress(90, message = "Finalizing Excel File...")

      wb_template$save(file, overwrite = TRUE)

      setProgress(100, message = "Excel File Generated Successfully!")

    }
  )

}
)


wb_template$add_data(
  sheet = "Pre-Lease Summary",
  x = xl_data |>
    dplyr::select(
      prior_total_new:prior_total_renewals
    ),
  start_row = 6,
  start_col = 11,
  col_names = FALSE
)

wb_template$add_data(
  sheet = "Pre-Lease Summary",
  x = xl_data |>
    dplyr::select(
      weekly_new:weekly_renewal
    ),
  start_row = 6,
  start_col = 17,
  col_names = FALSE
)

# remove rows if filtered / nrows between template and data differ
nrow_template <- 21
nrow_data <- nrow(xl_data)

if (nrow_data < nrow_template) {
  start_row <- 6L
  end_row <- start_row + nrow_data - 1L
  remove_start_row <- end_row + 1L
  remove_end_row <- nrow_template + 5L

  remove_dims <- openxlsx2::wb_dims(
    rows = remove_start_row:remove_end_row,
    cols = 1:10000000
  )

  wb_template$clean_sheet(
    sheet = "Pre-Lease Summary",
    dims = remove_dims
  )
}

setProgress(90, message = "Finalizing Excel File...")

wb_template$save(file, overwrite = TRUE)



wb_init$set_cell_style(
  sheet = "Summary",
  dims = "1:1",
  style = title_style
)

wb_init$add_data(
  sheet = "Summary",
  x = paste0("Report Date: ", report_date),
  start_col = 1,
  start_row = 2,
  name = "Summary"
)

wb_init$add_data(
  sheet = "Summary",
  x = pre_lease_data,
  start_col = 2,
  start_row = 4,
  name = "Summary"
)

gmh_img <- pkg_sys("www/images/gmh/logos/gmh-logo.svg")
noclocks_img <- pkg_sys("www/images/noclocks/logos/noclocks-logo-black.svg")
entrata_img <- pkg_sys("www/images/entrata/logos/entrata-logo-red-transparent.png")




# options
options(
  "openxlsx2.dateFormat" = "yyyy-mm-dd",
  "openxlsx2.datetimeFormat" = "yyyy-mm-dd hh:mm:ss",
  "openxlsx2.numFormat" = "#,##0;[Red](#,##0)",

)





# styles
# sheet_title_style <- openxlsx2::create_cell_style(
#
# )

# cover
wb_init <- openxlsx2::wb_add_image(
  wb = wb_init,
  sheet = "Cover",
  dims = "D5",
  file = gmh_img,
  width = 6,
  height = 3,
  address = "https://gmhcommunities.com"
)

# summary
wb_init <- openxlsx2::wb_add_data_table(
  wb = wb_init,
  sheet = "Summary",
  x = entrata_pre_lease_summary_tbl,
  start_col = 2,
  start_row = 4,
  name = "Summary"
)

# index
# index_tbl <- tibble::tibble(
#   "Sheet" = c("Cover", "Index", "Summary", "Details", "Parameters"),
#   "Description" = c("Cover Page", "Index of Sheets", "Pre-Lease Summary", "Pre-Lease Details", "Pre-Lease Parameters")
# )

# wb_init <- openxlsx2::wb_add_data(
#   wb = wb_init,
#   sheet = "Index",
#   x = index_tbl,
#   start_col = 2,
#   start_row = 4,
#   name = "Index"
# )





library(R6)
library(openxlsx2)
library(writexl)
library(readxl)
library(dplyr)

# ExcelWorkbook <- R6Class("ExcelWorkbook",
#                          public = list(
#                            workbook = NULL,
#                            filename = NULL,
#
#                            initialize = function(base_name = "GMH_University_Housing") {
#                              self$workbook <- wb_workbook()
#                              self$filename <- self$derive_filename(base_name)
#                              self$add_worksheet("Prelease Summary")
#                              self$set_options()
#                            },
#
#                            derive_filename = function(base_name) {
#                              paste0(base_name, "_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
#                            },
#
#                            set_options = function() {
#                              wb_set_creator(self$workbook, "No Clocks, LLC")
#                              wb_set_properties(self$workbook,
#                                                title = "GMH University Housing Prelease Summary",
#                                                subject = "Prelease Data",
#                                                category = "Report")
#                            },
#
#                            add_worksheet = function(sheet_name) {
#                              wb_add_worksheet(self$workbook, sheet_name)
#                            },
#
#                            write_data = function(data, sheet_name, start_row, start_col) {
#                              wb_add_data(self$workbook, sheet_name, data, start_row = start_row, start_col = start_col)
#                            },
#
#                            add_formula = function(sheet_name, formula, start_row, start_col, array = FALSE) {
#                              wb_add_formula(self$workbook, sheet_name, x = formula, startRow = start_row, startCol = start_col, array = array)
#                            },
#
#                            add_data_table = function(sheet_name, data, table_name, start_row = 1, start_col = 1) {
#                              wb_add_data_table(self$workbook, sheet_name, x = data, tableName = table_name, startRow = start_row, startCol = start_col)
#                            },
#
#                            add_calculated_column = function(sheet_name, table_name, column_name, formula) {
#                              table_ref <- paste0(table_name, "[[#This Row],", column_name, "]]")
#                              formula <- gsub("@", table_ref, formula)
#                              wb_add_formula(self$workbook, sheet_name, x = formula, array = TRUE)
#                            },
#
#                            generate_prelease_summary = function(summary_table, investment_partners, selected_columns, selected_properties) {
#                              sheet_name <- "Prelease Summary"
#
#                              # Filter data based on user selections
#                              data <- self$prepare_data(summary_table, investment_partners, selected_columns, selected_properties)
#
#                              # Add data table
#                              self$add_data_table(sheet_name, data, "PreleaseData")
#
#                              # Add calculated columns (example)
#                              self$add_calculated_column(sheet_name, "PreleaseData", "TotalOccupancy", "=[@[Total New]] + [@[Total Renewals]]")
#
#                              # Add totals row
#                              self$add_totals_row(sheet_name, "PreleaseData", data)
#                            },
#
#                            prepare_data = function(summary_table, investment_partners, selected_columns, selected_properties) {
#                              data <- summary_table %>%
#                                filter(X1 %in% selected_properties) %>%
#                                left_join(investment_partners, by = c("X1" = "property_name")) %>%
#                                select(X1, investment_partner, all_of(selected_columns))
#
#                              return(data)
#                            },
#
#                            add_totals_row = function(sheet_name, table_name, data) {
#                              num_cols <- ncol(data)
#                              totals_row <- paste0("=SUBTOTAL(9,", table_name, "[", colnames(data), "])")
#                              self$add_formula(sheet_name, totals_row, nrow(data) + 2, 1:num_cols)
#                            },
#
#                            save = function() {
#                              wb_save(self$workbook, self$filename)
#                            }
#                          )
# )
#
#
#
# excel_wb <- ExcelWorkbook$new(paste0("GMH_University_Housing_25-26_Prelease_Summary_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = ""))

# styles
# sheet_title_style <- openxlsx2::create_cell_style(
#
# )

# cover
wb_init <- openxlsx2::wb_add_image(
  wb = wb_init,
  sheet = "Cover",
  dims = "D5",
  file = gmh_img,
  width = 6,
  height = 3,
  address = "https://gmhcommunities.com"
)

# summary
wb_init <- openxlsx2::wb_add_data_table(
  wb = wb_init,
  sheet = "Summary",
  x = entrata_pre_lease_summary_tbl,
  start_col = 2,
  start_row = 4,
  name = "Summary"
)

# index
# index_tbl <- tibble::tibble(
#   "Sheet" = c("Cover", "Index", "Summary", "Details", "Parameters"),
#   "Description" = c("Cover Page", "Index of Sheets", "Pre-Lease Summary", "Pre-Lease Details", "Pre-Lease Parameters")
# )

# wb_init <- openxlsx2::wb_add_data(
#   wb = wb_init,
#   sheet = "Index",
#   x = index_tbl,
#   start_col = 2,
#   start_row = 4,
#   name = "Index"
# )





library(R6)
library(openxlsx2)
library(writexl)
library(readxl)
library(dplyr)

# ExcelWorkbook <- R6Class("ExcelWorkbook",
#                          public = list(
#                            workbook = NULL,
#                            filename = NULL,
#
#                            initialize = function(base_name = "GMH_University_Housing") {
#                              self$workbook <- wb_workbook()
#                              self$filename <- self$derive_filename(base_name)
#                              self$add_worksheet("Prelease Summary")
#                              self$set_options()
#                            },
#
#                            derive_filename = function(base_name) {
#                              paste0(base_name, "_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
#                            },
#
#                            set_options = function() {
#                              wb_set_creator(self$workbook, "No Clocks, LLC")
#                              wb_set_properties(self$workbook,
#                                                title = "GMH University Housing Prelease Summary",
#                                                subject = "Prelease Data",
#                                                category = "Report")
#                            },
#
#                            add_worksheet = function(sheet_name) {
#                              wb_add_worksheet(self$workbook, sheet_name)
#                            },
#
#                            write_data = function(data, sheet_name, start_row, start_col) {
#                              wb_add_data(self$workbook, sheet_name, data, start_row = start_row, start_col = start_col)
#                            },
#
#                            add_formula = function(sheet_name, formula, start_row, start_col, array = FALSE) {
#                              wb_add_formula(self$workbook, sheet_name, x = formula, startRow = start_row, startCol = start_col, array = array)
#                            },
#
#                            add_data_table = function(sheet_name, data, table_name, start_row = 1, start_col = 1) {
#                              wb_add_data_table(self$workbook, sheet_name, x = data, tableName = table_name, startRow = start_row, startCol = start_col)
#                            },
#
#                            add_calculated_column = function(sheet_name, table_name, column_name, formula) {
#                              table_ref <- paste0(table_name, "[[#This Row],", column_name, "]]")
#                              formula <- gsub("@", table_ref, formula)
#                              wb_add_formula(self$workbook, sheet_name, x = formula, array = TRUE)
#                            },
#
#                            generate_prelease_summary = function(summary_table, investment_partners, selected_columns, selected_properties) {
#                              sheet_name <- "Prelease Summary"
#
#                              # Filter data based on user selections
#                              data <- self$prepare_data(summary_table, investment_partners, selected_columns, selected_properties)
#
#                              # Add data table
#                              self$add_data_table(sheet_name, data, "PreleaseData")
#
#                              # Add calculated columns (example)
#                              self$add_calculated_column(sheet_name, "PreleaseData", "TotalOccupancy", "=[@[Total New]] + [@[Total Renewals]]")
#
#                              # Add totals row
#                              self$add_totals_row(sheet_name, "PreleaseData", data)
#                            },
#
#                            prepare_data = function(summary_table, investment_partners, selected_columns, selected_properties) {
#                              data <- summary_table %>%
#                                filter(X1 %in% selected_properties) %>%
#                                left_join(investment_partners, by = c("X1" = "property_name")) %>%
#                                select(X1, investment_partner, all_of(selected_columns))
#
#                              return(data)
#                            },
#
#                            add_totals_row = function(sheet_name, table_name, data) {
#                              num_cols <- ncol(data)
#                              totals_row <- paste0("=SUBTOTAL(9,", table_name, "[", colnames(data), "])")
#                              self$add_formula(sheet_name, totals_row, nrow(data) + 2, 1:num_cols)
#                            },
#
#                            save = function() {
#                              wb_save(self$workbook, self$filename)
#                            }
#                          )
# )
#
#
#
# excel_wb <- ExcelWorkbook$new(paste0("GMH_University_Housing_25-26_Prelease_Summary_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx", sep = ""))
