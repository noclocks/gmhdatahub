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

# data
report_date <- pre_lease_data$report_date[[1]]
current_year_txt <- paste0(lubridate::year(report_date), "-", lubridate::year(report_date) + 1)
prior_year_txt <- paste0(lubridate::year(report_date) - 1, "-", lubridate::year(report_date))
weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

wb_init$add_data(
  sheet = "Summary",
  x = "GMH Communities Pre-Lease Summary Report",
  start_col = 1,
  start_row = 1,
  name = "Summary"
)

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
