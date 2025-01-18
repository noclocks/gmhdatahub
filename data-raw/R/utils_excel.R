
#  ------------------------------------------------------------------------
#
# Title : Excel Data Preparation Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-30
#
#  ------------------------------------------------------------------------

get_xl_sheets <- function(xl_file) {
  sheet_names <- readxl::excel_sheets(xl_file)
  summary_sheet <- sheet_names[1]
  report_params_sheet <- sheet_names[length(sheet_names)]
  property_names <- setdiff(
    sheet_names,
    c(summary_sheet, report_params_sheet)
  )
  list(
    "summary" = summary_sheet,
    "report_params" = report_params_sheet,
    "properties" = property_names
  )
}

read_xl_global_summary_tbl <- function(xl_file, ...) {

  global_summary_tbl_col_specs <- list(
    "property_name" = "text",
    "total_beds" = "numeric",
    "model_beds" = "numeric",
    "current_occupancy" = "numeric",
    "total_new" = "numeric",
    "total_renewals" = "numeric",
    "total_leases" = "numeric",
    "prelease_pct" = "numeric",
    "prior_total_new" = "numeric",
    "prior_total_renewals" = "numeric",
    "prior_total_leases" = "numeric",
    "prior_prelease_pct" = "numeric",
    "yoy_variance_num" = "numeric",
    "yoy_variance_pct" = "numeric",
    "weekly_new_leases" = "numeric",
    "weekly_new_renewals" = "numeric",
    "weekly_total_leases" = "numeric",
    "weekly_pct_gained" = "numeric",
    "weekly_velocity_beds_left" = "numeric",
    "weekly_velocity_leased_this_week" = "numeric",
    "weekly_velocity_90_pct" = "numeric",
    "weekly_velocity_95_pct" = "numeric",
    "weekly_velocity_100_pct" = "numeric"
  )

  global_summary_tbl_metadata <- list(
    leasing_week = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "B3",
      col_names = FALSE,
      col_types = "text"
    ) |> dplyr::pull(1) |> as.integer(),
    leasing_season_ending = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "AB3",
      col_names = FALSE,
      col_types = "date"
    ) |> dplyr::pull(1) |> as.Date(),
    weeks_left_to_lease = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "AB4",
      col_names = FALSE,
      col_types = "numeric"
    ) |> dplyr::pull(1) |> as.integer(),
    report_date = readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "A1",
      col_names = FALSE,
      col_types = "date"
    ) |> dplyr::pull(1) |> as.Date()
  )

  global_summary_tbl_totals <- list(
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "A9:D9",
      col_names = names(global_summary_tbl_col_specs)[c(1:4)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(1:4)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "F9:I9",
      col_names = names(global_summary_tbl_col_specs)[c(5:8)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(5:8)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "K9:N9",
      col_names = names(global_summary_tbl_col_specs)[c(9:12)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(9:12)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "P9:Q9",
      col_names = names(global_summary_tbl_col_specs)[c(13:14)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(13:14)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "S9:V9",
      col_names = names(global_summary_tbl_col_specs)[c(15:18)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(15:18)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "X9:AB9",
      col_names = names(global_summary_tbl_col_specs)[c(19:23)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(19:23)])))
    )
  ) |>
    dplyr::bind_cols()

  global_summary_tbl <- list(
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "A11:D20",
      col_names = names(global_summary_tbl_col_specs)[c(1:4)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(1:4)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "F11:I20",
      col_names = names(global_summary_tbl_col_specs)[c(5:8)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(5:8)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "K11:N20",
      col_names = names(global_summary_tbl_col_specs)[c(9:12)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(9:12)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "P11:Q20",
      col_names = names(global_summary_tbl_col_specs)[c(13:14)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(13:14)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "S11:V20",
      col_names = names(global_summary_tbl_col_specs)[c(15:18)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(15:18)])))
    ),
    readxl::read_excel(
      xl_file,
      sheet = "Summary",
      range = "X11:AB20",
      col_names = names(global_summary_tbl_col_specs)[c(19:23)],
      col_types = as.vector(unlist(unname(global_summary_tbl_col_specs[c(19:23)])))
    )
  ) |>
    dplyr::bind_cols()

  return(
    list(
      "metadata" = global_summary_tbl_metadata,
      "totals" = global_summary_tbl_totals,
      "data" = global_summary_tbl
    )
  )

}


#' Create Excel Ranges
#'
#' @description
#' Helper function to create ranges for Excel sheets based on filter patterns
#' and column offsets.
#'
#' @param cells `data.frame` of raw Excel cells created via [tidyxl::xlsx_cells()].
#'   Must contain (at least) the columns `sheet`, `row`, `col`, `data_type`
#'   and `character`.
#' @param filter_pattern `character` pattern to filter cells by.
#' @param start_offset `integer` offset to start the range from.
#' @param stop_col `integer` or `character` column to stop the range at.
#'
#' @returns `data.frame` of ranges for each sheet.
#' @export
#'
#' @importFrom dplyr group_by slice ungroup mutate select filter
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @importFrom readxl excel_sheets read_excel
create_ranges <- function(
    cells,
    filter_pattern,
    start_offset,
    stop_col) {
  # validate inputs
  stopifnot(exprs = {
    is.data.frame(cells)
    all(
      c("sheet", "row", "col", "data_type", "character") %in% colnames(cells)
    )
    is.character(filter_pattern)
    is.integer(start_offset)
    is.character(stop_col) || is.integer(stop_col)
  }) |> try()

  # create ranges
  cells |>
    dplyr::filter(
      stringr::str_detect(.data$character, .env$filter_pattern)
    ) |>
    dplyr::group_by(.data$sheet) |>
    dplyr::slice(1) |> # get first row of each sheet
    dplyr::mutate(
      start_cell = paste0("A", .env$start_offset),
      stop_cell = paste0(.env$stop_col, .data$row - 1)
    ) |>
    dplyr::ungroup() %>%
    dplyr::mutate(range = paste0(.data$start_cell, ":", .data$stop_cell)) |>
    dplyr::select(.data$sheet, .data$range)
}

# summary table ranges
# --------------------
# - searches for 'Total/Average:' in the 'character' column
#   to find the final row in the summary table range for each
#   sheet in the excel file representing a property unit
# - slices the first instance of 'Total/Average:' for each sheet
#   to ensure using the top summary table
# - establishes "A9" as the starting cell for each summary table
# - sets the stop cell as the row before the 'Total/Average:' row
# - creates a 'range' column with the range of the summary table
get_xl_summary_tbl_ranges <- function(
    xl_cells,
    start_cell = "A9",
    stop_cell_col = "O",
    stop_cell_row_offset = -1
) {

  xl_cells |>
    dplyr::select(sheet, address, row, col, character) |>
    dplyr::filter(
      stringr::str_detect(character, "Total/Average:")
    ) |>
    dplyr::group_by(sheet) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      start_cell = start_cell,
      stop_cell = paste0(stop_cell_col, row + stop_cell_row_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      sheet,
      start_cell,
      stop_cell
    ) |>
    dplyr::mutate(
      range = paste0(start_cell, ":", stop_cell)
    )
}

# prior_summary_tbl_ranges <- get_xl_summary_tbl_ranges(prior_xl_cells)
# current_summary_tbl_ranges <- get_xl_summary_tbl_ranges(current_xl_cells)

# details table ranges ----------------------------------------------------


# details table ranges
# --------------------
# - filters the cells to only include column A and non-blanks
# - find the rows where the first column contains "Details"
# - sets the start cell as 4 rows below the "Details" row
# - find the rows where the first column contains "Total/Average:"
# - sets the stop cell as the row before the 'Total/Average:' row
# - creates a 'range' column with the range of the details table

get_details_tbl_ranges <- function(
    xl_cells,
    start_cell_offset = 4,
    stop_cell_col = "O",
    stop_cell_row_offset = -1
) {

  xl_cells |>
    dplyr::select(sheet, address, row, col, character) |>
    dplyr::filter(
      col == 1
    ) |>
    dplyr::filter(
      stringr::str_detect(character, "Details")
    ) |>
    dplyr::mutate(
      start_cell = paste0("A", row + start_cell_offset)
    ) |>
    dplyr::select(sheet, start_cell) |>
    dplyr::inner_join(
      xl_cells |>
        dplyr::filter(
          stringr::str_detect(character, "Total/Average:")
        ) |>
        dplyr::group_by(sheet) |>
        dplyr::slice(2) |>
        dplyr::mutate(
          stop_cell = paste0(stop_cell_col, row + stop_cell_row_offset)
        ) |>
        dplyr::ungroup() |>
        dplyr::select(sheet, stop_cell),
      by = "sheet"
    ) |>
    dplyr::mutate(
      range = paste0(start_cell, ":", stop_cell)
    )
}

# prior_details_tbl_ranges <- get_details_tbl_ranges(prior_xl_cells)
# current_details_tbl_ranges <- get_details_tbl_ranges(current_xl_cells, stop_cell_col = "Q", stop_cell_row_offset = -1)

# read excel ranges -------------------------------------------------------

read_excel_ranges <- function(file, sheets, ranges, col_specs) {

  col_names <- names(col_specs)
  col_types <- purrr::map_chr(col_specs, ~ .x)

  purrr::map_dfr(
    sheets,
    ~ readxl::read_excel(
      file,
      sheet = .x,
      range = ranges[ranges$sheet == .x, "range"][[1]],
      col_types = col_types,
      col_names = col_names
    ) |>
      dplyr::mutate(unit_name = .x) |>
      dplyr::select(unit_name, tidyselect::everything())
  )
}

read_raw_market_survey_data <- function(wb_path, sheet, property_id, property_name, leasing_week, ...) {

  args <- list(
    property_detail = list(
      range = "X1",
      col_names = c("property_detail")
    ),
    property_summary = list(
      range = "A4:B19",
      col_names = c("key", "value")
    ),
    leasing_summary = list(
      range = "A22:B35",
      col_names = c("key", "value")
    ),
    short_term_leases = list(
      range = "A38:B43",
      col_names = c("key", "value")
    ),
    fees = list(
      range = "A46:C58",
      col_names = c("fees", "amount", "frequency")
    ),
    property_amenitites = list(
      range = "E4:H35",
      col_names = c("key", "x", "xx", "value")
    ),
    parking = list(
      range = "E39:H43",
      col_names = c("parking", "required", "included", "amount")
    ),
    unit_amenities = list(
      range = "K4:P30",
      col_names = c("key", "x", "xx", "xxx", "xxxx", "value")
    ),
    utilities_summary_1 = list(
      range = "K34:P36",
      col_names = c("utility", "all_inclusive", "cap", "allowance", "amount", "per_unit_bed")
    ),
    utilities_summary_2 = list(
      range = "K38:P42",
      col_names = c("utility", "included", "amount", "x", "xx", "per_unit_bed")
    ),
    parking = list(
      range = "E39:H43",
      col_names = c("parking_type", "required", "included", "amount")
    ),
    notes = list(
      range = "R20:R36",
      col_names = c("notes")
    ),
    office_hours = list(
      range = "R38:T44",
      col_names = c("day", "x", "hours")
    ),
    rents_by_floorplan = list(
      range = "A71:X81",
      col_names = c(
        "floorplan_type",
        "description",
        "x",
        "count",
        "sf_per_bed",
        "bed",
        "bath",
        "available",
        "market_rent_per_bed",
        "market_rent_per_sf",
        "concessions_gift_card",
        "concessions_one_time_rent",
        "concessions_monthly_rent",
        "effective_rent_per_bed",
        "effective_rent_per_sf",
        "additional_monthly_expenses_furniture",
        "additional_monthly_expenses_tv",
        "additional_monthly_expenses_electricity_and_gas",
        "additional_monthly_expenses_water",
        "additional_monthly_expenses_cable_and_internet",
        "additional_monthly_expenses_trash_and_valet",
        "additional_monthly_expenses_parking",
        "bundled_rent_per_bed",
        "bundled_rent_per_sf"
      )
    ),
    average_rents_by_unit_type = list(
      range = "A235:X242",
      col_names = c(
        "unit_type",
        "x",
        "xx",
        "xxx",
        "xxxx",
        "count",
        "sf_per_bed",
        "available",
        "market_rent_per_bed",
        "market_rent_per_sf",
        "concessions_gift_card",
        "concessions_one_time_rent",
        "concessions_monthly_rent",
        "effective_rent_per_bed",
        "effective_rent_per_sf",
        "additional_monthly_expenses_furniture",
        "additional_monthly_expenses_tv",
        "additional_monthly_expenses_electricity_and_gas",
        "additional_monthly_expenses_water",
        "additional_monthly_expenses_cable_and_internet",
        "additional_monthly_expenses_trash_and_valet",
        "additional_monthly_expenses_parking",
        "bundled_rent_per_bed",
        "bundled_rent_per_sf"
      )
    )
  )

  market_survey_data_lst <- purrr::map(
    args,
    function(x) {
      readxl::read_excel(
        wb_path,
        sheet = sheet,
        range = x$range,
        col_names = x$col_names,
        na = c("", "N/A")
      )
    }
  ) |>
    setNames(names(args)) |>
    purrr::map(
      function(df) {
        df |>
          dplyr::mutate(
            property_id = property_id,
            property_name = property_name,
            leasing_week = leasing_week
          )
      }
    ) |>
    purrr::map(
      function(df) {
        df |>
          # remove x, xx, xxx cols (only if they exist)
          dplyr::select(-matches("x|xx|xxx|xxxx"))
      }
    )

  # property detail ---------------------------------------------------------
  property_detail <- market_survey_data_lst$property_detail |>
    dplyr::pull(property_detail) |>
    stringr::str_split(": ") |>
    purrr::pluck(2)

  # property summary --------------------------------------------------------

  property_summary <- market_survey_data_lst$property_summary |>
    dplyr::filter(!is.na(key))

  property_summary_wide <- property_summary |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  property_summary_lst <- list(
    property_summary = property_summary,
    property_summary_wide = property_summary_wide
  )


  # leasing summary ---------------------------------------------------------

  leasing_summary <- market_survey_data_lst$leasing_summary |>
    dplyr::filter(!is.na(key))

  leasing_summary_wide <- leasing_summary |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  leasing_summary_lst <- list(
    leasing_summary = leasing_summary,
    leasing_summary_wide = leasing_summary_wide
  )


  # short term leases -------------------------------------------------------

  short_term_leases <- market_survey_data_lst$short_term_leases |>
    dplyr::filter(!is.na(key))

  short_term_leases_wide <- short_term_leases |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  short_term_leases_lst <- list(
    short_term_leases = short_term_leases,
    short_term_leases_wide = short_term_leases_wide
  )


  # fees --------------------------------------------------------------------

  fees_fee_structure <- market_survey_data_lst$fees |>
    dplyr::filter(
      fees == "Fee Structure"
    ) |>
    dplyr::pull(amount)

  fees_text_amounts <- market_survey_data_lst$fees |>
    # find values in amount column that are not parseable to numeric
    dplyr::filter(
      !grepl("^\\d+\\.?\\d*$", amount)
    )

  fees <- market_survey_data_lst$fees |>
    dplyr::filter(
      !(fees %in% fees_text_amounts$fees)
    ) |>
    dplyr::mutate(
      amount = as.numeric(amount)
    )

  fees_wide <- fees |>
    tidyr::pivot_wider(
      names_from = fees,
      values_from = amount
    )

  fees_lst <- list(
    fee_structure = fees_fee_structure,
    fees_text_amounts = fees_text_amounts,
    fees = fees,
    fees_wide = fees_wide
  )

  # property amenities ------------------------------------------------------

  property_amenities_common_area_rating <- market_survey_data_lst$property_amenitites |>
    dplyr::filter(
      key == "Common Area Rating"
    ) |>
    dplyr::pull(value)

  property_amenitites <- market_survey_data_lst$property_amenitites |>
    dplyr::filter(
      !is.na(key),
      key != "Common Area Rating"
    )

  property_amenitites_wide <- property_amenitites |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )


  # unit amenities ----------------------------------------------------------

  unit_amenities_unit_rating <- market_survey_data_lst$unit_amenities |>
    dplyr::filter(
      key == "Unit Rating"
    ) |>
    dplyr::pull(value)

  unit_amenities <- market_survey_data_lst$unit_amenities |>
    dplyr::filter(
      !is.na(key),
      key != "Unit Rating"
    )

  unit_amenities[13, "key"] <- "TV Included in Rent"
  unit_amenities[14, "key"] <- "TV Renatable Rate"
  unit_amenities[15, "key"] <- "TV Bedroom"
  unit_amenities[16, "key"] <- "TV Common Area"
  unit_amenities[18, "key"] <- "Furniture Included in Rent"
  unit_amenities[19, "key"] <- "Furniture Renatable Rate"
  unit_amenities[21, "key"] <- "Floor Premium"
  unit_amenities[22, "key"] <- "Poolside Premium"
  unit_amenities[23, "key"] <- "Top Floor Premium"
  unit_amenities[24, "key"] <- "View Premium"
  unit_amenities[25, "key"] <- "Other Premiums"

  unit_amenities <- unit_amenities |>
    dplyr::filter(
      !(key %in% c("TV", "Furniture", "Other Premiums"))
    )

  unit_amenities_wide <- unit_amenities |>
    tidyr::pivot_wider(
      names_from = key,
      values_from = value
    )

  amenities_lst <- list(
    property_amenities_common_area_rating = property_amenities_common_area_rating,
    property_amenitites = property_amenitites,
    property_amenitites_wide = property_amenitites_wide,
    unit_amenities_unit_rating = unit_amenities_unit_rating,
    unit_amenities = unit_amenities,
    unit_amenities_wide = unit_amenities_wide
  )

  # parking -----------------------------------------------------------------

  parking <- market_survey_data_lst$parking |>
    dplyr::filter(
      !is.na(parking)
    ) |>
    dplyr::rename(
      parking_type = parking
    ) |>
    dplyr::mutate(
      included = ifelse(included == "Yes", TRUE, FALSE),
      required = ifelse(required == "Yes", TRUE, FALSE)
    ) |>
    dplyr::select(
      property_id,
      property_name,
      parking_type,
      required,
      included,
      amount
    )

  parking_lst <- list(
    parking = parking
  )

  # utilities ---------------------------------------------------------------

  utilities_summary_2 <- market_survey_data_lst$utilities_summary_2 |>
    dplyr::filter(
      !is.na(utility)
    )

  utilities_lst <- list(
    utilities_summary_1 = market_survey_data_lst$utilities_summary_1,
    utilities_summary_2 = utilities_summary_2
  )


  # notes -------------------------------------------------------------------

  notes_leasing_special <- market_survey_data_lst$notes[2, ]
  notes_property_notes <- market_survey_data_lst$notes[12, ]

  notes_lst <- list(
    notes_leasing_special = notes_leasing_special,
    notes_property_notes = notes_property_notes
  )


  # office hours -----------------------------------------------------------

  office_hours <- market_survey_data_lst$office_hours |>
    dplyr::filter(
      !is.na(day)
    )

  office_hours_lst <- list(
    office_hours = office_hours
  )


  # rents -------------------------------------------------------------------

  rents_by_floorplan <- market_survey_data_lst$rents_by_floorplan |>
    dplyr::filter(
      !is.na(floorplan_type)
    )

  average_rents_by_unit_type <- market_survey_data_lst$average_rents_by_unit_type |>
    dplyr::filter(
      !is.na(unit_type)
    )

  rents_lst <- list(
    rents_by_floorplan = rents_by_floorplan,
    average_rents_by_unit_type = average_rents_by_unit_type
  )


  # finish ------------------------------------------------------------------

  list(
    property_detail,
    property_summary_lst,
    leasing_summary_lst,
    short_term_leases_lst,
    fees_lst,
    amenities_lst,
    utilities_lst,
    parking_lst,
    notes_lst,
    office_hours_lst,
    rents_lst
  ) |>
    setNames(
      c(
        "property_detail",
        "property_summary",
        "leasing_summary",
        "short_term_leases",
        "fees",
        "amenities",
        "utilities",
        "parking",
        "notes",
        "office_hours",
        "rents"
      )
    )

}

get_property_data_for_csv <- function(property, data_lst = market_survey_data_by_property) {

  property_data <- purrr::pluck(
    data_lst,
    property
  )

  # get property_summary data
  property_summary_data <- property_data$property_summary$property_summary
  property_summary_data_wide <- property_data$property_summary$property_summary_wide

  # get leasing_summary data
  leasing_summary_data <- property_data$leasing_summary$leasing_summary
  leasing_summary_data_wide <- property_data$leasing_summary$leasing_summary_wide

  # get short_term_leases data
  short_term_leases_data <- property_data$short_term_leases$short_term_leases
  short_term_leases_data_wide <- property_data$short_term_leases$short_term_leases_wide

  # get fees data
  fees_text_amounts_data <- property_data$fees$fees_text_amounts
  fees_data <- property_data$fees$fees
  fees_data_wide <- property_data$fees$fees_wide

  # get amenities data
  property_amenitites_data <- property_data$amenities$property_amenitites
  property_amenitites_data_wide <- property_data$amenities$property_amenitites_wide
  unit_amenities_data <- property_data$amenities$unit_amenities
  unit_amenities_data_wide <- property_data$amenities$unit_amenities_wide

  # get utilities data
  utilities_summary_1_data <- property_data$utilities$utilities_summary_1
  utilities_summary_2_data <- property_data$utilities$utilities_summary_2

  # get notes data
  notes_leasing_special_data <- property_data$notes$notes_leasing_special
  notes_property_notes_data <- property_data$notes$notes_property_notes

  # get office hours data
  office_hours_data <- property_data$office_hours$office_hours

  # get rents data
  rents_by_floorplan_data <- property_data$rents$rents_by_floorplan
  average_rents_by_unit_type_data <- property_data$rents$average_rents_by_unit_type

  # get file names
  property_summary_file <- paste0("market_survey_", property, "_property_summary.csv")
  property_summary_wide_file <- paste0("market_survey_", property, "_property_summary_wide.csv")
  leasing_summary_file <- paste0("market_survey_", property, "_leasing_summary.csv")
  leasing_summary_wide_file <- paste0("market_survey_", property, "_leasing_summary_wide.csv")
  short_term_leases_file <- paste0("market_survey_", property, "_short_term_leases.csv")
  short_term_leases_wide_file <- paste0("market_survey_", property, "_short_term_leases_wide.csv")
  fees_text_amounts_file <- paste0("market_survey_", property, "_fees_text_amounts.csv")
  fees_file <- paste0("market_survey_", property, "_fees.csv")
  fees_wide_file <- paste0("market_survey_", property, "_fees_wide.csv")
  property_amenitites_file <- paste0("market_survey_", property, "_property_amenitites.csv")
  property_amenitites_wide_file <- paste0("market_survey_", property, "_property_amenitites_wide.csv")
  unit_amenities_file <- paste0("market_survey_", property, "_unit_amenities.csv")
  unit_amenities_wide_file <- paste0("market_survey_", property, "_unit_amenities_wide.csv")
  utilities_summary_1_file <- paste0("market_survey_", property, "_utilities_summary_1.csv")
  utilities_summary_2_file <- paste0("market_survey_", property, "_utilities_summary_2.csv")
  notes_leasing_special_file <- paste0("market_survey_", property, "_notes_leasing_special.csv")
  notes_property_notes_file <- paste0("market_survey_", property, "_notes_property_notes.csv")
  office_hours_file <- paste0("market_survey_", property, "_office_hours.csv")
  rents_by_floorplan_file <- paste0("market_survey_", property, "_rents_by_floorplan.csv")
  average_rents_by_unit_type_file <- paste0("market_survey_", property, "_average_rents_by_unit_type.csv")

  # return list ready to pass to readr write csv
  list(
    property_summary = list(
      property_summary_data = property_summary_data,
      property_summary_file = property_summary_file,
      property_summary_data_wide = property_summary_data_wide,
      property_summary_wide_file = property_summary_wide_file
    ),
    leasing_summary = list(
      leasing_summary_data = leasing_summary_data,
      leasing_summary_file = leasing_summary_file,
      leasing_summary_data_wide = leasing_summary_data_wide,
      leasing_summary_wide_file = leasing_summary_wide_file
    ),
    short_term_leases = list(
      short_term_leases_data = short_term_leases_data,
      short_term_leases_file = short_term_leases_file,
      short_term_leases_data_wide = short_term_leases_data_wide,
      short_term_leases_wide_file = short_term_leases_wide_file
    ),
    fees = list(
      fees_text_amounts_data = fees_text_amounts_data,
      fees_text_amounts_file = fees_text_amounts_file,
      fees_data = fees_data,
      fees_file = fees_file,
      fees_data_wide = fees_data_wide,
      fees_wide_file = fees_wide_file
    ),
    amenities = list(
      property_amenitites_data = property_amenitites_data,
      property_amenitites_file = property_amenitites_file,
      property_amenitites_data_wide = property_amenitites_data_wide,
      property_amenitites_wide_file = property_amenitites_wide_file,
      unit_amenities_data = unit_amenities_data,
      unit_amenities_file = unit_amenities_file,
      unit_amenities_data_wide = unit_amenities_data_wide,
      unit_amenities_wide_file = unit_amenities_wide_file
    ),
    utilities = list(
      utilities_summary_1_data = utilities_summary_1_data,
      utilities_summary_1_file = utilities_summary_1_file,
      utilities_summary_2_data = utilities_summary_2_data,
      utilities_summary_2_file = utilities_summary_2_file
    ),
    notes = list(
      notes_leasing_special_data = notes_leasing_special_data,
      notes_leasing_special_file = notes_leasing_special_file,
      notes_property_notes_data = notes_property_notes_data,
      notes_property_notes_file = notes_property_notes_file
    ),
    office_hours = list(
      office_hours_data = office_hours_data,
      office_hours_file = office_hours_file
    ),
    rents = list(
      rents_by_floorplan_data = rents_by_floorplan_data,
      rents_by_floorplan_file = rents_by_floorplan_file,
      average_rents_by_unit_type_data = average_rents_by_unit_type_data,
      average_rents_by_unit_type_file = average_rents_by_unit_type_file
    )
  )

}

save_market_survey_csvs <- function(
    data_by_property,
    csv_dir = "data-raw/data/working",
    nest_folders_by_property = TRUE,
    nest_folders_by_section = TRUE
) {

  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  purrr::walk(
    names(data_by_property),
    function(x) {

      property <- x

      csv_dir <- if (nest_folders_by_property) {
        file.path(csv_dir, property)
      } else {
        csv_dir
      }

      if (!dir.exists(csv_dir)) {
        dir.create(csv_dir, recursive = TRUE)
      }

      property_data <- purrr::pluck(
        data_by_property,
        property
      )

      # get property_summary data
      property_summary_data <- property_data$property_summary$property_summary
      property_summary_data_wide <- property_data$property_summary$property_summary_wide

      # get leasing_summary data
      leasing_summary_data <- property_data$leasing_summary$leasing_summary
      leasing_summary_data_wide <- property_data$leasing_summary$leasing_summary_wide

      # get short_term_leases data
      short_term_leases_data <- property_data$short_term_leases$short_term_leases
      short_term_leases_data_wide <- property_data$short_term_leases$short_term_leases_wide

      # get fees data
      fees_text_amounts_data <- property_data$fees$fees_text_amounts
      fees_data <- property_data$fees$fees
      fees_data_wide <- property_data$fees$fees_wide

      # get amenities data
      property_amenitites_data <- property_data$amenities$property_amenitites
      property_amenitites_data_wide <- property_data$amenities$property_amenitites_wide
      unit_amenities_data <- property_data$amenities$unit_amenities
      unit_amenities_data_wide <- property_data$amenities$unit_amenities_wide

      # get utilities data
      utilities_summary_1_data <- property_data$utilities$utilities_summary_1
      utilities_summary_2_data <- property_data$utilities$utilities_summary_2

      # get notes data
      notes_leasing_special_data <- property_data$notes$notes_leasing_special
      notes_property_notes_data <- property_data$notes$notes_property_notes

      # get office hours data
      office_hours_data <- property_data$office_hours$office_hours

      # get rents data
      rents_by_floorplan_data <- property_data$rents$rents_by_floorplan
      average_rents_by_unit_type_data <- property_data$rents$average_rents_by_unit_type

      # get file names
      property_summary_file <- paste0("market_survey_", property, "_property_summary.csv")
      property_summary_wide_file <- paste0("market_survey_", property, "_property_summary_wide.csv")
      leasing_summary_file <- paste0("market_survey_", property, "_leasing_summary.csv")
      leasing_summary_wide_file <- paste0("market_survey_", property, "_leasing_summary_wide.csv")
      short_term_leases_file <- paste0("market_survey_", property, "_short_term_leases.csv")
      short_term_leases_wide_file <- paste0("market_survey_", property, "_short_term_leases_wide.csv")
      fees_text_amounts_file <- paste0("market_survey_", property, "_fees_text_amounts.csv")
      fees_file <- paste0("market_survey_", property, "_fees.csv")
      fees_wide_file <- paste0("market_survey_", property, "_fees_wide.csv")
      property_amenitites_file <- paste0("market_survey_", property, "_property_amenitites.csv")
      property_amenitites_wide_file <- paste0("market_survey_", property, "_property_amenitites_wide.csv")
      unit_amenities_file <- paste0("market_survey_", property, "_unit_amenities.csv")
      unit_amenities_wide_file <- paste0("market_survey_", property, "_unit_amenities_wide.csv")
      utilities_summary_1_file <- paste0("market_survey_", property, "_utilities_summary_1.csv")
      utilities_summary_2_file <- paste0("market_survey_", property, "_utilities_summary_2.csv")
      notes_leasing_special_file <- paste0("market_survey_", property, "_notes_leasing_special.csv")
      notes_property_notes_file <- paste0("market_survey_", property, "_notes_property_notes.csv")
      office_hours_file <- paste0("market_survey_", property, "_office_hours.csv")
      rents_by_floorplan_file <- paste0("market_survey_", property, "_rents_by_floorplan.csv")
      average_rents_by_unit_type_file <- paste0("market_survey_", property, "_average_rents_by_unit_type.csv")

      readr::write_csv(property_summary_data, file.path(csv_dir, property_summary_file))
      readr::write_csv(property_summary_data_wide, file.path(csv_dir, property_summary_wide_file))
      readr::write_csv(leasing_summary_data, file.path(csv_dir, leasing_summary_file))
      readr::write_csv(leasing_summary_data_wide, file.path(csv_dir, leasing_summary_wide_file))
      readr::write_csv(short_term_leases_data, file.path(csv_dir, short_term_leases_file))
      readr::write_csv(short_term_leases_data_wide, file.path(csv_dir, short_term_leases_wide_file))
      readr::write_csv(fees_text_amounts_data, file.path(csv_dir, fees_text_amounts_file))
      readr::write_csv(fees_data, file.path(csv_dir, fees_file))
      readr::write_csv(fees_data_wide, file.path(csv_dir, fees_wide_file))
      readr::write_csv(property_amenitites_data, file.path(csv_dir, property_amenitites_file))
      readr::write_csv(property_amenitites_data_wide, file.path(csv_dir, property_amenitites_wide_file))
      readr::write_csv(unit_amenities_data, file.path(csv_dir, unit_amenities_file))
      readr::write_csv(unit_amenities_data_wide, file.path(csv_dir, unit_amenities_wide_file))
      readr::write_csv(utilities_summary_1_data, file.path(csv_dir, utilities_summary_1_file))
      readr::write_csv(utilities_summary_2_data, file.path(csv_dir, utilities_summary_2_file))
      readr::write_csv(notes_leasing_special_data, file.path(csv_dir, notes_leasing_special_file))
      readr::write_csv(notes_property_notes_data, file.path(csv_dir, notes_property_notes_file))
      readr::write_csv(office_hours_data, file.path(csv_dir, office_hours_file))
      readr::write_csv(rents_by_floorplan_data, file.path(csv_dir, rents_by_floorplan_file))
      readr::write_csv(average_rents_by_unit_type_data, file.path(csv_dir, average_rents_by_unit_type_file))

      cli::cli_alert_info("Saved data for property {.field {property}} to path: {.path {csv_dir}}")

    }
  )

  cli::cli_alert_info("Saved all market survey data to csvs at path: {.path {csv_dir}}")

}

