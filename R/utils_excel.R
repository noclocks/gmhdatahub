
#  ------------------------------------------------------------------------
#
# Title : Excel Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------


# report name -----------------------------------------------------------------------------------------------------

#' Get Excel Report File Name
#'
#' @description
#' This function generates a file name for an Excel report based on the report name and date.
#'
#' For example, if the report name is **"GMH Report"** and the date is **"2025-01-01"**,
#' the function will return **"2025-01-01-GMH_Report.xlsx"**.
#'
#' @param report_name The name of the report.
#' @param report_date The date of the report. Defaults to the current date.
#'
#' @returns
#' A string containing the file name for the Excel report.
#'
#' @export
#'
#' @importFrom snakecase to_title_case
#' @importFrom stringr str_replace_all
get_xl_report_file_name <- function(
    report_name,
    report_date = Sys.Date()
) {

  report_date_str <- format(as.Date(report_date), "%Y-%m-%d")
  report_file_ext <- ".xlsx"
  abbrs <- c("GMH", "AGC", "CBRE", "JHU", "AEW", "CRG")
  replace <- c("Gmh" = "GMH", "Agc" = "AGC", "Cbre" = "CBRE", "Jhu" = "JHU", "Aew" = "AEW", "Crg" = "CRG")

  report_name <- snakecase::to_title_case(report_name) |>
    stringr::str_replace_all(" ", "_") |>
    stringr::str_replace_all(replace)

  paste0(
    report_date_str,
    "-",
    report_name,
    report_file_ext
  )

}

# cells -----------------------------------------------------------------------------------------------------------

#' Get Cells
#'
#' @description
#' This function retrieves cell data from an Excel file using the `tidyxl` package.
#'
#' @param file The path to the Excel file.
#' @param sheets A vector of sheet names to read. If `NULL`, all sheets will be read.
#'
#' @returns
#' A data frame containing cell data.
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom tidyxl xlsx_cells
xl_get_cells <- function(file, sheets = NULL) {

  if (is.null(sheets)) sheets <- tidyxl::xlsx_sheet_names(file)

  tidyxl::xlsx_cells(
    file,
    sheets = sheets,
    include_blank_cells = FALSE
  ) |>
    dplyr::select(
      "sheet",
      "address",
      "row",
      "col",
      "data_type",
      "character"
    )
}

# pre-lease -------------------------------------------------------------------------------------------------------

#' Pre-Lease Excel Data
#'
#' @name utils_excel_pre_lease
#'
#' @description
#' Set of functions to read and process GMH Pre-Lease Excel data.
#'
#' - `xl_read_pre_lease_global_summary_tbl()`: Reads the global summary table from the Excel file.
#' - `xl_read_pre_lease_property_summary_tbls()`: Reads the property summary tables from the Excel file.
#' - `xl_read_pre_lease_property_details_tbls()`: Reads the property details tables from the Excel file.
#' - `xl_read_pre_lease_report_parameters()`: Reads the report parameters from the Excel file.
#'
#' @param xl_file Path to the Excel file.
#'
#' @returns
#' - `xl_read_pre_lease_global_summary_tbl()`: A data frame containing the global summary table.
#' - `xl_read_pre_lease_property_summary_tbls()`: A data frame containing the merged property summary tables.
#' - `xl_read_pre_lease_property_details_tbls()`: A data frame containing the merged property details tables.
#' - `xl_read_pre_lease_report_parameters()`: A data frame containing the report parameters.
NULL


#' @rdname utils_excel_pre_lease
#' @export
#' @importFrom cli cli_abort
#' @importFrom dplyr filter pull slice
#' @importFrom readxl excel_sheets read_excel
xl_read_pre_lease_global_summary_tbl <- function(xl_file) {

  sheets <- readxl::excel_sheets(xl_file)

  if (!"Summary" %in% sheets) {
    cli::cli_abort("Summary sheet not found in {.file {xl_file}}.")
  }

  cells <- xl_get_cells(xl_file, "Summary")

  # split between numeric & character
  numeric_cells <- cells |>
    dplyr::filter(
      .data$sheet == "Summary",
      .data$data_type == "numeric"
    )

  character_cells <- cells |>
    dplyr::filter(
      .data$sheet == "Summary",
      .data$data_type == "character"
    )

  prop_names <- xl_get_pre_lease_property_sheets(xl_file)

  # get last cell with any (numeric) data
  last_cell <- numeric_cells |>
    dplyr::filter(
      .data$row == max(.data$row, na.rm = TRUE),
      .data$col == max(.data$col, na.rm = TRUE)
    ) |>
    dplyr::slice(1L) |>
    dplyr::pull("address")

  # get start cell
  start_cell <- character_cells |>
    dplyr::filter(
      .data$sheet == "Summary",
      .data$character %in% prop_names
    ) |>
    dplyr::slice(1L) |>
    dplyr::pull("address")

  range <- paste0(start_cell, ":", last_cell)

  col_specs <- .pre_lease_global_summary_tbl_col_specs()
  col_names <- names(col_specs)
  col_types <- unlist(unname(col_specs))

  # read table
  readxl::read_excel(
    xl_file,
    sheet = "Summary",
    range = range,
    col_names = col_names,
    col_types = col_types,
    na = c("", "N/A", "n/a", "NA", "-", NA)
  )
}

#' @rdname utils_excel_pre_lease
#' @export
#' @importFrom dplyr select mutate
#' @importFrom purrr map_dfr
#' @importFrom readxl read_excel
#' @importFrom tidyselect everything
xl_read_pre_lease_property_summary_tbls <- function(xl_file) {

  sheets <- xl_get_pre_lease_property_sheets(xl_file)

  purrr::map_dfr(
    sheets,
    function(sheet) {
      cells <- xl_get_cells(xl_file, sheet)
      ranges <- xl_get_pre_lease_summary_tbl_ranges(cells)
      range <- ranges$range[ranges$sheet == sheet]
      col_specs <- .pre_lease_property_summary_tbl_col_specs()
      col_names <- names(col_specs)
      col_types <- unlist(unname(col_specs))

      readxl::read_excel(
        xl_file,
        sheet = sheet,
        range = range,
        col_names = col_names,
        col_types = col_types,
        na = c("", "N/A", "n/a", "NA", "-", NA)
      ) |>
        dplyr::mutate(
          property_name = sheet
        ) |>
        dplyr::select(
          "property_name",
          tidyselect::everything()
        )
    }
  )

}

#' @rdname utils_excel_pre_lease
#' @export
#' @importFrom dplyr select filter mutate
#' @importFrom purrr map_dfr
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect
#' @importFrom tidyselect everything
xl_read_pre_lease_property_details_tbls <- function(xl_file, charge_codes = FALSE) {

  sheets <- xl_get_pre_lease_property_sheets(xl_file)

  purrr::map_dfr(
    sheets,
    function(sheet) {
      cells <- xl_get_cells(xl_file, sheet)

      if (charge_codes) {
        col_specs <- .pre_lease_property_details_tbl_col_specs_w_charge_codes()
        ranges <- xl_get_pre_lease_details_tbl_ranges(cells, stop_cell_col = "Q")
      } else {
        col_specs <- .pre_lease_property_details_tbl_col_specs()
        ranges <- xl_get_pre_lease_details_tbl_ranges(cells)
      }

      range <- ranges$range[ranges$sheet == sheet]

      col_names <- names(col_specs)
      col_types <- unlist(unname(col_specs))

      hold <- readxl::read_excel(
        xl_file,
        sheet = sheet,
        range = range,
        col_names = col_names,
        col_types = col_types,
        na = c("", "N/A", "n/a", "NA", "-", NA)
      ) |>
        dplyr::mutate(
          property_name = sheet
        ) |>
        dplyr::filter(
          !stringr::str_detect(.data$bldg_unit, "Unit Type:"),
          !stringr::str_detect(.data$bldg_unit, "Total/Average:")
        ) |>
        dplyr::select(
          "property_name",
          tidyselect::everything()
        )

      if (charge_codes) {
        hold |>
          dplyr::filter(.data$charge_code != "Charge Total:")
      } else {
        hold
      }
    }
  )

}

#' @rdname utils_excel_pre_lease
#' @export
#' @importFrom cellranger anchored
#' @importFrom cli cli_abort
#' @importFrom dplyr select mutate filter
#' @importFrom lubridate mdy mdy_hm
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect everything
xl_read_pre_lease_report_parameters <- function(xl_file) {

  sheets <- readxl::excel_sheets(xl_file)

  if (!"Report Parameters" %in% sheets) {
    cli::cli_abort("Report Parameters sheet not found in {.file {xl_file}}.")
  }

  cells <- xl_get_cells(xl_file, "Report Parameters")

  readxl::read_excel(
    xl_file,
    sheet = "Report Parameters",
    range = cellranger::anchored("A4", dim = c(18, 2)),
    col_names = c("key", "value"),
    col_types = c("text", "text"),
    na = c("", "N/A", "n/a", "NA", "-", NA)
  ) |>
    dplyr::filter(.data$key != "") |>
    tidyr::pivot_wider(
      names_from = "key",
      values_from = "value"
    ) |>
    setNames(names(.pre_lease_report_parameters_tbl_col_specs())) |>
    dplyr::mutate(
      xl_file = basename(xl_file),
      period_date = lubridate::mdy(.data$period_date),
      generated_date = lubridate::mdy_hm(.data$generated_date),
      data_as_of_date = lubridate::mdy_hm(.data$data_as_of_date)
    ) |>
    dplyr::select(
      "xl_file",
      tidyselect::everything()
    )

}

#' @keywords internal
#' @noRd
#' @importFrom readxl excel_sheets
xl_get_pre_lease_property_sheets <- function(xl_file) {
  non_property_sheets <- c("Summary", "Report Parameters")
  setdiff(readxl::excel_sheets(xl_file), non_property_sheets)
}

#' @keywords internal
#' @noRd
#' @importFrom dplyr mutate select ungroup slice group_by filter
#' @importFrom stringr str_detect
xl_get_pre_lease_summary_tbl_ranges <- function(
    cells,
    start_cell = "A9",
    stop_cell_col = "O",
    stop_cell_row_offset = -1
) {

  cells |>
    dplyr::select("sheet", "address", "row", "col", "character") |>
    dplyr::filter(stringr::str_detect(character, "Total/Average:")) |>
    dplyr::group_by(.data$sheet) |>
    dplyr::slice(1L) |>
    dplyr::mutate(
      start_cell = .env$start_cell,
      stop_cell = paste0(.env$stop_cell_col, .data$row + .env$stop_cell_row_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "sheet",
      "start_cell",
      "stop_cell"
    ) |>
    dplyr::mutate(
      range = paste0(.data$start_cell, ":", .data$stop_cell)
    )
}

#' @keywords internal
#' @noRd
#' @importFrom dplyr mutate inner_join select filter ungroup slice group_by
#' @importFrom stringr str_detect
xl_get_pre_lease_details_tbl_ranges <- function(
    cells,
    start_cell_offset = 4L,
    stop_cell_col = "O",
    stop_cell_row_offset = -1L
) {

  cells |>
    dplyr::select("sheet", "address", "row", "col", "character") |>
    dplyr::filter(
      .data$col == 1L,
      stringr::str_detect(.data$character, "Details")
    ) |>
    dplyr::mutate(
      start_cell = paste0("A", .data$row + .env$start_cell_offset)
    ) |>
    dplyr::select(
      "sheet",
      "start_cell"
    ) |>
    dplyr::inner_join(
      cells |>
        dplyr::filter(
          stringr::str_detect(.data$character, "Total/Average:")
        ) |>
        dplyr::group_by(.data$sheet) |>
        dplyr::slice(2L) |>
        dplyr::mutate(
          stop_cell = paste0(.env$stop_cell_col, .data$row + .env$stop_cell_row_offset)
        ) |>
        dplyr::ungroup() |>
        dplyr::select("sheet", "stop_cell"),
      by = "sheet"
    ) |>
    dplyr::mutate(
      range = paste0(.data$start_cell, ":", .data$stop_cell)
    )

}

#' @keywords internal
#' @noRd
#' @importFrom dplyr mutate select ungroup slice group_by filter
#' @importFrom stringr str_detect
xl_get_pre_lease_unit_type_ranges <- function(
    cells,
    start_cell = "A9",
    stop_cell_col = "A",
    stop_cell_row_offset = -1
) {

  cells |>
    dplyr::select("sheet", "address", "row", "col", "character") |>
    dplyr::filter(stringr::str_detect(character, "Total/Average:")) |>
    dplyr::group_by(.data$sheet) |>
    dplyr::slice(1L) |>
    dplyr::mutate(
      start_cell = start_cell,
      stop_cell = paste0(stop_cell_col, .data$row + stop_cell_row_offset)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "sheet",
      "start_cell",
      "stop_cell"
    ) |>
    dplyr::mutate(
      range = paste0(.data$start_cell, ":", .data$stop_cell)
    )

}

# survey excel data -----------------------------------------------------------------------------------------------

xl_get_market_survey_sheets <- function(xl_file) {

  ignore_sheets <- c(
    "Dashboard",
    "Data",
    "Log Sheet",
    "Historical Market Data",
    "PropertyTemplate",
    "Comparison",
    "Proposed Rate Comparison",
    "SWOT Analysis",
    "Test"
  )

  sheets <- readxl::excel_sheets(xl_file) |>
    setdiff(ignore_sheets)

  property_sheet <- sheets[1]
  competitor_sheets <- sheets[-1]

  list(
    property = property_sheet,
    competitors = competitor_sheets
  )

}

#' Load Market Survey Data from the Excel Workbook Template
#'
#' @description
#' This function loads market survey data from Excel files in a specified directory.
#'
#' @param root_xl_path Root directory containing Excel files
#' @param ignore_sheets Vector of sheet names to ignore. Defaults to a set of common sheets if left `NULL`.
#' @param output_cache Path to save cached data (optional)
#'
#' @returns
#' List of survey data organized by file and sheet
#'
#' @export
#'
#' @importFrom cli cli_h1 cli_alert_info cli_abort cli_alert_success
#' @importFrom fs file_exists path dir_exists dir_ls dir_create path_dir
#' @importFrom purrr map
#' @importFrom qs2 qs_read qs_save
#' @importFrom readxl excel_sheets
#' @importFrom stats setNames
xl_read_market_survey_data <- function(
  root_xl_path,
  ignore_sheets = NULL,
  output_cache = NULL
) {

  cli::cli_h1("Loading Market Survey Data")

  # Default sheets to ignore if none provided
  if (is.null(ignore_sheets)) {
    ignore_sheets <- c(
      "Dashboard",
      "Data",
      "Log Sheet",
      "Historical Market Data",
      "PropertyTemplate",
      "Comparison",
      "Proposed Rate Comparison",
      "SWOT Analysis",
      "Test"
    )
  }

  # Check if using cached data
  if (!is.null(output_cache) && fs::file_exists(output_cache)) {
    cli::cli_alert_info("Using cached survey data from: {.file {output_cache}}")
    return(qs2::qs_read(output_cache))
  }

  # .xlsm or .xlsx + .zip & ignoring and files with ~$
  xl_files <- fs::dir_ls(root_xl_path, type = "file", regexp = "\\.xlsm$|\\.xlsx$|.zip$", recurse = TRUE) |>
    purrr::discard(~ stringr::str_detect(.x, "~\\$"))

  # Get sheets for each file
  xl_file_sheets <- purrr::map(xl_files, readxl::excel_sheets) |>
    stats::setNames(basename(xl_files)) |>
    purrr::map(~ setdiff(.x, ignore_sheets))

  # Define specs for market survey sections
  market_survey_specs <- .market_survey_specs()

  # Process workbooks
  all_survey_data <- process_survey_workbooks(
    xl_files = xl_files,
    xl_file_sheets = xl_file_sheets,
    market_survey_specs = market_survey_specs
  )

  # Cache results if needed
  if (!is.null(output_cache)) {
    # Ensure directory exists
    fs::dir_create(fs::path_dir(output_cache), recurse = TRUE)
    qs2::qs_save(all_survey_data, file = output_cache)
    cli::cli_alert_success("Cached survey data to: {output_cache}")
  }

  return(all_survey_data)
}




#' Read a single sheet from an Excel workbook
#'
#' @param wb_path Path to Excel workbook
#' @param sheet Sheet name to read
#' @param specs Market survey specifications
#' @return List of data frames, one for each section
#'
#' @keywords internal
read_sheet_data <- function(wb_path, sheet, specs) {

  # Special case for Rise at Northgate
  if (stringr::str_detect(basename(wb_path), "Rise at Northgate")) {
    specs$property_summary$range <- "A3:B18"
  }

  tryCatch({
    result <- purrr::map(
      names(specs),
      function(section_name) {
        section_spec <- specs[[section_name]]

        readxl::read_excel(
          wb_path,
          sheet = sheet,
          range = section_spec$range,
          col_names = section_spec$col_names,
          na = c("", "N/A", "n/a", "NA", "-", NA)
        ) |>
          # Remove x, xx, xxx columns
          dplyr::select(-dplyr::matches("^x+$"))
      }
    ) |>
      stats::setNames(names(specs))

    return(result)
  },
  error = function(e) {
    cli::cli_alert_warning("Error reading sheet '{sheet}' from '{basename(wb_path)}': {e$message}")
    return(NULL)
  })
}

#' Process multiple Excel workbooks
#'
#' @param xl_files Vector of Excel file paths
#' @param xl_file_sheets Named list of sheets to process for each file
#' @param market_survey_specs Named list of range specifications
#' @return List of all survey data
#'
#' @keywords internal
process_survey_workbooks <- function(
    xl_files,
    xl_file_sheets,
    market_survey_specs
) {

  # Start timer
  start_time <- Sys.time()
  cli::cli_alert_info(
    c(
      "Starting to process {.field {length(xl_files)}} workbooks with a total of ",
      "{.field {sum(purrr::map_int(xl_file_sheets, length))}} sheets"
    )
  )

  # Process all workbooks and sheets
  all_survey_data <- list()

  # Main processing loop
  total_processed <- 0
  total_sheets <- sum(purrr::map_int(xl_file_sheets, length))

  for (file_path in xl_files) {
    file_basename <- basename(file_path)

    # Skip if no sheet list for this file
    if (!file_basename %in% names(xl_file_sheets)) {
      cli::cli_alert_warning("No sheet list provided for file: {file_basename}")
      next
    }

    # Get sheets for this file
    sheets <- xl_file_sheets[[file_basename]]

    # Process each sheet
    file_data <- list()

    for (sheet in sheets) {
      total_processed <- total_processed + 1
      cli::cli_alert_info("Processing {total_processed}/{total_sheets}: {file_basename} - {sheet}")

      # Read sheet data
      sheet_data <- read_sheet_data(file_path, sheet, market_survey_specs)

      if (!is.null(sheet_data)) {
        file_data[[sheet]] <- sheet_data
      }
    }

    if (length(file_data) > 0) {
      all_survey_data[[file_basename]] <- file_data
    }
  }

  # Report processing time
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  cli::cli_alert_success("Processing completed in {round(as.numeric(duration), 2)} minutes")

  return(all_survey_data)
}

#' Process all survey data and extract property summaries
#'
#' @param all_survey_data The loaded survey data
#' @param valid_prop_names Vector of property names that aren't competitors
#' @param output_path Path to save the results
#' @return Data frame of processed property summaries
#'
#' @export
process_property_summaries <- function(all_survey_data, valid_prop_names = NULL, output_path = NULL) {
  cli::cli_h2("Extracting property summaries")

  # Process all workbooks
  prop_summs <- purrr::map_dfr(
    names(all_survey_data),
    function(file_name) {
      cli::cli_alert_info("Processing file: {file_name}")

      # Process each sheet in the file
      purrr::map_dfr(
        names(all_survey_data[[file_name]]),
        function(sheet_name) {
          # Get the survey data for this sheet
          survey_data <- all_survey_data[[file_name]][[sheet_name]]

          # Skip if needed sections are missing
          if (is.null(survey_data) || !"property_summary" %in% names(survey_data)) {
            return(NULL)
          }

          # Get property summary data
          property_data <- survey_data$property_summary

          # Skip if invalid data
          if (is.null(property_data) || nrow(property_data) == 0 ||
              !all(c("key", "value") %in% names(property_data))) {
            return(NULL)
          }

          # Extract property name
          prop_name <- property_data |>
            dplyr::filter(tolower(key) == "property") |>
            dplyr::pull(value)

          if (length(prop_name) == 0 || is.na(prop_name)) {
            prop_name <- sheet_name  # Fallback to sheet name
          }

          # Make specific corrections
          if (prop_name == "1047 Commonwealth") prop_name <- "1047 Commonwealth Avenue"
          if (prop_name == "Academy at Frisco") prop_name <- "The Academy at Frisco"

          # Handle address line 2 (often has NA key)
          property_data <- property_data |>
            dplyr::mutate(
              key = dplyr::if_else(is.na(key) & !is.na(value), "Address Line 2", key)
            )

          # Determine competitor status
          is_competitor <- TRUE
          if (!is.null(valid_prop_names)) {
            is_competitor <- !(prop_name %in% valid_prop_names)
          }

          # Process the data
          property_data |>
            dplyr::filter(!is.na(key)) |>
            dplyr::mutate(
              property_name = prop_name,
              section = "Property Summary",
              property_type = ifelse(is_competitor, "Competitor", "Property"),
              file_name = file_name,
              sheet_name = sheet_name
            ) |>
            dplyr::select(property_name, property_type, file_name, sheet_name, key, value) |>
            tidyr::pivot_wider(names_from = key, values_from = value) |>
            janitor::clean_names()
        }
      )
    }
  )

  # Safe date conversion with explicit handling for different formats
  safe_date_convert <- function(x) {
    if (is.na(x) || x %in% c("n/a", "N/A", "-", "NA")) {
      return(as.Date(NA))
    }

    # Try several date formats
    if (nchar(stringr::str_trim(x)) == 4 && stringr::str_detect(x, "^\\d{4}$")) {
      # Year only
      return(as.Date(paste0(x, "-01-01")))
    } else if (stringr::str_detect(x, "^\\d{5}$")) {
      # Excel date number
      return(suppressWarnings(as.Date(as.numeric(x), origin = "1899-12-30")))
    } else {
      # Return NA for any other format
      return(as.Date(NA))
    }
  }

  # Apply data cleaning
  cli::cli_alert_info("Cleaning property summary data")

  # Apply date conversion safely
  if ("most_recent_sale" %in% names(prop_summs)) {
    prop_summs <- prop_summs |>
      dplyr::mutate(
        most_recent_sale = as.character(most_recent_sale),
        most_recent_sale = purrr::map_vec(most_recent_sale, safe_date_convert)
      )
  }

  # Fix other data types
  if ("property_rating" %in% names(prop_summs)) {
    prop_summs <- prop_summs |>
      dplyr::mutate(
        property_rating = as.character(property_rating),
        property_rating = suppressWarnings(as.numeric(property_rating))
      )
  }

  # Clean up year built field
  if ("year_built_renovated" %in% names(prop_summs)) {
    prop_summs <- prop_summs |>
      dplyr::mutate(
        year_built_renovated = as.character(year_built_renovated),
        year_built_renovated = dplyr::case_when(
          is.na(year_built_renovated) ~ NA_character_,
          year_built_renovated %in% c("n/a", "N/A", "-", "NA") ~ NA_character_,
          stringr::str_detect(year_built_renovated, "/") ~
            stringr::str_extract(year_built_renovated, "^\\d{4}"),
          TRUE ~ year_built_renovated
        ),
        year_built_renovated = suppressWarnings(as.integer(year_built_renovated))
      )
  }

  # Standardize phone numbers
  if ("phone_number" %in% names(prop_summs)) {
    prop_summs <- prop_summs |>
      dplyr::mutate(
        phone_number = as.character(phone_number),
        phone_clean = stringr::str_replace_all(phone_number, "[^0-9]", ""),
        phone_number = dplyr::case_when(
          stringr::str_length(phone_clean) == 10 ~
            stringr::str_c("(", stringr::str_sub(phone_clean, 1, 3), ") ",
                           stringr::str_sub(phone_clean, 4, 6), "-",
                           stringr::str_sub(phone_clean, 7, 10)),
          TRUE ~ phone_number
        )
      ) |>
      dplyr::select(-phone_clean)
  }

  # Fix website URLs
  if ("website" %in% names(prop_summs)) {
    prop_summs <- prop_summs |>
      dplyr::mutate(
        website = dplyr::case_when(
          is.na(website) ~ NA_character_,
          website %in% c("n/a", "N/A", "-", "NA") ~ NA_character_,
          !stringr::str_detect(website, "^http") ~ paste0("https://", website),
          TRUE ~ website
        )
      )
  }

  # Save results if path provided
  if (!is.null(output_path)) {
    # Ensure output directory exists
    fs::dir_create(fs::path_dir(output_path), recurse = TRUE)

    readr::write_csv(prop_summs, output_path)
    cli::cli_alert_success("Property summaries saved to: {output_path}")
  }

  return(prop_summs)
}

#' Extract and process rents by floorplan data
#'
#' @param all_survey_data The loaded survey data
#' @param output_path Path to save the results
#' @return Data frame of processed rents data
#'
#' @export
process_rents_data <- function(all_survey_data, output_path = NULL) {
  cli::cli_h2("Extracting rents by floorplan")

  # Process all workbooks
  rents_data <- purrr::map_dfr(
    names(all_survey_data),
    function(file_name) {
      cli::cli_alert_info("Processing file: {file_name}")

      # Process each sheet in the file
      purrr::map_dfr(
        names(all_survey_data[[file_name]]),
        function(sheet_name) {
          # Get the survey data for this sheet
          survey_data <- all_survey_data[[file_name]][[sheet_name]]

          # Skip if rents section is missing
          if (is.null(survey_data) || !"rents_by_floorplan" %in% names(survey_data)) {
            return(NULL)
          }

          # Get property name from property summary if available
          prop_name <- NULL
          if ("property_summary" %in% names(survey_data)) {
            prop_data <- survey_data$property_summary
            if (!is.null(prop_data) && "key" %in% names(prop_data) && "value" %in% names(prop_data)) {
              prop_row <- prop_data |>
                dplyr::filter(tolower(key) == "property")

              if (nrow(prop_row) > 0) {
                prop_name <- prop_row$value[1]
              }
            }
          }

          # If property name not found, use sheet name
          if (is.null(prop_name) || length(prop_name) == 0 || is.na(prop_name)) {
            prop_name <- sheet_name
          }

          # Apply specific name corrections
          if (prop_name == "1047 Commonwealth") prop_name <- "1047 Commonwealth Avenue"
          if (prop_name == "Academy at Frisco") prop_name <- "The Academy at Frisco"

          # Get rents data
          rents_data <- survey_data$rents_by_floorplan

          # Skip if invalid data
          if (is.null(rents_data) || nrow(rents_data) == 0) {
            return(NULL)
          }

          # Process rents data
          rents_data |>
            dplyr::filter(!is.na(floorplan_type)) |>
            dplyr::mutate(
              property_name = prop_name,
              file_name = file_name,
              sheet_name = sheet_name
            ) |>
            dplyr::select(property_name, file_name, sheet_name, dplyr::everything())
        }
      )
    }
  )

  # Convert numeric columns
  numeric_cols <- c(
    "count", "sf_per_bed", "bed", "bath", "available",
    "market_rent_per_bed", "market_rent_per_sf",
    "concessions_gift_card", "concessions_one_time_rent", "concessions_monthly_rent",
    "effective_rent_per_bed", "effective_rent_per_sf",
    dplyr::starts_with("additional_monthly_expenses_"),
    "bundled_rent_per_bed", "bundled_rent_per_sf"
  )

  # Ensure columns exist before trying to convert them
  existing_numeric_cols <- intersect(numeric_cols, names(rents_data))

  if (length(existing_numeric_cols) > 0) {
    rents_data <- rents_data |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(existing_numeric_cols),
        ~ suppressWarnings(as.numeric(.x))
      ))
  }

  # Save results if path provided
  if (!is.null(output_path)) {
    # Ensure output directory exists
    fs::dir_create(fs::path_dir(output_path), recurse = TRUE)

    readr::write_csv(rents_data, output_path)
    cli::cli_alert_success("Rents data saved to: {output_path}")
  }

  return(rents_data)
}

#' Complete market survey processing workflow
#'
#' @param root_xl_path Root directory containing Excel files
#' @param output_dir Directory to save processed data
#' @param valid_prop_names Vector of valid property names
#' @param use_cache Whether to use cached data if available
#' @return List of processed data frames
#'
#' @export
process_market_surveys <- function(
    root_xl_path,
    output_dir = "data-raw/data/working",
    valid_prop_names = NULL,
    use_cache = TRUE
) {
  # Ensure output directory exists
  fs::dir_create(output_dir, recurse = TRUE)

  # Define cache path
  cache_path <- fs::path(output_dir, "all_survey_data.qs")

  # Load survey data (from cache if available and requested)
  all_survey_data <- load_market_survey_data(
    root_xl_path = root_xl_path,
    output_cache = if (use_cache) cache_path else NULL
  )

  # Process property summaries
  prop_summs <- process_property_summaries(
    all_survey_data = all_survey_data,
    valid_prop_names = valid_prop_names,
    output_path = fs::path(output_dir, "property_summaries.csv")
  )

  # Process rents data
  rents_data <- process_rents_data(
    all_survey_data = all_survey_data,
    output_path = fs::path(output_dir, "rents_by_floorplan.csv")
  )

  # Return all processed data
  return(list(
    property_summaries = prop_summs,
    rents_data = rents_data,
    raw_data = all_survey_data
  ))
}

# specifications --------------------------------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.market_survey_specs <- function() {
  list(
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
}

#' @keywords internal
#' @noRd
.pre_lease_report_parameters_tbl_col_specs <- function() {
  list(
    "report_name" = "text",
    "report_version" = "text",
    "property_names" = "text",
    "period_date" = "date",
    "lease_occupancy_types" = "text",
    "summarize_by" = "text",
    "group_by" = "text",
    "consider_pre_leased_on" = "text",
    "leases_included" = "text",
    "charge_code_details" = "text",
    "space_options" = "text",
    "combine_unit_spaces_with_same_lease" = "logical",
    "consolidate_by" = "text",
    "arrange_by_property" = "logical",
    "yoy" = "logical",
    "generated_date" = "datetime",
    "data_as_of_date" = "datetime"
  )
}

#' @keywords internal
#' @noRd
.pre_lease_property_summary_tbl_col_specs <- function() {
  list(
    "unit_type" = "text",
    "excluded_units" = "numeric",
    "rentable_units" = "numeric",
    "avg_scheduled_charges" = "numeric",
    "current_occupied" = "numeric",
    "prior_total_new" = "numeric",
    "current_total_new" = "numeric",
    "prior_renewal" = "numeric",
    "current_renewal" = "numeric",
    "prior_total_leases" = "numeric",
    "current_total_leases" = "numeric",
    "prior_pre_lease_pct" = "numeric",
    "current_pre_lease_pct" = "numeric",
    "yoy_variance_pct" = "numeric",
    "projected_availability" = "numeric"
  )
}

#' @keywords internal
#' @noRd
.pre_lease_property_details_tbl_col_specs <- function() {
  list(
    "bldg_unit" = "text",
    "unit_type" = "text",
    "unit_status" = "text",
    "resident_name" = "text",
    "lease_status" = "text",
    "lease_term_name" = "text",
    "lease_term_month" = "numeric",
    "lease_start_date" = "date",
    "lease_end_date" = "date",
    "lease_completed_date" = "date",
    "lease_approved_date" = "date",
    "deposit_charged" = "numeric",
    "budgeted_rent" = "numeric",
    "scheduled_charges" = "numeric",
    "posted_charges" = "numeric"
  )
}

#' @keywords internal
#' @noRd
.pre_lease_property_details_tbl_col_specs_w_charge_codes <- function() {

  list(
    "bldg_unit" = "text",
    "unit_type" = "text",
    "unit_status" = "text",
    "resident_name" = "text",
    "lease_status" = "text",
    "lease_term_name" = "text",
    "lease_term_month" = "numeric",
    "lease_start_date" = "date",
    "lease_end_date" = "date",
    "lease_completed_date" = "date",
    "lease_approved_date" = "date",
    "deposit_charged" = "numeric",
    "budgeted_rent" = "numeric",
    "ledger" = "text",
    "charge_code" = "text",
    "scheduled_charges" = "numeric",
    "posted_charges" = "numeric"
  )

}

#' @keywords internal
#' @noRd
.pre_lease_report_parameters_skeleton <- function() {
  tibble::tibble(
    report_name = character(),
    report_version = character(),
    property_names = character(),
    period_date = as.Date(NA),
    lease_occupancy_types = character(),
    summarize_by = character(),
    group_by = character(),
    consider_pre_leased_on = character(),
    leases_included = character(),
    charge_code_details = character(),
    space_options = character(),
    combine_unit_spaces_with_same_lease = logical(),
    consolidate_by = character(),
    arrange_by_property = logical(),
    yoy = logical(),
    x = character(),
    generated_date = as.POSIXct(NA, tz = "EDT"),
    data_as_of_date = as.POSIXct(NA, tz = "EDT")
  )
}

#' @keywords internal
#' @noRd
.pre_lease_global_summary_tbl_col_specs <- function() {
  list(
    "property_name" = "text",
    "total_beds" = "numeric",
    "model_beds" = "numeric",
    "current_occupancy" = "numeric",
    "x" = "skip",
    "current_total_new" = "numeric",
    "current_total_renewals" = "numeric",
    "current_total_leases" = "numeric",
    "current_pre_lease_pct" = "numeric",
    "xx" = "skip",
    "prior_total_new" = "numeric",
    "prior_total_renewals" = "numeric",
    "prior_total_leases" = "numeric",
    "prior_pre_lease_pct" = "numeric",
    "xxx" = "skip",
    "yoy_variance_cnt" = "numeric",
    "yoy_variance_pct" = "numeric",
    "xxxx" = "skip",
    "weekly_new" = "numeric",
    "weekly_renewals" = "numeric",
    "weekly_leases" = "numeric",
    "weekly_pct_gained" = "numeric",
    "xxxxx" = "skip",
    "beds_left_to_lease" = "numeric",
    "weekly_beds_leased" = "numeric",
    "vel_90" = "numeric",
    "vel_95" = "numeric",
    "vel_100" = "numeric"
  )
}
