library(readxl)
library(purrr)
library(dplyr)
library(fs)
library(stringr)
library(tidyr)

#' Read structured Excel market survey data
#'
#' @param file_path Path to Excel file
#' @param sheet_name Sheet name to read (default: NULL, uses first sheet)
#' @param range_specs Named list of range specifications
#' @param clean_data Boolean indicating whether to clean data (default: TRUE)
#' @return Named list of data frames with extracted data
#'
#' @export
read_market_survey <- function(file_path, sheet_name = NULL, range_specs, clean_data = TRUE) {
  # Validate input file exists
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  # If sheet_name is NULL, use the first sheet
  if (is.null(sheet_name)) {
    sheets <- excel_sheets(file_path)
    if (length(sheets) == 0) {
      stop("No sheets found in workbook")
    }
    sheet_name <- sheets[1]
  }

  # Read each range according to specs
  result <- map(names(range_specs), function(section_name) {
    spec <- range_specs[[section_name]]

    tryCatch({
      # Read the data from the specified range
      data <- read_excel(
        path = file_path,
        sheet = sheet_name,
        range = spec$range,
        col_names = spec$col_names,
        .name_repair = "minimal"
      )

      # Clean data if requested
      if (clean_data) {
        data <- clean_market_survey_data(data, section_name)
      }

      return(data)
    }, error = function(e) {
      warning(paste("Error reading section", section_name, ":", e$message))
      return(NULL)
    })
  }) %>% setNames(names(range_specs))

  # Filter out NULL results (failed reads)
  result <- result[!sapply(result, is.null)]

  # Add metadata
  attr(result, "file_path") <- file_path
  attr(result, "sheet_name") <- sheet_name
  attr(result, "read_timestamp") <- Sys.time()

  return(result)
}

#' Process a directory of Excel files with consistent structure
#'
#' @param dir_path Directory containing Excel files
#' @param pattern File pattern to match (default: "\\.xlsx$")
#' @param range_specs Named list of range specifications
#' @param sheet_selector Function to select sheet from each workbook (default: first sheet)
#' @param recursive Whether to search recursively (default: FALSE)
#' @return Named list of survey data by file
#'
#' @export
process_market_survey_directory <- function(
    dir_path,
    pattern = "\\.xlsx$",
    range_specs,
    sheet_selector = function(sheets) sheets[1],
    recursive = FALSE
) {
  # Find all matching files
  files <- dir_ls(path = dir_path, regexp = pattern, recurse = recursive)

  if (length(files) == 0) {
    warning("No matching files found")
    return(list())
  }

  # Process each file
  result <- map(files, function(file_path) {
    # Get sheets and select the appropriate one
    sheets <- excel_sheets(file_path)
    sheet_name <- sheet_selector(sheets)

    # Read the data
    survey_data <- read_market_survey(
      file_path = file_path,
      sheet_name = sheet_name,
      range_specs = range_specs
    )

    # Add file metadata
    attr(survey_data, "file_name") <- basename(file_path)

    return(survey_data)
  }) %>% setNames(basename(files))

  return(result)
}

#' Clean market survey data based on section type
#'
#' @param data Data frame to clean
#' @param section_name Name of the section
#' @return Cleaned data frame
#'
#' @export
clean_market_survey_data <- function(data, section_name) {
  # Skip cleaning if data is empty
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(data)
  }

  # General cleaning for all sections
  data <- data %>%
    # Remove completely empty rows
    filter(rowSums(is.na(.)) != ncol(.)) %>%
    # Handle section-specific cleaning
    (function(df) {
      # Key-value pair handling for appropriate sections
      if (section_name %in% c("property_summary", "leasing_summary", "short_term_leases")) {
        df <- df %>%
          filter(!is.na(key)) %>%
          mutate(key = str_trim(key)) %>%
          # Convert keys to snake_case
          mutate(key = str_to_lower(key)) %>%
          mutate(key = str_replace_all(key, "\\s+", "_")) %>%
          mutate(key = str_replace_all(key, "[^a-z0-9_]", ""))
      }

      # Special handling for rents sections
      if (grepl("rents", section_name)) {
        # Handle numeric columns
        numeric_cols <- grep("(rent|expenses|sf|count|available)", names(df), value = TRUE)
        df <- df %>%
          mutate(across(all_of(numeric_cols), ~as.numeric(.)))
      }

      # Handle notes section
      if (section_name == "notes") {
        df <- df %>%
          filter(!is.na(notes)) %>%
          mutate(notes = str_trim(notes))
      }

      return(df)
    })

  return(data)
}

#' Extract a normalized key-value data frame from survey data
#'
#' @param survey_data List of data frames from read_market_survey
#' @param sections Vector of section names to include (default: all)
#' @return Tibble with normalized key-value pairs
#'
#' @export
extract_property_attributes <- function(survey_data, sections = NULL) {
  # If sections not specified, use all key-value pair sections
  if (is.null(sections)) {
    sections <- c("property_summary", "leasing_summary", "short_term_leases")
  }

  # Filter to only include requested sections
  sections <- intersect(sections, names(survey_data))

  if (length(sections) == 0) {
    return(tibble(section = character(), key = character(), value = character()))
  }

  # Combine and normalize data
  result <- map_dfr(sections, function(section_name) {
    if (!section_name %in% names(survey_data)) {
      return(NULL)
    }

    df <- survey_data[[section_name]]

    # Skip if no data or missing expected columns
    if (is.null(df) || nrow(df) == 0 || !all(c("key", "value") %in% names(df))) {
      return(NULL)
    }

    df %>%
      select(key, value) %>%
      filter(!is.na(key)) %>%
      mutate(section = section_name) %>%
      select(section, key, value)
  })

  return(result)
}

#' Example usage demonstrating how to use the functions
#'
example_usage <- function() {
  # Define your range specs (as shown in your question)
  mkt_survey_col_specs <- list(
    property_summary = list(
      range = "A4:B19",
      col_names = c("key", "value")
    ),
    # ... other specs as defined in your question
  )

  # Read a single file
  single_survey <- read_market_survey(
    file_path = "path/to/survey.xlsx",
    range_specs = mkt_survey_col_specs
  )

  # Process a directory of surveys
  all_surveys <- process_market_survey_directory(
    dir_path = "path/to/survey/directory",
    range_specs = mkt_survey_col_specs,
    # Custom sheet selector function
    sheet_selector = function(sheets) {
      # Select sheet with "Market Survey" in the name, or default to first sheet
      survey_sheet <- grep("Market Survey", sheets, value = TRUE)
      if (length(survey_sheet) > 0) survey_sheet[1] else sheets[1]
    }
  )

  # Combine key-value data from all surveys
  all_properties <- map_dfr(names(all_surveys), function(file_name) {
    extract_property_attributes(all_surveys[[file_name]]) %>%
      mutate(file_name = file_name)
  })

  # Return results for inspection
  return(list(
    single_survey = single_survey,
    property_summary = all_properties
  ))
}
