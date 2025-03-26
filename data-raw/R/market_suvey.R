
#  ------------------------------------------------------------------------
#
# Title : Market Survey Data Prep Functions
#    By : Jimmy Briggs
#  Date : 2025-03-18
#
#  ------------------------------------------------------------------------


# process survey excel files --------------------------------------------------------------------------------------


#' Process multiple Excel market survey files
#'
#' @param root_xl_path Directory containing Excel files
#' @param ignore_sheets Vector of sheet names to ignore
#' @param output_dir Directory to save processed data
#' @param use_cache Whether to use cached data if available
#' @return List of processed data frames
#'
#' @export
process_market_surveys <- function(
    root_xl_path,
    ignore_sheets = NULL,
    output_dir = "data/processed",
    use_cache = TRUE
) {
  # Ensure output directory exists
  fs::dir_create(output_dir, recursive = TRUE)

  # Set cache path
  cache_path <- fs::path(output_dir, "all_survey_data.qs")

  # Check if using cached data
  if (use_cache && fs::file_exists(cache_path)) {
    cli::cli_alert_info("Using cached survey data from: {cache_path}")
    return(qs2::qs_read(cache_path))
  }

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

  cli::cli_h1("Processing Market Survey Data")

  # Find commonwealth file
  commonwealth_xl_file <- fs::path(root_xl_path, "UNLOCKED_1047_Commonwealth_Market_Survey_Updated.xlsm")

  # Extract zip file if needed
  extracted_dir <- fs::path(root_xl_path, "gmh_market_survey_data_extracted")
  if (!fs::dir_exists(extracted_dir)) {
    zip_file <- fs::path(root_xl_path, "GMH Associates  Inc.-selected-assets.zip")
    if (!fs::file_exists(zip_file)) {
      cli::cli_abort("Zip file not found at: {zip_file}")
    }
    cli::cli_alert_info("Extracting zip file to: {extracted_dir}")
    unzip(zip_file, exdir = extracted_dir)
  }

  # Get list of Excel files
  xl_files_path <- extracted_dir
  xl_files <- c(
    commonwealth_xl_file,
    fs::dir_ls(xl_files_path, type = "file", regexp = "~\\$", invert = TRUE)
  )

  # Get sheets for each file
  xl_file_sheets <- purrr::map(xl_files, readxl::excel_sheets) |>
    stats::setNames(basename(xl_files)) |>
    purrr::map(~ setdiff(.x, ignore_sheets))

  # Create a mapping of property names and IDs
  # This is a placeholder - in production, you'd get this from your database
  property_mapping <- data.frame(
    property_name = c("1047 Commonwealth Avenue"),
    property_id = c("739085"),
    stringsAsFactors = FALSE
  )

  # Process each file
  cli::cli_alert_info("Processing {length(xl_files)} Excel files...")

  all_data <- list()

  for (file_idx in seq_along(xl_files)) {
    file_path <- xl_files[file_idx]
    file_basename <- basename(file_path)
    cli::cli_alert_info("Processing file {file_idx}/{length(xl_files)}: {file_basename}")

    # Skip if no sheets for this file
    if (!file_basename %in% names(xl_file_sheets)) {
      cli::cli_alert_warning("No sheets found for file: {file_basename}")
      next
    }

    # Get sheets for this file
    sheets <- xl_file_sheets[[file_basename]]

    # Process each sheet
    for (sheet_idx in seq_along(sheets)) {
      sheet_name <- sheets[sheet_idx]
      cli::cli_alert_info("  Processing sheet {sheet_idx}/{length(sheets)}: {sheet_name}")

      # Determine if this is a property or competitor
      # For 1047 Commonwealth sheet, this is the property
      # Other sheets are typically competitors
      is_property <- sheet_name == "1047 Commonwealth"
      property_id <- if (is_property) "739085" else NA_character_

      # If this is the Commonwealth file and the sheet is not 1047 Commonwealth,
      # then it's a competitor to 1047 Commonwealth
      property_for_competitor <- if (!is_property && file_basename == basename(commonwealth_xl_file)) {
        "1047 Commonwealth Avenue"
      } else {
        # For other files, we may need a more complex lookup
        # This is a simplification - in production, you'd have a mapping
        NA_character_
      }

      # Process the sheet
      tryCatch({
        sheet_data <- read_raw_market_survey_data(
          wb_path = file_path,
          sheet = sheet_name,
          property_id = property_id,
          property_name = if (is_property) "1047 Commonwealth Avenue" else sheet_name,
          leasing_week = "6" # Using a fixed leasing week for demo
        )

        # Store the data
        if (is.null(all_data[[file_basename]])) {
          all_data[[file_basename]] <- list()
        }
        all_data[[file_basename]][[sheet_name]] <- sheet_data

      }, error = function(e) {
        cli::cli_alert_danger("Error processing sheet {sheet_name}: {e$message}")
      })
    }
  }

  # Cache the results
  qs2::qs_save(all_data, file = cache_path)
  cli::cli_alert_success("All survey data processed and cached at: {cache_path}")

  # Extract and process different section types
  process_section_data(all_data, output_dir)

  return(all_data)
}


# process section data --------------------------------------------------------------------------------------------

#' Process various types of section data from the market survey
#'
#' @param all_data The raw survey data from all workbooks
#' @param output_dir Directory to save processed data
#' @return List of processed dataframes
#'
#' @export
process_section_data <- function(all_data, output_dir) {
  # Process property summaries
  property_summaries <- extract_property_summaries(all_data)
  readr::write_csv(property_summaries, fs::path(output_dir, "property_summaries.csv"))
  cli::cli_alert_success("Property summaries saved to: {fs::path(output_dir, 'property_summaries.csv')}")

  # Process leasing summaries
  leasing_summaries <- extract_leasing_summaries(all_data)
  readr::write_csv(leasing_summaries, fs::path(output_dir, "leasing_summaries.csv"))
  cli::cli_alert_success("Leasing summaries saved to: {fs::path(output_dir, 'leasing_summaries.csv')}")

  # Process short-term leases
  short_term_leases <- extract_short_term_leases(all_data)
  readr::write_csv(short_term_leases, fs::path(output_dir, "short_term_leases.csv"))
  cli::cli_alert_success("Short-term leases saved to: {fs::path(output_dir, 'short_term_leases.csv')}")

  # Process fees
  fees <- extract_fees(all_data)
  readr::write_csv(fees, fs::path(output_dir, "fees.csv"))
  cli::cli_alert_success("Fees saved to: {fs::path(output_dir, 'fees.csv')}")

  # Process property amenities
  property_amenities <- extract_property_amenities(all_data)
  readr::write_csv(property_amenities, fs::path(output_dir, "property_amenities.csv"))
  cli::cli_alert_success("Property amenities saved to: {fs::path(output_dir, 'property_amenities.csv')}")

  # Process unit amenities
  unit_amenities <- extract_unit_amenities(all_data)
  readr::write_csv(unit_amenities, fs::path(output_dir, "unit_amenities.csv"))
  cli::cli_alert_success("Unit amenities saved to: {fs::path(output_dir, 'unit_amenities.csv')}")

  # Process rents by floorplan
  rents_by_floorplan <- extract_rents_by_floorplan(all_data)
  readr::write_csv(rents_by_floorplan, fs::path(output_dir, "rents_by_floorplan.csv"))
  cli::cli_alert_success("Rents by floorplan saved to: {fs::path(output_dir, 'rents_by_floorplan.csv')}")

  return(list(
    property_summaries = property_summaries,
    leasing_summaries = leasing_summaries,
    short_term_leases = short_term_leases,
    fees = fees,
    property_amenities = property_amenities,
    unit_amenities = unit_amenities,
    rents_by_floorplan = rents_by_floorplan
  ))
}

#' Read raw market survey data from a workbook and sheet
#'
#' @param wb_path Path to Excel workbook
#' @param sheet Sheet name to read
#' @param property_id Property ID (if available)
#' @param property_name Property name
#' @param leasing_week Leasing week ID or period
#' @return List of data frames with extracted data
#'
#' @export
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

  # Special case for Rise at Northgate (and possibly others)
  if (stringr::str_detect(basename(wb_path), "Rise at Northgate") ||
      stringr::str_detect(sheet, "Rise at Northgate")) {
    args$property_summary$range <- "A3:B18"
  }

  # Read each section with error handling
  market_survey_data_lst <- purrr::map(
    names(args),
    function(section_name) {
      section_spec <- args[[section_name]]

      result <- tryCatch({
        readxl::read_excel(
          wb_path,
          sheet = sheet,
          range = section_spec$range,
          col_names = section_spec$col_names,
          na = c("", "N/A", "n/a", "NA", "-", NA)
        ) |>
          # Add property info
          dplyr::mutate(
            property_id = property_id,
            property_name = property_name,
            leasing_week = leasing_week
          ) |>
          # Remove x, xx, xxx cols (only if they exist)
          dplyr::select(-dplyr::matches("^x+$"))
      }, error = function(e) {
        cli::cli_alert_warning("Error reading {section_name} from {sheet}: {e$message}")
        return(NULL)
      })

      return(result)
    }
  ) |>
    stats::setNames(names(args)) |>
    # Filter out NULL results
    purrr::compact()

  # property detail ---------------------------------------------------------
  property_detail <- if (!is.null(market_survey_data_lst$property_detail)) {
    market_survey_data_lst$property_detail |>
      dplyr::pull(property_detail) |>
      stringr::str_split(": ") |>
      purrr::pluck(1)
  } else {
    NULL
  }

  # property summary --------------------------------------------------------
  property_summary <- if (!is.null(market_survey_data_lst$property_summary)) {
    market_survey_data_lst$property_summary |>
      dplyr::filter(!is.na(key))
  } else {
    NULL
  }

  property_summary_wide <- if (!is.null(property_summary)) {
    property_summary |>
      tidyr::pivot_wider(
        names_from = key,
        values_from = value
      )
  } else {
    NULL
  }

  property_summary_lst <- list(
    property_summary = property_summary,
    property_summary_wide = property_summary_wide
  )

  # leasing summary ---------------------------------------------------------
  leasing_summary <- if (!is.null(market_survey_data_lst$leasing_summary)) {
    market_survey_data_lst$leasing_summary |>
      dplyr::filter(!is.na(key))
  } else {
    NULL
  }

  leasing_summary_wide <- if (!is.null(leasing_summary)) {
    leasing_summary |>
      tidyr::pivot_wider(
        names_from = key,
        values_from = value
      )
  } else {
    NULL
  }

  leasing_summary_lst <- list(
    leasing_summary = leasing_summary,
    leasing_summary_wide = leasing_summary_wide
  )

  # short term leases -------------------------------------------------------
  short_term_leases <- if (!is.null(market_survey_data_lst$short_term_leases)) {
    market_survey_data_lst$short_term_leases |>
      dplyr::filter(!is.na(key))
  } else {
    NULL
  }

  short_term_leases_wide <- if (!is.null(short_term_leases)) {
    short_term_leases |>
      tidyr::pivot_wider(
        names_from = key,
        values_from = value
      )
  } else {
    NULL
  }

  short_term_leases_lst <- list(
    short_term_leases = short_term_leases,
    short_term_leases_wide = short_term_leases_wide
  )

  # fees --------------------------------------------------------------------
  fees_lst <- if (!is.null(market_survey_data_lst$fees)) {
    fees_data <- market_survey_data_lst$fees

    # Extract fee structure if present
    fees_fee_structure <- fees_data |>
      dplyr::filter(
        fees == "Fee Structure"
      ) |>
      dplyr::pull(amount)

    # Find text amounts
    fees_text_amounts <- fees_data |>
      # Find values in amount column that are not parseable to numeric
      dplyr::filter(
        !grepl("^\\d+\\.?\\d*$", amount)
      )

    # Regular fees
    fees <- fees_data |>
      dplyr::filter(
        !(fees %in% fees_text_amounts$fees)
      ) |>
      dplyr::mutate(
        amount = suppressWarnings(as.numeric(amount))
      )

    # Pivot to wide format
    fees_wide <- fees |>
      tidyr::pivot_wider(
        names_from = fees,
        values_from = amount
      )

    list(
      fee_structure = if(length(fees_fee_structure) > 0) fees_fee_structure else NULL,
      fees_text_amounts = fees_text_amounts,
      fees = fees,
      fees_wide = fees_wide
    )
  } else {
    list(
      fee_structure = NULL,
      fees_text_amounts = NULL,
      fees = NULL,
      fees_wide = NULL
    )
  }

  # property amenities ------------------------------------------------------
  amenities_lst <- if (!is.null(market_survey_data_lst$property_amenitites) &&
                       !is.null(market_survey_data_lst$unit_amenities)) {

    # Property amenities
    property_amenitites_data <- market_survey_data_lst$property_amenitites

    property_amenities_common_area_rating <- property_amenitites_data |>
      dplyr::filter(
        key == "Common Area Rating"
      ) |>
      dplyr::pull(value)

    property_amenitites <- property_amenitites_data |>
      dplyr::filter(
        !is.na(key),
        key != "Common Area Rating"
      )

    property_amenitites_wide <- property_amenitites |>
      tidyr::pivot_wider(
        names_from = key,
        values_from = value
      )

    # Unit amenities
    unit_amenities_data <- market_survey_data_lst$unit_amenities

    unit_amenities_unit_rating <- unit_amenities_data |>
      dplyr::filter(
        key == "Unit Rating"
      ) |>
      dplyr::pull(value)

    unit_amenities <- unit_amenities_data |>
      dplyr::filter(
        !is.na(key),
        key != "Unit Rating"
      )

    # Fix specific keys based on your original code
    # This mapping might need to be updated based on your data
    unit_amenities_key_mapping <- c(
      "TV Included in Rent" = 13,
      "TV Rentable Rate" = 14,
      "TV Bedroom" = 15,
      "TV Common Area" = 16,
      "Furniture Included in Rent" = 18,
      "Furniture Rentable Rate" = 19,
      "Floor Premium" = 21,
      "Poolside Premium" = 22,
      "Top Floor Premium" = 23,
      "View Premium" = 24,
      "Other Premiums" = 25
    )

    # Apply key mapping where appropriate
    for (i in names(unit_amenities_key_mapping)) {
      row_idx <- unit_amenities_key_mapping[i]
      if (row_idx <= nrow(unit_amenities)) {
        unit_amenities[row_idx, "key"] <- i
      }
    }

    # Filter out specific keys that should be removed
    unit_amenities <- unit_amenities |>
      dplyr::filter(
        !(key %in% c("TV", "Furniture", "Other Premiums"))
      )

    unit_amenities_wide <- unit_amenities |>
      tidyr::pivot_wider(
        names_from = key,
        values_from = value
      )

    list(
      property_amenities_common_area_rating = if(length(property_amenities_common_area_rating) > 0) property_amenities_common_area_rating else NULL,
      property_amenitites = property_amenitites,
      property_amenitites_wide = property_amenitites_wide,
      unit_amenities_unit_rating = if(length(unit_amenities_unit_rating) > 0) unit_amenities_unit_rating else NULL,
      unit_amenities = unit_amenities,
      unit_amenities_wide = unit_amenities_wide
    )
  } else {
    list(
      property_amenities_common_area_rating = NULL,
      property_amenitites = NULL,
      property_amenitites_wide = NULL,
      unit_amenities_unit_rating = NULL,
      unit_amenities = NULL,
      unit_amenities_wide = NULL
    )
  }

  # parking -----------------------------------------------------------------
  parking_lst <- if (!is.null(market_survey_data_lst$parking)) {
    parking_data <- market_survey_data_lst$parking |>
      dplyr::filter(
        !is.na(parking_type)
      ) |>
      dplyr::mutate(
        included = dplyr::if_else(included == "Yes", TRUE, FALSE),
        required = dplyr::if_else(required == "Yes", TRUE, FALSE),
        amount = suppressWarnings(as.numeric(amount))
      ) |>
      dplyr::select(
        property_id,
        property_name,
        parking_type,
        required,
        included,
        amount
      )

    list(parking = parking_data)
  } else {
    list(parking = NULL)
  }

  # utilities ---------------------------------------------------------------
  utilities_lst <- if (!is.null(market_survey_data_lst$utilities_summary_1) ||
                       !is.null(market_survey_data_lst$utilities_summary_2)) {

    utilities_summary_1 <- market_survey_data_lst$utilities_summary_1

    utilities_summary_2 <- if (!is.null(market_survey_data_lst$utilities_summary_2)) {
      market_survey_data_lst$utilities_summary_2 |>
        dplyr::filter(
          !is.na(utility)
        )
    } else {
      NULL
    }

    list(
      utilities_summary_1 = utilities_summary_1,
      utilities_summary_2 = utilities_summary_2
    )
  } else {
    list(
      utilities_summary_1 = NULL,
      utilities_summary_2 = NULL
    )
  }

  # notes -------------------------------------------------------------------
  notes_lst <- if (!is.null(market_survey_data_lst$notes)) {
    notes_data <- market_survey_data_lst$notes

    notes_leasing_special <- if (nrow(notes_data) >= 2) notes_data[2, ] else NULL
    notes_property_notes <- if (nrow(notes_data) >= 12) notes_data[12, ] else NULL

    list(
      notes_leasing_special = notes_leasing_special,
      notes_property_notes = notes_property_notes
    )
  } else {
    list(
      notes_leasing_special = NULL,
      notes_property_notes = NULL
    )
  }

  # office hours -----------------------------------------------------------
  office_hours_lst <- if (!is.null(market_survey_data_lst$office_hours)) {
    office_hours <- market_survey_data_lst$office_hours |>
      dplyr::filter(
        !is.na(day)
      )

    list(office_hours = office_hours)
  } else {
    list(office_hours = NULL)
  }

  # rents -------------------------------------------------------------------
  rents_lst <- if (!is.null(market_survey_data_lst$rents_by_floorplan) ||
                   !is.null(market_survey_data_lst$average_rents_by_unit_type)) {

    rents_by_floorplan <- if (!is.null(market_survey_data_lst$rents_by_floorplan)) {
      market_survey_data_lst$rents_by_floorplan |>
        dplyr::filter(
          !is.na(floorplan_type)
        )
    } else {
      NULL
    }

    average_rents_by_unit_type <- if (!is.null(market_survey_data_lst$average_rents_by_unit_type)) {
      market_survey_data_lst$average_rents_by_unit_type |>
        dplyr::filter(
          !is.na(unit_type)
        )
    } else {
      NULL
    }

    list(
      rents_by_floorplan = rents_by_floorplan,
      average_rents_by_unit_type = average_rents_by_unit_type
    )
  } else {
    list(
      rents_by_floorplan = NULL,
      average_rents_by_unit_type = NULL
    )
  }

  # Return complete data structure ------------------------------------------
  list(
    property_detail = property_detail,
    property_summary = property_summary_lst,
    leasing_summary = leasing_summary_lst,
    short_term_leases = short_term_leases_lst,
    fees = fees_lst,
    amenities = amenities_lst,
    utilities = utilities_lst,
    parking = parking_lst,
    notes = notes_lst,
    office_hours = office_hours_lst,
    rents = rents_lst
  )
}

# extraction ------------------------------------------------------------------------------------------------------


#' Extract property summaries from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of property summaries
#'
#' @export
extract_property_summaries <- function(all_data) {
  cli::cli_h2("Extracting property summaries")

  property_summaries <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$property_summary$property_summary_wide)) {
        return(NULL)
      }

      # Get property data
      sheet_data$property_summary$property_summary_wide |>
        dplyr::mutate(
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean property summaries
  property_summaries |>
    # Standardize column names
    janitor::clean_names() |>
    # Handle date columns
    dplyr::mutate(
      most_recent_sale = as.character(most_recent_sale),
      most_recent_sale_date = dplyr::case_when(
        is.na(most_recent_sale) ~ as.Date(NA),
        most_recent_sale %in% c("n/a", "N/A", "-", "NA") ~ as.Date(NA),
        nchar(trimws(most_recent_sale)) == 4 ~ as.Date(paste0(most_recent_sale, "-01-01")),
        grepl("^\\d{5}$", most_recent_sale) ~ {
          suppressWarnings(as.Date(as.numeric(most_recent_sale), origin = "1899-12-30"))
        },
        TRUE ~ as.Date(NA)
      ),
      # Ensure other fields are properly typed
      property_rating = suppressWarnings(as.numeric(property_rating)),
      year_built_renovated = suppressWarnings(as.integer(year_built_renovated))
    ) |>
    # Remove duplicate property summaries
    dplyr::distinct(property_name, .keep_all = TRUE)
}

#' Extract leasing summaries from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of leasing summaries
#'
#' @export
extract_leasing_summaries <- function(all_data) {
  cli::cli_h2("Extracting leasing summaries")

  leasing_summaries <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$leasing_summary$leasing_summary_wide)) {
        return(NULL)
      }

      # Get leasing data
      sheet_data$leasing_summary$leasing_summary_wide |>
        dplyr::mutate(
          property_name = sheet_data$property_summary$property_summary_wide$property_name,
          property_id = sheet_data$property_summary$property_summary_wide$property_id,
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean leasing summaries
  leasing_summaries |>
    # Standardize column names
    janitor::clean_names() |>
    # Handle date columns
    dplyr::mutate(
      lease_launch_date = as.Date(lease_launch_date),
      renewal_launch_date = as.Date(renewal_launch_date),
      # Ensure numeric fields are properly typed
      current_occupancy = suppressWarnings(as.numeric(current_occupancy)),
      prior_year_occupancy = suppressWarnings(as.numeric(prior_year_occupancy)),
      current_pre_lease = suppressWarnings(as.numeric(current_pre_lease)),
      prior_year_pre_lease = suppressWarnings(as.numeric(prior_year_pre_lease)),
      total_renewals = suppressWarnings(as.integer(total_renewals)),
      total_new_leases = suppressWarnings(as.integer(total_new_leases)),
      weekly_leases = suppressWarnings(as.integer(weekly_leases)),
      weekly_traffic = suppressWarnings(as.integer(weekly_traffic))
    ) |>
    # Remove duplicates
    dplyr::distinct(property_name, .keep_all = TRUE)
}

#' Extract short-term leases from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of short-term leases
#'
#' @export
extract_short_term_leases <- function(all_data) {
  cli::cli_h2("Extracting short-term leases")

  short_term_leases <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$short_term_leases$short_term_leases_wide)) {
        return(NULL)
      }

      # Get short-term lease data
      sheet_data$short_term_leases$short_term_leases |>
        dplyr::mutate(
          property_name = sheet_data$property_summary$property_summary_wide$property_name,
          property_id = sheet_data$property_summary$property_summary_wide$property_id,
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean short-term leases
  short_term_leases |>
    # Standardize column names
    janitor::clean_names() |>
    # Ensure correct data types
    dplyr::mutate(
      term_months = as.integer(as.numeric(value)),
      is_available = stringr::str_detect(tolower(key), "available|offered"),
      premium = 0.00,  # Default premium
      quantity = 0     # Default quantity
    ) |>
    dplyr::select(property_name, property_id, key, term_months, is_available, premium, quantity, file_name, sheet_name) |>
    dplyr::filter(!is.na(term_months))
}

#' Extract fees from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of fees
#'
#' @export
extract_fees <- function(all_data) {
  cli::cli_h2("Extracting fees")

  fees <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$fees$fees)) {
        return(NULL)
      }

      # Get fee data
      sheet_data$fees$fees |>
        dplyr::mutate(
          property_name = sheet_data$property_summary$property_summary_wide$property_name,
          property_id = sheet_data$property_summary$property_summary_wide$property_id,
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean fees
  fees |>
    # Standardize column names
    janitor::clean_names() |>
    # Ensure numeric amounts
    dplyr::mutate(
      fee_amount = suppressWarnings(as.numeric(amount)),
      fee_name = fees,
      fee_frequency = dplyr::if_else(is.na(frequency), "One Time", frequency)
    ) |>
    dplyr::select(property_name, property_id, fee_name, fee_amount, fee_frequency, file_name, sheet_name) |>
    dplyr::filter(!is.na(fee_name), !is.na(fee_amount))
}

#' Extract property amenities from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of property amenities
#'
#' @export
extract_property_amenities <- function(all_data) {
  cli::cli_h2("Extracting property amenities")

  property_amenities <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$amenities$property_amenitites)) {
        return(NULL)
      }

      # Get property amenities data
      sheet_data$amenities$property_amenitites |>
        dplyr::mutate(
          property_name = sheet_data$property_summary$property_summary_wide$property_name,
          property_id = sheet_data$property_summary$property_summary_wide$property_id,
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean property amenities
  property_amenities |>
    # Standardize column names
    janitor::clean_names() |>
    # Convert values to boolean
    dplyr::mutate(
      amenity_value = dplyr::case_when(
        tolower(value) %in% c("yes", "y", "true", "t", "1") ~ TRUE,
        tolower(value) %in% c("no", "n", "false", "f", "0") ~ FALSE,
        TRUE ~ NA
      ),
      amenity_name = key
    ) |>
    dplyr::select(property_name, property_id, amenity_name, amenity_value, file_name, sheet_name) |>
    dplyr::filter(!is.na(amenity_name))
}

#' Extract unit amenities from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of unit amenities
#'
#' @export
extract_unit_amenities <- function(all_data) {
  cli::cli_h2("Extracting unit amenities")

  unit_amenities <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$amenities$unit_amenities)) {
        return(NULL)
      }

      # Get unit amenities data
      sheet_data$amenities$unit_amenities |>
        dplyr::mutate(
          property_name = sheet_data$property_summary$property_summary_wide$property_name,
          property_id = sheet_data$property_summary$property_summary_wide$property_id,
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean unit amenities
  unit_amenities |>
    # Standardize column names
    janitor::clean_names() |>
    # Convert values to boolean
    dplyr::mutate(
      amenity_value = dplyr::case_when(
        tolower(value) %in% c("yes", "y", "true", "t", "1") ~ TRUE,
        tolower(value) %in% c("no", "n", "false", "f", "0") ~ FALSE,
        TRUE ~ NA
      ),
      amenity_name = key
    ) |>
    dplyr::select(property_name, property_id, amenity_name, amenity_value, file_name, sheet_name) |>
    dplyr::filter(!is.na(amenity_name))
}

#' Extract rents by floorplan from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of rents by floorplan
#'
#' @export
extract_rents_by_floorplan <- function(all_data) {
  cli::cli_h2("Extracting rents by floorplan")

  rents_by_floorplan <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$rents$rents_by_floorplan)) {
        return(NULL)
      }

      # Get rents data
      sheet_data$rents$rents_by_floorplan |>
        dplyr::mutate(
          property_name = sheet_data$property_summary$property_summary_wide$property_name,
          property_id = sheet_data$property_summary$property_summary_wide$property_id,
          file_name = file_name,
          sheet_name = sheet_name
        )
    })
  })

  # Clean rents by floorplan
  rents_by_floorplan |>
    # Standardize column names
    janitor::clean_names() |>
    # Ensure numeric fields are properly typed
    dplyr::mutate(
      square_feet = suppressWarnings(as.numeric(sf_per_bed)),
      number_of_beds = suppressWarnings(as.integer(bed)),
      number_of_baths = suppressWarnings(as.integer(bath)),
      total_units_count = suppressWarnings(as.integer(count)),
      available = !is.na(available),
      market_rent_per_bed = suppressWarnings(as.numeric(market_rent_per_bed)),
      # Generate a simple floorplan ID if none exists
      floorplan_id = dplyr::if_else(
        is.na(floorplan_type),
        paste0("FP", dplyr::row_number()),
        paste0(substr(floorplan_type, 1, 1), dplyr::row_number())
      )
    ) |>
    dplyr::select(
      property_name, property_id, floorplan_type, floorplan_id, square_feet,
      number_of_beds, number_of_baths, total_units_count, available,
      market_rent_per_bed, file_name, sheet_name
    ) |>
    dplyr::filter(!is.na(floorplan_type))
}


