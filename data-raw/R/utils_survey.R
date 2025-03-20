#  ------------------------------------------------------------------------
#
# Title : Survey Utility Functions
# By    : Jimmy Briggs
# Date  : 2025-03-18
#
#  ------------------------------------------------------------------------

#' Process survey workbooks
#'
#' @param xl_files Vector of Excel file paths
#' @param xl_file_sheets Named list of sheets to process for each file
#' @return List of processed survey data
#' @export
process_survey_workbooks <- function(xl_files, xl_file_sheets) {
  all_survey_data <- list()

  # Create property mapping
  property_mapping <- data.frame(
    property_name = c("1047 Commonwealth Avenue"),
    property_id = c("739085"),
    stringsAsFactors = FALSE
  )

  # Main processing loop
  total_processed <- 0
  total_sheets <- sum(purrr::map_int(xl_file_sheets, length))

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

    # Initialize file data
    file_data <- list()

    # Process each sheet
    for (sheet_idx in seq_along(sheets)) {
      sheet_name <- sheets[sheet_idx]
      total_processed <- total_processed + 1
      cli::cli_alert_info("  Processing {total_processed}/{total_sheets}: {file_basename} - {sheet_name}")

      # Determine if this is a property or competitor
      is_property <- sheet_name == "1047 Commonwealth"
      property_id <- if (is_property) "739085" else NA_character_

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
        file_data[[sheet_name]] <- sheet_data

      }, error = function(e) {
        cli::cli_alert_danger("Error processing sheet {sheet_name}: {e$message}")
      })
    }

    # Add file data to result if not empty
    if (length(file_data) > 0) {
      all_survey_data[[file_basename]] <- file_data
    }
  }

  return(all_survey_data)
}

#' Extract property summaries from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of property summaries
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

#' Extract and save section data from survey
#'
#' @param all_data The raw survey data from all workbooks
#' @param output_dir Directory to save data
#' @return Invisibly returns NULL
#' @export
extract_and_save_section_data <- function(all_data, output_dir) {
  # Extract leasing summaries
  leasing_summaries <- extract_leasing_summaries(all_data)
  readr::write_csv(leasing_summaries, fs::path(output_dir, "leasing_summaries.csv"))
  cli::cli_alert_success("Leasing summaries saved to: {fs::path(output_dir, 'leasing_summaries.csv')}")

  # Extract short-term leases
  short_term_leases <- extract_short_term_leases(all_data)
  readr::write_csv(short_term_leases, fs::path(output_dir, "short_term_leases.csv"))
  cli::cli_alert_success("Short-term leases saved to: {fs::path(output_dir, 'short_term_leases.csv')}")

  # Extract fees
  fees <- extract_fees(all_data)
  readr::write_csv(fees, fs::path(output_dir, "fees.csv"))
  cli::cli_alert_success("Fees saved to: {fs::path(output_dir, 'fees.csv')}")

  # Extract property amenities
  property_amenities <- extract_property_amenities(all_data)
  readr::write_csv(property_amenities, fs::path(output_dir, "property_amenities.csv"))
  cli::cli_alert_success("Property amenities saved to: {fs::path(output_dir, 'property_amenities.csv')}")

  # Extract unit amenities
  unit_amenities <- extract_unit_amenities(all_data)
  readr::write_csv(unit_amenities, fs::path(output_dir, "unit_amenities.csv"))
  cli::cli_alert_success("Unit amenities saved to: {fs::path(output_dir, 'unit_amenities.csv')}")

  # Extract rents by floorplan
  rents_by_floorplan <- extract_rents_by_floorplan(all_data)
  readr::write_csv(rents_by_floorplan, fs::path(output_dir, "rents_by_floorplan.csv"))
  cli::cli_alert_success("Rents by floorplan saved to: {fs::path(output_dir, 'rents_by_floorplan.csv')}")

  # Return invisibly
  invisible(NULL)
}

#' Extract leasing summaries from all survey data
#'
#' @param all_data The raw survey data from all workbooks
#' @return Data frame of leasing summaries
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
#' @export
extract_short_term_leases <- function(all_data) {
  cli::cli_h2("Extracting short-term leases")

  short_term_leases <- purrr::map_dfr(names(all_data), function(file_name) {
    purrr::map_dfr(names(all_data[[file_name]]), function(sheet_name) {
      sheet_data <- all_data[[file_name]][[sheet_name]]

      if (is.null(sheet_data$short_term_leases$short_term_leases)) {
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
