db_update_survey_unit_amenities_rates_premiums <- function(pool, new_values) {
  check_db_conn(pool)

  data <- new_values |>
    dplyr::mutate(
      amenity_name = stringr::str_replace(
        snakecase::to_title_case(.data$amenity_name), "Tv", "TV"
      ),
      property_id = purrr::map_int(.data$property_name, get_property_id_by_name),
      amenity_id = purrr::map_int(.data$amenity_name, get_amenity_id_by_name),
      rentable_rate = dplyr::coalesce(.data$rentable_rate, 0),
      premium = dplyr::coalesce(.data$premium, 0)
    )

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.unit_amenities_rates_premiums"),
        records = data,
        where_cols = c("property_name", "amenity_id"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated unit amenities."
      )

      shiny::showNotification(
        "Successfully updated unit amenities.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update unit amenities: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update unit amenities.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(data))
}

db_update_survey_unit_amenities <- function(pool, new_values) {
  check_db_conn(pool)

  data <- new_values |>
    dplyr::mutate(
      property_id = purrr::map_int(.data$property_name, get_property_id_by_name),
      amenity_id = purrr::map_int(.data$amenity_name, get_amenity_id_by_name)
    )

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.unit_amenities"),
        records = data,
        where_cols = c("property_name", "amenity_id"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated unit amenities."
      )

      shiny::showNotification(
        "Successfully updated unit amenities.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update unit amenities: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update unit amenities.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(data))
}

db_update_survey_utilities <- function(pool, new_values) {
  check_db_conn(pool)

  data <- new_values

  if (!all(c("property_name", "utility_name") %in% colnames(data))) {
    # get id (property id for property and competitor id for competitor)
    comp_names <- db_read_tbl(pool, "survey.competitors", collect = FALSE) |>
      dplyr::pull(competitor_name)
    prop_names <- db_read_tbl(pool, "survey.properties", collect = FALSE) |>
      dplyr::pull(property_name)

    if (data$property_name %in% prop_names) {
      data <- data |>
        dplyr::mutate(
          property_id = purrr::map_int(.data$property_name, get_property_id_by_name),
          competitor_id = NA_integer_
        )
    } else if (data$property_name %in% comp_names) {
      data <- data |>
        dplyr::mutate(
          property_id = NA_integer_,
          competitor_id = purrr::map_int(.data$property_name, get_competitor_id_by_name)
        )
    }
  }

  if (!all(c("updated_by") %in% colnames(data))) {
    user_id <- get_user_id_by_email(pool, "jimmy.briggs@noclocks.dev")
    data <- data |>
      dplyr::mutate(
        updated_by = user_id
      )
  }

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.utilities"),
        records = data,
        where_cols = c("property_name", "utility_name"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated utilities"
      )

      shiny::showNotification(
        "Successfully updated utilities.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update utilities: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update utilities.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(data))
}

db_update_survey_property_summary <- function(
    pool,
    new_values,
    property_id = NULL,
    competitor_id = NULL,
    user_id = NULL) {
  check_db_conn(pool)

  # get id (property id for property and competitor id for competitor)
  comp_names <- db_read_tbl(pool, "survey.competitors", collect = FALSE) |>
    dplyr::pull(competitor_name)
  prop_names <- db_read_tbl(pool, "survey.properties", collect = FALSE) |>
    dplyr::pull(property_name)

  data <- new_values

  if (data$property_name %in% prop_names) {
    data <- data |>
      dplyr::mutate(
        property_id = purrr::map_int(.data$property_name, get_property_id_by_name),
        competitor_id = NA_integer_
      )
  } else if (data$property_name %in% comp_names) {
    data <- data |>
      dplyr::mutate(
        property_id = NA_integer_,
        competitor_id = purrr::map_int(.data$property_name, get_competitor_id_by_name)
      )
  }

  if (is.null(user_id)) {
    user_id <- get_user_id_by_email(pool, "jimmy.briggs@noclocks.dev")
  }

  summary_data <- data |>
    dplyr::mutate(
      created_by = .env$user_id,
      updated_by = .env$user_id
    ) |>
    dplyr::select(
      property_id,
      competitor_id,
      property_name,
      property_website,
      property_address,
      property_email,
      property_phone,
      property_developer,
      property_manager,
      property_owner,
      property_type,
      property_rating,
      property_status,
      comp_status,
      year_built,
      most_recent_sale,
      distance_from_campus,
      property_image_url,
      property_description,
      created_by,
      updated_by
    )

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.property_summary"),
        records = summary_data,
        where_cols = c("property_name"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated property details."
      )

      shiny::showNotification(
        "Successfully updated property details.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property details: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property details.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(summary_data))
}



db_update_survey_property_amenities <- function(pool, new_values) {
  check_db_conn(pool)

  data <- new_values |>
    dplyr::mutate(
      property_id = purrr::map_int(.data$property_name, get_property_id_by_name),
      amenity_id = purrr::map_int(.data$amenity_name, get_amenity_id_by_name, pool = pool)
    )

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.property_amenities"),
        records = data,
        where_cols = c("property_name", "amenity_id"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated property amenities."
      )

      shiny::showNotification(
        "Successfully updated property amenities.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property amenities: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property amenities.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(data))
}

db_update_mkt_property_summary <- function(pool, property_id, new_values) {
  check_db_conn(pool)

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("mkt.property_summary"),
        records = new_values,
        where_cols = c("property_id"),
        skip_existing = FALSE
      )
      cli::cli_alert_success(
        "Successfully updated property details."
      )
      shiny::showNotification(
        "Successfully updated property details.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property details: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property details.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(new_values))
}

db_update_mkt_leasing_summary <- function(pool, property_id, leasing_week, new_values) {
  check_db_conn(pool)
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("mkt.leasing_summary"),
        records = new_values,
        where_cols = c("property_id", "leasing_week"),
        skip_existing = FALSE
      )
      cli::cli_alert_success(
        "Successfully updated property details."
      )
      shiny::showNotification(
        "Successfully updated property details.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property details: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property details.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(new_values))
}

db_update_mkt_short_term_leases <- function(pool, property_id, leasing_week, new_values) {
  check_db_conn(pool)
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  # browser()

  tryCatch(
    {
      # Check for existing survey entry in `mkt.short_term_leases`
      existing_entry <- db_read_tbl(pool, "mkt.short_term_leases", collect = FALSE) |>
        dplyr::filter(property_id == .env$property_id, leasing_week == .env$leasing_week) |>
        dplyr::collect() |>
        nrow()

      if (existing_entry > 0) {
        dbx::dbxUpsert(
          conn,
          DBI::SQL("mkt.short_term_leases"),
          records = new_values,
          where_cols = c("property_id", "leasing_week"),
          skip_existing = FALSE
        )
      } else {
        dbx::dbxInsert(
          conn,
          DBI::SQL("mkt.short_term_leases"),
          records = new_values
        )
      }

      cli::cli_alert_success(
        "Successfully updated property details."
      )
      shiny::showNotification(
        "Successfully updated property details.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property details: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property details.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(new_values))
}

db_update_mkt_fees <- function(pool, property_id, leasing_week, new_values) {
  check_db_conn(pool)
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  # browser()

  tryCatch(
    {
      # Check for existing survey entry in `mkt.short_term_leases`
      existing_entry <- db_read_tbl(pool, "mkt.fees", collect = FALSE) |>
        dplyr::filter(property_id == .env$property_id, leasing_week == .env$leasing_week) |>
        dplyr::collect() |>
        nrow()

      if (existing_entry > 0) {
        dbx::dbxUpdate(
          conn,
          DBI::SQL("mkt.fees"),
          records = new_values,
          where_cols = c("property_id", "leasing_week", "Fees")
        )
      } else {
        dbx::dbxInsert(
          conn,
          DBI::SQL("mkt.fees"),
          records = new_values
        )
      }

      cli::cli_alert_success(
        "Successfully updated property details."
      )
      shiny::showNotification(
        "Successfully updated property details.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property details: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property details.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(new_values))
}

db_update_mkt_parking <- function(pool, property_id, leasing_week, new_values) {
  check_db_conn(pool)
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  browser()

  tryCatch(
    {
      # Check for existing survey entry in `mkt.short_term_leases`
      existing_entry <- db_read_tbl(pool, "mkt.parking", collect = FALSE) |>
        dplyr::filter(property_id == .env$property_id, leasing_week == .env$leasing_week) |>
        dplyr::collect() |>
        nrow()

      if (existing_entry > 0) {
        dbx::dbxUpdate(
          conn,
          DBI::SQL("mkt.parking"),
          records = new_values,
          where_cols = c("property_id", "leasing_week")
        )
      } else {
        dbx::dbxInsert(
          conn,
          DBI::SQL("mkt.parking"),
          records = new_values
        )
      }

      cli::cli_alert_success(
        "Successfully updated property details."
      )
      shiny::showNotification(
        "Successfully updated property details.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update property details: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update property details.",
        duration = 500,
        type = "error"
      )
    }
  )

  return(invisible(new_values))
}
