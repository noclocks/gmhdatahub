db_update_survey_property_amenities <- function(pool, new_values) {

  check_db_conn(pool)

  data <- new_values |>
    dplyr::mutate(
      property_id = purrr::map_int(.data$property_name, get_property_id_by_name),
      amenity_id = purrr::map_int(.data$amenity_name, get_amenity_id_by_name, pool = pool)
    )

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch({

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
  }, error = function(e) {
    cli::cli_alert_danger(
      "Failed to update property amenities: {.error {e$message}}"
    )
    shiny::showNotification(
      "Failed to update property amenities.",
      duration = 500,
      type = "error"
    )
  })

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

  tryCatch({
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
    })

  return(invisible(new_values))
}

db_update_mkt_fees <- function(pool, property_id, leasing_week, new_values) {
  check_db_conn(pool)
  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  # browser()

  tryCatch({
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
  })

  return(invisible(new_values))
}
