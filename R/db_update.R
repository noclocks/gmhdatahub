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
