
#' Database Update Functions
#'
#' @name db_update
#'
#' @description
#' These functions are used to update data in the database.
#'
#' @param pool A database connection pool object.
#' @param new_values A data frame containing the new values to update.
#' @param property_id (Optional) The property ID to update.
#' @param competitor_id (Optional) The competitor ID to update.
#' @param user_id (Optional) The user ID to update.
#'
#' @returns
#' The updated data (invisible).
#'
#' @export
NULL

#' @rdname db_update
#' @importFrom dplyr mutate select pull
#' @importFrom purrr map_int
#' @importFrom pool poolCheckout poolReturn
#' @importFrom dbx dbxUpsert
#' @importFrom shiny showNotification
#' @importFrom cli cli_alert_success cli_alert_danger
db_update_survey_property_summary <- function(
    pool,
    new_values,
    property_id = NULL,
    competitor_id = NULL,
    user_id = NULL
) {

  check_db_conn(pool)

  data <- new_values

  if (!all(c("property_name", "competitor_id") %in% colnames(data))) {

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

  if (is.null(user_id)) {
    if (!"user_id" %in% colnames(data)) {
      user_id <- get_user_id_by_email(pool, "default_user@example.com")
    } else {
      user_id <- .env$user_id
    }
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

db_update_survey_leasing_summary <- function(pool, new_values) {
  check_db_conn(pool)

  data <- new_values

  if (!all(c("property_name", "competitor_id") %in% colnames(data))) {
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

  if (!"leasing_week_id" %in% colnames(data)) {
    data$leasing_week_id <- get_leasing_week_id_by_date(get_leasing_week_start_date())
  }

  if (!all(c("updated_by") %in% colnames(data))) {
    data$updated_by <- get_user_id_by_email(pool, "default_user@example.com")
  }

  if (!"survey_id" %in% colnames(data)) {
    data$survey_id <- db_read_survey_id(
      pool,
      property_id = data$property_id,
      competitor_id = data$competitor_id,
      leasing_week_id = data$leasing_week_id
    )
  }

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.leasing_summary"),
        records = data,
        where_cols = c("property_name", "survey_id", "leasing_week_id"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated survey.leasing_summary"
      )

      shiny::showNotification(
        "Successfully updated leasing summary data.",
        duration = 500,
        type = "default"
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update leasing_summary: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update leasing_summary.",
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
    user_id <- get_user_id_by_email(pool, "default_user@example.com")
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

db_update_survey_hours <- function(pool, new_values) {

  check_db_conn(pool)

  data <- new_values

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.hours"),
        records = data,
        where_cols = c("property_name", "day_of_week"),
        skip_existing = FALSE
      )

      cli::cli_alert_success(
        "Successfully updated hours."
      )

      shiny::showNotification(
        "Successfully updated hours."
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to update hours: {.error {e$message}}"
      )
      shiny::showNotification(
        "Failed to update hours.",
        type = "error"
      )
    }
  )

  return(invisible(data))
}

db_update_survey_notes <- function(pool, new_values) {

  check_db_conn(pool)

  data <- new_values |>
    dplyr::mutate(
      note_type = dplyr::coalesce(.data$note_type, "General"),
      note_actionable = dplyr::coalesce(.data$note_actionable, FALSE),
      note_status = dplyr::coalesce(.data$note_status, "Pending"),
      note_tags = dplyr::coalesce(.data$note_tags, "{}")
    )

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxInsert(
        conn,
        DBI::SQL("survey.notes"),
        records = data
      )

      cli::cli_alert_success("Successfully updated notes.")
      shiny::showNotification("Successfully updated notes.")
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to update notes: {.error {e$message}}")
      shiny::showNotification("Failed to update notes.", type = "error")
    }
  )

  return(invisible(data))

}

db_update_survey_short_term_leases <- function(pool, new_values) {

  check_db_conn(pool)

  data <- new_values

  conn <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(conn))

  tryCatch(
    {
      dbx::dbxUpsert(
        conn,
        DBI::SQL("survey.short_term_leases"),
        records = data,
        where_cols = c("property_name", "leasing_week_id"),
        skip_existing = FALSE
      )

      cli::cli_alert_success("Successfully updated short-term leases.")
      shiny::showNotification("Successfully updated short-term leases.")
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to update notes: {.error {e$message}}")
      shiny::showNotification("Failed to update short-term leases.", type = "error")
    }
  )

  return(invisible(data))

}
