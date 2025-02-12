#  ------------------------------------------------------------------------
#
# Title : Database Read Functions
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

#' Read and Execute SQL File
#'
#' @description
#' This function reads an SQL file and executes the SQL code on the database connection.
#'
#' @inheritParams .shared-params
#' @param sql_file Path to the SQL file.
#' @param ... Additional arguments passed to `dbExecute()`.
#'
#' @returns
#' The result of the SQL execution.
#'
#' @export
#'
#' @importFrom pool dbExecute
#' @importFrom readr read_file
db_read_sql <- function(pool, sql_file, ...) {
  check_db_pool(pool)

  sql <- readr::read_file(sql_file)

  pool::dbExecute(pool, sql, ...)
}






db_read_mkt_leasing_summary <- function(pool, property_id = NULL, leasing_week = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.leasing_summary", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |>
      dplyr::pull("property_id") |>
      unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
    }
  }

  if (!is.null(leasing_week)) {
    leasing_week_dates <- hold |>
      dplyr::pull("leasing_week") |>
      unique()
    if (!leasing_week %in% leasing_week_dates) {
      cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
    }
  }

  hold <- dplyr::filter(hold, .data$leasing_week == .env$leasing_week)

  dplyr::collect(hold)
}

db_read_mkt_short_term_leases <- function(pool, property_id = NULL, leasing_week = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.short_term_leases", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |>
      dplyr::pull("property_id") |>
      unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
    }
  }

  if (!is.null(leasing_week)) {
    leasing_week_dates <- hold |>
      dplyr::pull("leasing_week") |>
      unique()
    if (!leasing_week %in% leasing_week_dates) {
      cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
    }
  }

  hold <- dplyr::filter(hold, .data$leasing_week == .env$leasing_week)

  dplyr::collect(hold)
}

db_read_mkt_fees <- function(pool, property_id = NULL, leasing_week = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.fees", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |>
      dplyr::pull("property_id") |>
      unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
    }
  }

  if (!is.null(leasing_week)) {
    leasing_week_dates <- hold |>
      dplyr::pull("leasing_week") |>
      unique()
    if (!leasing_week %in% leasing_week_dates) {
      cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
    }
  }

  hold <- hold |>
    dplyr::filter(leasing_week == .env$leasing_week) |>
    dplyr::select(-property_id, -leasing_week)

  dplyr::collect(hold)
}

db_read_mkt_property_amenities <- function(pool, property_id = NULL, leasing_week = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.property_amenities", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |>
      dplyr::pull("property_id") |>
      unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
    }
  }

  if (!is.null(leasing_week)) {
    leasing_week_dates <- hold |>
      dplyr::pull("leasing_week") |>
      unique()
    if (!leasing_week %in% leasing_week_dates) {
      cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
    }
  }

  # hold <- dplyr::filter(hold, .data$leasing_week == .env$leasing_week)
  hold <- hold |>
    dplyr::filter(leasing_week == .env$leasing_week) |>
    dplyr::select(-property_id, -leasing_week)

  dplyr::collect(hold)
}

db_read_mkt_unit_amenities <- function(pool, property_id = NULL, leasing_week = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.unit_amenities", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |>
      dplyr::pull("property_id") |>
      unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
    }
  }

  if (!is.null(leasing_week)) {
    leasing_week_dates <- hold |>
      dplyr::pull("leasing_week") |>
      unique()
    if (!leasing_week %in% leasing_week_dates) {
      cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
    }
  }

  hold <- hold |>
    dplyr::filter(leasing_week == .env$leasing_week) |>
    dplyr::select(-property_id, -leasing_week)

  dplyr::collect(hold)
}

db_read_mkt_parking <- function(pool, property_id = NULL, competitor_id = NULL, leasing_week = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.parking", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |>
      dplyr::pull("property_id") |>
      unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)

      if (!is.null(competitor_id)) {
        comp_ids <- hold |>
          dplyr::pull("competitor_id") |>
          unique()

        if (!competitor_id %in% comp_ids) {
          cli::cli_alert_warning("No data found for the specified competitor ID: {.field {competitor_id}}.")
        } else {
          hold <- dplyr::filter(hold, competitor_id == .env$competitor_id)
        }
      }
    }
  } # else if (!is.null(property_id) && !is.null(competitor_id)) {
  #   comp_ids <- hold |>
  #     dplyr::pull("competitor_id") |>
  #     unique()
  #
  #   if (!competitor_id %in% comp_ids) {
  #     cli::cli_alert_warning("No data found for the specified competitor ID: {.field {competitor_id}}.")
  #   } else {
  #     hold <- hold |>
  #       dplyr::filter(
  #         competitor_id == .env$competitor_id
  #       )
  #   }
  # }

  # if (!is.null(leasing_week)) {
  #   leasing_week_dates <- hold |>
  #     dplyr::pull("leasing_week") |>
  #     unique()
  #   if (!leasing_week %in% leasing_week_dates) {
  #     cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
  #   }
  # }

  hold <- hold |>
    dplyr::filter(updated_at == max(updated_at, na.rm = TRUE)) |>
    dplyr::select(-property_id, -competitor_id, -property_name, -created_at, -updated_at, -created_by, -updated_by)

  dplyr::collect(hold)
}



db_read_mkt_locations <- function(pool, property_ids = NULL) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.locations", collect = FALSE)

  if (!is.null(property_ids)) {
    hold <- dplyr::filter(hold, .data$property_id %in% .env$property_ids)
  }

  dplyr::collect(hold)
}

db_read_home_metrics <- function(pool, report_date = NULL, property_ids = NULL) {
  check_db_pool(pool)

  tbl <- dplyr::tbl(pool, I("entrata.pre_lease_summary"))
  tbl_report_date <- max(dplyr::pull(tbl, "report_date"), na.rm = TRUE)

  if (is.null(report_date)) {
    report_date <- tbl_report_date
  }

  tbl_filt <- dplyr::filter(tbl, .data$report_date == .env$report_date)

  total_properties <- length(dplyr::pull(tbl_filt, "property_id"))

  if (total_properties == 0) {
    cli::cli_alert_warning("No data found for the specified report date: {.field {report_date}}.")
    report_date <- tbl_report_date
    tbl_filt <- dplyr::filter(tbl, .data$report_date == .env$report_date)
    total_properties <- length(dplyr::pull(tbl_filt, "property_id"))
    cli::cli_alert_info("Using the latest report date instead: {.field {report_date}}.")
  }

  last_updated_at <- max(dplyr::pull(tbl_filt, "created_at"), na.rm = TRUE)
  occupancy_current <- mean(
    dplyr::pull(tbl_filt, "occupied_count") / dplyr::pull(tbl_filt, "rentable_unit_count"),
    na.rm = TRUE
  )
  occupancy_prior <- mean(
    dplyr::pull(tbl_filt, "preleased_count_prior") / dplyr::pull(tbl_filt, "rentable_unit_count"),
    na.rm = TRUE
  )
  occupancy_pct_change <- occupancy_current - occupancy_prior
  scheduled_rent_total <- sum(
    dplyr::pull(tbl, "scheduled_rent_total"),
    na.rm = TRUE
  )
  scheduled_rent_avg <- mean(
    dplyr::pull(tbl_filt, "avg_scheduled_rent"),
    na.rm = TRUE
  )
  variance_total <- sum(
    dplyr::pull(tbl_filt, "variance"),
    na.rm = TRUE
  )
  variance_avg <- variance_total / total_properties
  prelease_pct_current <- mean(
    dplyr::pull(tbl_filt, "preleased_percent"),
    na.rm = TRUE
  )
  prelease_pct_prior <- mean(
    dplyr::pull(tbl_filt, "preleased_percent_prior"),
    na.rm = TRUE
  )
  prelease_pct_change <- prelease_pct_current - prelease_pct_prior

  list(
    report_date = report_date,
    last_updated_at = last_updated_at,
    total_properties = total_properties,
    occupancy_current = occupancy_current,
    occupancy_prior = occupancy_prior,
    occupancy_pct_change = occupancy_pct_change,
    scheduled_rent_total = scheduled_rent_total,
    scheduled_rent_avg = scheduled_rent_avg,
    variance_total = variance_total,
    variance_avg = variance_avg,
    prelease_pct_current = prelease_pct_current,
    prelease_pct_prior = prelease_pct_prior,
    prelease_pct_change = prelease_pct_change
  )
}

db_read_recent_activity_logs <- function(pool, ...) {
  check_db_pool(pool)

  dplyr::tbl(pool, I("logs.recent_activity")) |>
    dplyr::arrange(dplyr::desc(created_at))
}



get_leasing_week_id_by_date <- function(pool, date) {
  check_db_pool(pool)

  leasing_week_start_date <- get_leasing_week_start_date(date)

  valid_leasing_weeks <- db_read_tbl(pool, "survey.leasing_weeks") |>
    dplyr::pull("leasing_week_start_date")

  if (!leasing_week_start_date %in% valid_leasing_weeks) {
    cli::cli_abort(
      c(
        "{.arg date} is not a valid leasing week start date. ",
        "Provided: {.field {date}}."
      )
    )
  }

  db_read_tbl(pool, "survey.leasing_weeks") |>
    dplyr::filter(.data$leasing_week_start_date == .env$leasing_week_start_date) |>
    dplyr::pull("leasing_week_id")
}
