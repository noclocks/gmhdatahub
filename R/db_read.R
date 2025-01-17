
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


db_read_gmh_pre_lease_summary_tbl <- function(
    pool,
    report_date = NULL,
    property_ids = NULL,
    collect = TRUE
) {

  check_db_conn(pool)

  hold <- db_read_tbl(pool, "gmh.pre_lease_summary", collect = FALSE)

  if (!is.null(report_date)) {
    hold <- dplyr::filter(hold, .data$report_date == .env$report_date)
  } else {
    hold <- dplyr::filter(hold, report_date == max(report_date, na.rm = TRUE))
  }

  if (!is.null(property_ids)) {
    hold <- dplyr::filter(hold, property_id %in% property_ids)
  }

  if (!collect) {
    return(hold)
  }

  dplyr::collect(hold)

}

db_read_gmh_model_beds <- function(pool, collect = TRUE) {

  check_db_pool(pool)

  db_read_tbl(pool, tbl_name = "gmh.model_beds", collect = collect)

}

db_read_gmh_partners <- function(pool, collect = TRUE) {

  check_db_pool(pool)

  db_read_tbl(pool, "investment_partners", schema = "gmh", collect = collect)

}

db_read_gmh_locations <- function(pool, collect = TRUE) {
  check_db_pool(pool)
  db_read_tbl(pool, "gmh.locations", collect = collect)
}

db_read_gmh_property_summary <- function(pool, property_ids = NULL) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "gmh.property_summary", collect = FALSE)

  if (!is.null(property_ids)) {
    hold <- dplyr::filter(hold, property_id %in% property_ids)
  }

  if (!collect) {
    return(hold)
  }

  dplyr::collect(hold)

}

db_read_gmh_universities <- function(pool, collect = TRUE) {
  check_db_pool(pool)
  db_read_tbl(pool, "gmh.universities", collect = collect)
}

db_read_survey_metrics <- function(pool) {

  check_db_pool(pool)

  pool <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(pool), add = TRUE)

  tryCatch({

    total_properties_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "properties",
      .con = pool
    )

    total_competitors_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "competitors",
      .con = pool
    )

    total_surveys_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "surveys",
      .con = pool
    )

    total_responses_qry <- glue::glue_sql(
      "SELECT COUNT(*) AS count FROM {`schema`}.{`tbl`}",
      schema = "mkt",
      tbl = "responses",
      .con = pool
    )

    total_properties <- DBI::dbGetQuery(pool, total_properties_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    total_competitors <- DBI::dbGetQuery(pool, total_competitors_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    total_surveys <- DBI::dbGetQuery(pool, total_surveys_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    total_responses <- DBI::dbGetQuery(pool, total_responses_qry) |>
      dplyr::pull("count") |>
      purrr::pluck(1) |>
      as.integer()

    cli::cli_alert_success("Successfully retrieved survey metrics from database.")

    return(
      list(
        total_properties = total_properties,
        total_competitors = total_competitors,
        total_surveys = total_surveys,
        total_responses = total_responses
      )
    )

  }, error = function(e) {

    cli::cli_alert_danger(
      c(
        "Failed to retrieve survey metrics from database.\n",
        "Error: {.error {conditionMessage(e)}}"
      )
    )

    return(
      list(
        total_properties = 0,
        total_competitors = 0,
        total_surveys = 0,
        total_responses = 0
      )
    )

  })

}

db_read_mkt_property_summary <- function(pool, property_id = NULL) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.property_summary", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |> dplyr::pull("property_id") |> unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    }
    hold <- dplyr::filter(hold, .data$property_id == as.character(.env$property_id))
  }

  dplyr::collect(hold)

}

db_read_mkt_leasing_summary <- function(pool, property_id = NULL, leasing_week = NULL) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "mkt.leasing_summary", collect = FALSE)

  if (!is.null(property_id)) {
    prop_ids <- hold |> dplyr::pull("property_id") |> unique()
    if (!property_id %in% prop_ids) {
      cli::cli_alert_warning("No data found for the specified property ID: {.field {property_id}}.")
    } else {
      hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
    }
  }

  if (!is.null(leasing_week)) {
    leasing_week_dates <- hold |> dplyr::pull("leasing_week") |> unique()
    if (!leasing_week %in% leasing_week_dates) {
      cli::cli_alert_warning("No data found for the specified leasing week: {.field {leasing_week}}.")
    } else {
      hold <- dplyr::filter(hold, .data$leasing_week == .env$leasing_week)
    }
  }

  dplyr::collect(hold)

}

db_read_gmh_leasing_calendar <- function(pool, date_key = Sys.Date()) {
  check_db_pool(pool)
  date_key <- format(date_key, "%Y-%m-%d")
  db_read_tbl(pool, "gmh.leasing_calendar", collect = FALSE) |>
    dplyr::filter(.data$date_key == .env$date_key) |>
    dplyr::collect()
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

db_read_survey_property_ids <- function(pool, ...) {

  db_read_tbl(pool, "mkt.properties", collect = FALSE) |>
    dplyr::filter(.data$is_competitor == FALSE) |>
    dplyr::pull("property_id")

}
