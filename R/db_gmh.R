
#  ------------------------------------------------------------------------
#
# Title : Database Functions for the GMH Schema
#    By : Jimmy Briggs
#  Date : 2025-03-21
#
#  ------------------------------------------------------------------------

#' GMH Database Functions
#'
#' @description
#'

db_read_gmh_properties <- function(
    pool,
    property_ids = NULL,
    partner_ids = NULL,
    portfolio_ids = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "gmh.properties", collect = FALSE)

  filts <- list(
    property_id = property_ids,
    partner_id = partner_ids,
    portfolio_id = portfolio_ids
  ) |>
    purrr::compact()

  if (length(filts) == 0) {
    if (collect) {
      return(dplyr::collect(hold))
    } else {
      return(hold)
    }
  }

  hold_filtered <- purrr::reduce(
    names(filts),
    function(acc, col) {
      dplyr::filter(acc, !!rlang::sym(col) %in% !!filts[[col]])
    },
    .init = hold
  )

  if (collect) {
    return(dplyr::collect(hold_filtered))
  } else {
    return(hold_filtered)
  }

}

db_read_gmh_competitors <- function(
    pool,
    competitor_id = NULL,
    property_id = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "gmh.competitors", collect = FALSE)

  filters <- list(
    competitor_id = competitor_id,
    property_id = property_id
  ) |>
    purrr::compact()

  if (length(filters) == 0) {
    if (collect) {
      return(dplyr::collect(hold))
    } else {
      return(hold)
    }
  }

  hold_filtered <- purrr::reduce(
    names(filters),
    function(acc, col) {
      dplyr::filter(acc, !!rlang::sym(col) == !!filters[[col]])
    },
    .init = hold
  )

  if (collect) {
    return(dplyr::collect(hold_filtered))
  } else {
    return(hold_filtered)
  }
}

db_read_gmh_map_data <- function(
    pool,
    property_id = NULL,
    collect = TRUE) {
  check_db_pool(pool)

  hold <- db_read_tbl(pool, "gmh.locations", collect = FALSE)

  prop_names <- db_read_gmh_properties(pool, property_id = property_id, collect = FALSE) |>
    dplyr::select("property_id", "property_name") |>
    dplyr::distinct()
  uni_names <- db_read_gmh_universities(pool, property_id = property_id, collect = FALSE) |>
    dplyr::select("university_id", "university_name", "property_id") |>
    dplyr::distinct()
  comp_names <- db_read_gmh_competitors(pool, property_id = property_id, collect = FALSE) |>
    dplyr::select("competitor_id", "competitor_name", "property_id") |>
    dplyr::distinct()

  out <- hold |>
    dplyr::left_join(prop_names, by = c("location_name" = "property_name")) |>
    dplyr::left_join(uni_names, by = c("location_name" = "university_name")) |>
    dplyr::left_join(comp_names, by = c("location_name" = "competitor_name")) |>
    dplyr::mutate(
      property_id = dplyr::coalesce(property_id.x, property_id.y, property_id)
    ) |>
    dplyr::filter(!is.na(property_id)) |>
    dplyr::select(-c("property_id.x", "property_id.y"))

  if (collect) {
    return(dplyr::collect(out))
  } else {
    return(out)
  }
}

db_read_gmh_leasing_calendar <- function(pool, date_key = Sys.Date()) {
  check_db_pool(pool)
  date_key <- format(date_key, "%Y-%m-%d")
  db_read_tbl(pool, "gmh.leasing_calendar", collect = FALSE) |>
    dplyr::filter(.data$date_key == .env$date_key) |>
    dplyr::collect()
}


db_read_gmh_properties_partners <- function(
  pool,
  property_ids = NULL,
  partner_ids = NULL,
  collect = TRUE
) {
  check_db_pool(pool)
  properties <- db_read_tbl(pool, "gmh.properties", collect = FALSE)
  partners <- db_read_tbl(pool, "gmh.partners", collect = FALSE)

  out <- properties |>
    dplyr::left_join(
      partners |>
        dplyr::select("partner_id", "partner_name"),
      , by = c("partner_id")
    ) |>
    dplyr::filter(
      if (!is.null(property_ids)) {
        property_id %in% property_ids
      } else {
        TRUE
      },
      if (!is.null(partner_ids)) {
        partner_id %in% partner_ids
      } else {
        TRUE
      }
    ) |>
    dplyr::select(
      "property_id",
      "partner_id",
      "property_name",
      "partner_name",
      tidyselect::everything()
    )

  if (collect) {
    return(dplyr::collect(out))
  } else {
    return(out)
  }
}

# gmh_props <- db_read_tbl(pool, "gmh.properties") |> dplyr::left_join(db_read_tbl(pool, "gmh.partners"), by = c("")

db_read_gmh_pre_lease_summary_tbl <- function(
    pool,
    report_date = NULL,
    property_ids = NULL,
    partner_names = NULL,
    collect = TRUE
) {

  check_db_conn(pool)

  hold <- db_read_tbl(pool, "gmh.pre_lease_summary", collect = FALSE)

  if (is.null(report_date)) {
    report_date <- dplyr::pull(hold, "report_date") |>
      unique() |>
      max(na.rm = TRUE)
  } else {
    report_date <- as.Date(report_date)
    if (!report_date %in% dplyr::pull(hold, "report_date")) {
      report_date_orig <- report_date
      report_date <- dplyr::pull(hold, "report_date") |>
        unique() |>
        max(na.rm = TRUE)
      cli::cli_alert_warning(
        c(
          "Invalid report date provided: {.field {report_date_orig}}.",
          "Using most recent report date instead: {.field {report_date}}."
        )
      )
    }
  }

  hold <- dplyr::filter(hold, .data$report_date == .env$report_date)

  if (!is.null(property_ids)) {
    valid_prop_ids <- dplyr::pull(hold, "property_id") |> unique() |> as.integer()
    property_ids <- as.integer(property_ids)
    property_ids <- property_ids[property_ids %in% valid_prop_ids]
    hold <- dplyr::filter(hold, .data$property_id %in% .env$property_ids)
  }

  if (!is.null(partner_names)) {
    valid_partner_names <- dplyr::pull(hold, "investment_partner") |> unique()
    partner_names <- partner_names[partner_names %in% valid_partner_names]
    hold <- dplyr::filter(hold, .data$investment_partner %in% .env$partner_names)
  }

  if (collect) {
    return(dplyr::collect(hold))
  } else {
    return(hold)
  }
}

db_read_gmh_model_beds <- function(pool, property_ids = NULL, collect = TRUE) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, tbl_name = "gmh.model_beds", collect = FALSE)

  if (!is.null(property_ids)) {
    hold <- dplyr::filter(hold, .data$property_id %in% as.integer(.env$property_ids))
  }

  if (collect) {
    return(dplyr::collect(hold))
  } else {
    return(hold)
  }

}

db_update_gmh_model_beds <- function(pool, property_id, model_bed_count, notes = NULL) {

  check_db_pool(pool)

  property_id <- as.integer(property_id)
  model_bed_count <- as.integer(model_bed_count)
  notes <- if (is.null(notes)) character() else notes

  qry <- glue::glue_sql("
    UPDATE gmh.model_beds
    SET model_bed_count = {model_bed_count}, notes = {notes}
    WHERE property_id = {property_id}
  ", .con = pool)

  tryCatch({
    pool::dbExecute(
      pool,
      qry
    )
    cli::cli_alert_success("Updated property's model bed count: {.field {property_id}} -> {.field {model_bed_count}}")
    cli::cli_alert_info("Executed SQL: {.field {qry}}")
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to update property's model bed count: {.field {property_id}} -> {.field {model_bed_count}}",
        "Executed SQL: {.field {qry}}",
        "Error: {.error_message {e}}"
      )
    )
  })

}

db_read_gmh_partners <- function(pool, partner_ids = NULL, collect = TRUE) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "gmh.partners", collect = FALSE)

  if (!is.null(partner_ids)) {
    hold <- dplyr::filter(hold, .data$partner_id %in% .env$partner_ids)
  }

  if (collect) {
    return(dplyr::collect(hold))
  } else {
    return(hold)
  }

}

db_update_gmh_property_partner <- function(pool, property_id, partner_id) {

  check_db_pool(pool)

  property_id <- as.integer(property_id)
  partner_id <- as.integer(partner_id)

  qry <- glue::glue_sql("
    UPDATE gmh.properties
    SET partner_id = {partner_id}
    WHERE property_id = {property_id}
  ", .con = pool)

  tryCatch({
    pool::dbExecute(
      pool,
      qry
    )
    cli::cli_alert_success("Updated property's partner: {.field {property_id}} -> {.field {partner_id}}")
    cli::cli_alert_info("Executed SQL: {.field {qry}}")
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to update property's partner: {.field {property_id}} -> {.field {partner_id}}",
        "Executed SQL: {.field {qry}}",
        "Error: {.error_message {e}}"
      )
    )
  })

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

db_read_gmh_universities <- function(pool, property_id = NULL, collect = TRUE) {
  check_db_pool(pool)
  hold <- db_read_tbl(pool, "gmh.universities", collect = FALSE)
  if (!is.null(property_id)) {
    hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
  }
  if (collect) {
    return(dplyr::collect(hold))
  } else {
    return(hold)
  }
}

db_read_university_locations <- function(pool, collect = TRUE) {
  check_db_pool(pool)

  locs <- db_read_tbl(pool, "gmh.locations", collect = FALSE) |>
    dplyr::filter(.data$map_layer == "universities") |>
    dplyr::select(
      "university_name" = "location_name",
      dplyr::everything(),
      -c("geom", "created_at", "updated_at", "is_active", "location_id")
    )

  hold <- db_read_tbl(pool, "gmh.universities", collect = FALSE) |>
    dplyr::select(-c("created_at", "updated_at")) |>
    dplyr::left_join(locs, by = c("university_name"))

  if (collect) {
    return(dplyr::collect(hold))
  }

  return(hold)
}


