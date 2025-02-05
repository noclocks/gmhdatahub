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
    property_ids = NULL) {
  check_db_conn(pool)

  hold <- db_read_tbl(pool, "gmh.pre_lease_summary", collect = FALSE)
  report_dates <- dplyr::pull(hold, "report_date") |> unique()
  prop_ids <- dplyr::pull(hold, "property_id") |> unique()

  if (is.null(report_date)) {
    report_date <- max(report_dates, na.rm = TRUE)
  }

  if (is.null(property_ids)) {
    property_ids <- prop_ids
  }

  # if (!report_date %in% report_dates) {
  #   cli::cli_alert_warning("No data found for the specified report date: {.field {report_date}}.")
  #   report_date <- max(report_dates, na.rm = TRUE)
  #   cli::cli_alert_info("Using the latest report date instead: {.field {report_date}}.")
  # }
  #
  # if (all(property_ids %in% prop_ids)) {
  #   hold <- dplyr::filter(hold, .data$report_date == .env$report_date)
  # } else {
  #   cli::cli_alert_warning("No data found for the specified property IDs: {.field {property_ids}}.")
  #   hold <- dplyr::filter(hold, .data$report_date == .env$report_date)
  # }

  # browser()

  out <- dplyr::filter(
    hold,
    .data$report_date == .env$report_date,
    .data$property_id %in% .env$property_ids
  ) |>
    dplyr::collect() |>
    # replace NAs
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.numeric),
        ~ dplyr::if_else(
          is.na(.x),
          0,
          .x
        )
      )
    )

  # if (collect) {
  return(dplyr::collect(out))
  # }
  # return(out)
}

db_read_gmh_model_beds <- function(pool, collect = TRUE) {
  check_db_pool(pool)

  db_read_tbl(pool, tbl_name = "gmh.model_beds", collect = collect)
}

db_read_gmh_partners <- function(pool, collect = TRUE) {
  check_db_pool(pool)

  db_read_tbl(pool, "gmh.partners", collect = collect)
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

db_read_survey_metrics <- function(pool) {
  check_db_pool(pool)

  pool <- pool::poolCheckout(pool)
  on.exit(pool::poolReturn(pool), add = TRUE)

  tryCatch(
    {
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
    },
    error = function(e) {
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
    }
  )
}

db_read_survey_property_summary <- function(
    pool,
    survey_id = NULL,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.property_summary", collect = FALSE)

  if (is.null(survey_id)) {
    survey_id <- db_read_survey_id(pool, property_id = property_id)
  }

  filters <- list(
    survey_id = survey_id,
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
      dplyr::filter(acc, !!rlang::sym(col) == !!filters[[col]])
    },
    .init = hold
  )

  # collect and return
  if (collect) {
    return(dplyr::collect(hold_filtered))
  } else {
    return(hold_filtered)
  }

}

db_read_survey_leasing_summary <- function(
    pool,
    survey_id = NULL,
    property_id = NULL,
    competitor_id = NULL,
    leasing_week_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.leasing_summary", collect = FALSE)

  filters <- list(
    survey_id = survey_id,
    property_id = property_id,
    competitor_id = competitor_id,
    leasing_week_id = leasing_week_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_short_term_leases <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    leasing_week_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.short_term_leases", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    leasing_week_id = leasing_week_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_fees <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    leasing_week_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.fees", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    leasing_week_id = leasing_week_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_property_amenities <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.property_amenities", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_unit_amenities <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.unit_amenities", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_unit_amenities_rates_premiums <- function(
  pool,
  property_id = NULL,
  competitor_id = NULL,
  property_name = NULL,
  collect = TRUE
) {

  hold <- db_read_tbl(pool, "survey.unit_amenities_rates_premiums", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_hours <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.hours", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_parking <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.parking", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_utilities <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.utilities", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_notes <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    leasing_week_id = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.notes", collect = FALSE) |>
    dplyr::mutate(
      note_tags = as.character(.data$note_tags) |>
        stringr::str_remove_all("\\{") |>
        stringr::str_remove_all("\\}")
    )

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name,
    leasing_week_id = leasing_week_id
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_rents_by_floorplan <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    property_name = NULL,
    leasing_week_id = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.rents_by_floorplan", collect = FALSE)

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    property_name = property_name,
    leasing_week_id = leasing_week_id
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
    .x = names(filters),
    .f = function(acc, col) {
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

db_read_survey_id <- function(pool, property_id = NULL, competitor_id = NULL, leasing_week_id = NULL) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.surveys")

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id,
    leasing_week_id = leasing_week_id
  ) |>
    purrr::compact()

  if (length(filters) == 0) {
    return(hold)
  }

  # filter using dynamic conditions
  hold_filtered <- hold |>
    dplyr::filter(
      dplyr::across(
        tidyselect::all_of(names(filters)),
        ~ .x %in% filters[[dplyr::cur_column()]]
      )
    )

  if (nrow(hold_filtered) > 1) {
    hold_filtered <- hold_filtered |>
      dplyr::filter(
        .data$survey_date == max(.data$survey_date, na.rm = TRUE)
      )
  }

  dplyr::pull(hold_filtered, "survey_id")

}

db_read_gmh_properties <- function(
  pool,
  property_id = NULL,
  collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "gmh.properties", collect = FALSE)

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

  if (collect) {
    return(dplyr::collect(hold))
  } else {
    return(hold)
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
    .x = names(filters),
    .f = function(acc, col) {
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
    collect = TRUE
) {

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

db_read_survey_hours_tbl <- function(pool, selected_property_name = NULL) {
  check_db_pool(pool)
  hold <- db_read_tbl(pool, "survey.hours", collect = FALSE)
  if (!is.null(selected_property_name)) {
    hold <- dplyr::filter(hold, .data$property_name == .env$selected_property_name)
  }
  dplyr::collect(hold)
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
