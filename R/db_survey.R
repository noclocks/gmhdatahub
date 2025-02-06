
#  ------------------------------------------------------------------------
#
# Title : Survey Schema Database Functions
#    By : Jimmy Briggs
#  Date : 2025-02-05
#
#  ------------------------------------------------------------------------

.survey_sections <- c(
  "property_summary",
  "leasing_summary",
  "short_term_leases",
  "fees",
  "property_amenities",
  "unit_amenities",
  "unit_amenities_rates_premiums",
  "hours",
  "parking",
  "utilities",
  "notes",
  "rents_by_floorplan"
)

# db_submit_survey <- function(pool, survey_data) {
#
#   check_db_pool(pool)
#
#   purrr::walk(
#     .survey_sections,
#     function(section) {
#       tbl_name <- db_schema_tbl_name(schema = "survey", tbl_name = section)
#       tbl_exists <- db_tbl_exists(conn = pool, tbl_name = section, in_schema = "survey")
#       if (!tbl_exists) {
#         cli::cli_abort(
#           c(
#             "Table {.code {section_tbl}} does not exist in the database.\n",
#             "Please ensure the table exists before submitting survey data."
#           )
#         )
#       }
#
#     }
#
#
#   )
#
#   pool <- pool::poolCheckout(pool)
#   on.exit(pool::poolReturn(pool), add = TRUE)
#
#   tryCatch(
#     {
#       # insert survey data
#       DBI::dbWriteTable(
#         pool,
#         "survey.surveys",
#         survey_data,
#         append = TRUE,
#         row.names = FALSE
#       )
#
#       cli::cli_alert_success("Successfully submitted survey data to database.")
#     },
#     error = function(e) {
#       cli::cli_alert_danger(
#         c(
#           "Failed to submit survey data to database.\n",
#           "Error: {.error {conditionMessage(e)}}"
#         )
#       )
#     }
#   )
# }
db_read_survey_properties <- function(
  pool,
  property_id = NULL,
  property_name = NULL,
  collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.properties")

  filters <- list(
    property_id = property_id,
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

db_read_survey_competitors <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.competitors")

  filters <- list(
    property_id = property_id,
    competitor_id = competitor_id
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

db_read_survey_section_data <- function(
    pool,
    section,
    ...,
    collect = TRUE
) {

  check_db_pool(pool)
  section <- rlang::arg_match0(section, .survey_sections)

  # collect any filters in dots (should be named and one of the available filters,
  # i.e. property_id, competitor_id, leasing_week_id, property_name)
  filters <- list(...)

  # get the table name
  section_tbl <- glue::glue("survey.{section}")

  # read the table
  hold <- db_read_tbl(pool, section_tbl, collect = FALSE)

  if (length(filters) == 0) {
    if (collect) {
      return(dplyr::collect(hold))
    } else {
      return(hold)
    }
  }

  # check filters and remove any invalid
  filters <- filters |> purrr::keep(~ .x %in% colnames(hold))

  # filter the data
  hold_filtered <- purrr::reduce(
    names(filters),
    function(acc, col) {
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

db_read_survey_property_summary <- function(
    pool,
    property_id = NULL,
    competitor_id = NULL,
    collect = TRUE
) {

  check_db_pool(pool)

  hold <- db_read_tbl(pool, "survey.property_summary", collect = FALSE)

  if (is.null(property_id) && is.null(competitor_id)) {
    if (collect) {
      return(dplyr::collect(hold))
    } else {
      return(hold)
    }
  }

  if (!is.null(competitor_id)) {
    hold <- dplyr::filter(hold, .data$competitor_id == .env$competitor_id)
  } else {
    hold <- dplyr::filter(hold, .data$property_id == .env$property_id)
  }

  # collect and return
  if (collect) {
    return(dplyr::collect(hold))
  } else {
    return(hold)
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

  if (nrow(hold_filtered) == 0) {
    # try latest leasing week id
    hold_filtered <- hold |>
      dplyr::filter(
        leasing_week_id == max(.data$leasing_week_id, na.rm = TRUE)
      )
  }

  if (nrow(hold_filtered) > 1) {
    hold_filtered <- hold_filtered |>
      dplyr::filter(
        .data$survey_date == max(.data$survey_date, na.rm = TRUE)
      )
  }

  dplyr::pull(hold_filtered, "survey_id")

}
