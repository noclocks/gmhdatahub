#  ------------------------------------------------------------------------
#
# Title : Manage Database Connection User Setting
#    By : Jimmy Briggs
#  Date : 2024-12-11
#
#  ------------------------------------------------------------------------

db_set_user_id <- function(conn, user_id) {
  check_db_conn(conn)

  qry <- glue::glue_sql("SELECT `public`.`set_user_id`({`user_id`})", .con = conn, user_id = user_id)
}

db_get_user_id <- function(conn) {
  check_db_conn(conn)

  user_id <- DBI::dbGetQuery(
    conn,
    "SHOW gmhdatahub.user_id"
  )

  return(user_id)
}


get_shiny_user_id <- function(session = shiny::getDefaultReactiveDomain()) {
  user_id <- if (!is.null(session)) {
    purrr::pluck(session, "userData", "user_id")
  } else {
    NULL
  }

  if (is.null(user_id)) {
    cli::cli_alert_warning("No user id found in session. Using default user id.")
    user_id <- 1
  } else {
    user_id <- as.integer(user_id)
  }

  return(user_id)
}

get_user_id_by_email <- function(pool, email) {
  check_db_conn(pool)

  db_emails <- db_read_tbl(pool, "survey.users") |>
    dplyr::pull("user_email") |>
    unique()

  if (!(email %in% db_emails)) {
    cli::cli_abort(
      "Provided {.arg email} not found in database. Please provide a valid email."
    )
  }

  db_read_tbl(pool, "survey.users") |>
    dplyr::filter(.data$user_email == .env$email) |>
    dplyr::pull("user_id")
}


# # check if inside a shiny app
#     if (!is.null(shiny::getDefaultReactiveDomain())) {
#       session <- shiny::getDefaultReactiveDomain()
#       # try and retrieve user id from session$userData$user_id
#
#
#     } else {
#       cli::cli_alert_warning("Not inside a shiny app. Using default user id.")
#       user_id <- 1
#     }
#   } else {
#     if (!is.integer(as.integer(user_id)) || user_id < 0) {
#       cli::cli_abort("Invalid {.arg user_id}: Must be a non-negative integer.")
#     }
#     user_id <- as.integer(user_id)
#   }
