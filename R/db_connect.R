
#  ------------------------------------------------------------------------
#
# Title : Database Connection
#    By : Jimmy Briggs
#  Date : 2024-11-29
#
#  ------------------------------------------------------------------------

#' Connect to GMH Database
#'
#' @description
#' Functions to connect, disconnect, and manage the database connection to the
#' GMH PostgreSQL database.
#'
#' - `db_connect()`: Connect to the GMH PostgreSQL database.
#' - `db_disconnect()`: Disconnect from the GMH PostgreSQL database.
#' - `db_checkout()`: Check out a connection from the database connection pool.
#' - `db_return()`: Return a connection to the database connection pool.
#'
#' @details
#' The `db_connect()` function establishes a connection pool to the GMH PostgreSQL
#' database hosted in Google Cloud SQL using the `pool` package.
#'
#' Additionally, the function performs the following:
#'
#' - Sets up automatic disconnect and cleanup of the connection pool when the R
#'   or shiny session ends.
#' - Sets the `app.user_id` database session variable
#' - Stores the connection pool in the shiny session object
#' - Stores the user id in the shiny session object
#' - Sets the `db.pool` option to the connection pool
#'
#' @inheritParams .shared-params
#'
#' @param user_id An integer representing the user id to use for the database
#'   connection. If not provided, the function will attempt to retrieve the
#'   user id from the shiny session object. If the function is not running
#'   inside a shiny app, the default user id of `0` will be used.
#'
#' @returns A `pool` object representing the connection pool to the GMH
#'   PostgreSQL database.
#'
#' @export
#'
#' @importFrom cli cli_alert_danger cli_alert_warning cli_abort cli_alert_info
#' @importFrom DBI SQL
#' @importFrom glue glue
#' @importFrom pool dbPool dbExecute poolClose
#' @importFrom purrr pluck
#' @importFrom RPostgres Postgres
#' @importFrom shiny getDefaultReactiveDomain onStop
db_connect <- function(db_config = get_db_config(), user_id = NULL) {

  validate_db_config(db_config)

  if (is.null(user_id)) {
    user_id <- get_shiny_user_id() %||% 1
  }

  tryCatch({
    conn <- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = db_config$dbname,
      host = db_config$host,
      port = db_config$port,
      user = db_config$user,
      password = db_config$password,
      maxSize = db_config$max_size %||% 10,  # Default to 10 connections
      minSize = db_config$min_size %||% 1,   # Default to 1 connection
      idleTimeout = db_config$idle_timeout %||% 60000, # Default to 60 seconds
      onCreate = function(conn) {
        pool::dbExecute(conn, glue::glue("SET SESSION {DBI::SQL('app.user_id')} = '{user_id}';"))
      }
    )

    shiny_session <- shiny::getDefaultReactiveDomain()

    if (!is.null(shiny_session)) {
      shiny::onStop(
        fun = function() {
          pool::poolClose(conn)
        },
        session = shiny_session
      )

      shiny_session$userData$db_pool <- conn
      shiny_session$userData$db_user_id <- user_id
    }

    options("db.pool" = conn)

    cli::cli_alert_info("Connected to Database: {db_config$dbname} on {db_config$host}.")

    return(conn)

  }, error = function(e) {
    cli::cli_abort(
      c(
        "Failed to connect to the database.",
        "Details: {conditionMessage(e)}",
        "i" = "Check if the database server is reachable and credentials are correct.",
        "i" = "Ensure that the database is configured to accept connections from this host."
      )
    )

  })

}

#' @rdname db_connect
#' @export
#' @importFrom pool poolClose
db_disconnect <- function(conn) {
  pool::poolClose(conn)
}

#' @rdname db_connect
#' @export
#' @importFrom pool poolCheckout
db_checkout <- function(conn) {
  pool::poolCheckout(conn)
}

#' @rdname db_connect
#' @export
#' @importFrom pool poolReturn
db_return <- function(conn) {
  pool::poolReturn(conn)
}

#' Get the Database Connection
#'
#' @description
#' This function retrieves the database connection pool from the shiny session
#' object if the function is running inside a shiny app. If the function is
#' not running inside a shiny app, it will attempt to retrieve the connection
#' pool from the global options. If the connection pool is not found, the
#' function will throw an error.
#'
#' @returns
#' A `pool` object representing the connection pool to the GMH PostgreSQL
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom shiny getDefaultReactiveDomain
get_db_conn <- function() {

  if (env_in_shiny_session()) {
    conn <- shiny::getDefaultReactiveDomain()$userData$db_pool
    return(conn)
  }

  conn <- getOption("db.pool", NULL)
  if (is.null(conn)) {
    cli::cli_abort(
      c(
        "Failed to retrieve the database connection.",
        "i" = "Ensure that the database connection is established."
      )
    )
  }
  return(conn)
}




