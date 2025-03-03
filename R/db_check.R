#  ------------------------------------------------------------------------
#
# Title : Database Checks
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# check connection --------------------------------------------------------

#' Check Database Connection
#'
#' @description
#' This function checks if the provided connection is a valid database
#' connection. The function will throw an error if the connection is not
#' a valid DBI or pool connection.
#'
#' @inheritParams .shared-params
#' @inheritParams rlang::args_error_context
#'
#' @returns The provided connection object invisibly if it is valid.
#'
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom DBI dbIsValid
#' @importFrom pool dbIsValid
check_db_conn <- function(
    conn,
    arg = rlang::caller_arg(conn),
    call = rlang::caller_env()
) {

  is_dbi <- inherits(conn, "PqConnection")
  is_pool <- inherits(conn, "Pool")
  is_rstudio_conn <- inherits(conn, "connConnection")

  if (is_rstudio_conn) {
    return(invisible(conn))
  }

  if (!is_dbi && !is_pool) {
    cli::cli_abort(
      c(
        "Invalid Database Connection Provided: {.arg {arg}}",
        "The provided connection is not a valid database connection."
      ),
      call = call
    )
  }

  if (is_pool) {
    valid <- pool::dbIsValid(conn)
    if (!valid) {
      cli::cli_abort(
        c(
          "Invalid Database Connection Provided: {.arg {arg}}",
          "The provided connection pool is not valid."
        ),
        call = call
      )
    }
  }

  if (is_dbi) {
    valid <- DBI::dbIsValid(conn)
    if (!valid) {
      cli::cli_abort(
        c(
          "Invalid Database Connection Provided: {.arg {arg}}",
          "The provided DBI connection is not valid."
        ),
        call = call
      )
    }
  }

  return(invisible(conn))
}

#' @rdname check_db_conn
#' @export
check_db_pool <- check_db_conn


# table exists ------------------------------------------------------------

#' Check if Table Exists
#'
#' @description
#' This function checks if a table exists in the database.
#'
#' @inheritParams .shared-params
#' @param tbl_name The name of the table to check.
#' @param in_schema The schema in which the table resides.
#'
#' @returns
#' A logical value indicating if the table exists.
#'
#' @export
#'
#' @importFrom DBI dbExistsTable
#' @importFrom pool dbExistsTable
db_tbl_exists <- function(conn, tbl_name, in_schema = NULL) {
  check_db_conn(conn)

  schema_tbl_name <- db_schema_tbl_name(in_schema, tbl_name)

  func <- switch(class(conn)[[1]],
    "PqConnection" = DBI::dbExistsTable,
    "Pool" = pool::dbExistsTable
  )

  return(func(conn, schema_tbl_name))
}
