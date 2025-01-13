
#  ------------------------------------------------------------------------
#
# Title : Database Creation
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

#' Create Database Table
#'
#' @description
#' Create a new table in the database.
#'
#' @inheritParams .shared-params
#' @param tbl Character string of the table name.
#' @param schema Character string of the schema name.
#' @param df Data frame to write to the database.
#' @param ... Additional arguments passed to `DBI::dbWriteTable()`.
#'
#' @returns
#' Invisible `NULL`.
#'
#' @export
db_create_tbl <- function(
    conn,
    tbl,
    schema,
    df,
    ...
) {

  check_db_conn(conn)

  schema <- rlang::arg_match0(
    schema,
    c("public", "entrata", "app", "gmh", "mkt", "logs", "survey", "meta", "auth", "util", "ext")
  )

  if (inherits(conn, "Pool")) {
    pool <- conn
    conn <- pool::poolCheckout(conn)
    on.exit(pool::poolReturn(conn), add = TRUE)
  }

  if (db_tbl_exists(conn, tbl, schema)) {
    cli::cli_alert_warning("Table {.field {schema_tbl}} already exists.")
    return(invisible())
  }

  tryCatch({
    DBI::dbWriteTable(conn, tbl, df, schema = schema, ...)
    cli::cli_alert_success("Successfully created {.field {schema_tbl}}.")
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to create table: {.field {schema_tbl}}.\n",
        "Error: {.error {conditionMessage(e)}}"
      )
    )
  })

}
