
#  ------------------------------------------------------------------------
#
# Title : Database Initialization
#    By : Jimmy Briggs
#  Date : 2024-11-14
#
#  ------------------------------------------------------------------------

db_init <- function(conn, force = FALSE, verbose = TRUE, ...) {

  if (!pool::dbIsValid(conn)) {
    cli::cli_abort(
      "The database connection is invalid. Please check the connection and try again."
    )
  }

  # check if the database is empty
  if (db_has_tables(conn)) {
    cli::cli_alert(
      "The database is already initialized. No action taken."
    )
    return(invisible())
  }

  # create the database schemas
  db_create_schema(conn, "entrata", verbose = verbose)
  db_create_schema(conn, "gmh", verbose = verbose)
  db_create_schema(conn, "app", verbose = verbose)
  db_create_schema(conn, "auth", verbose = verbose)

  # create the database tables
  db_create_table(conn, "entrata", "portfolios", portfolios, verbose = verbose)






}
