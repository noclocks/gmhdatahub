
#  ------------------------------------------------------------------------
#
# Title : Database Connection
#    By : Jimmy Briggs
#  Date : 2024-11-13
#
#  ------------------------------------------------------------------------

db_connect <- function(db_config = get_db_config(), ...) {

  conn <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db_config$dbname,
    host = db_config$host,
    port = db_config$port,
    username = db_config$username,
    password = db_config$password
  )

  return(conn)

}

debounced_db_query <- shiny::debounce(function(conn, qry, ...) {

  # query database
  pool::dbGetQuery(conn, qry, ...)

}, millis = 1000)

