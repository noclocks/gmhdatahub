db_read_sql_file <- function(sql_file) {
  readr::read_file(sql_file) |>
    purrr::pluck(1) |>
    stringr::str_replace_all("\n", " ") |>
    stringr::str_split(";") |>
    purrr::pluck(1) |>
    purrr::map_chr(~ stringr::str_trim(.x)) |>
    purrr::discard(~ .x == "")
}

db_run_sql <- function(sql_file, pool) {
  check_db_conn(pool)
  sql <- db_read_sql_file(sql_file)
  num_statements <- length(sql)
  cli::cli_alert_info("Discovered {.field {num_statements}} SQL statements in {.path {sql_file}}")
  purrr::walk(sql, function(sql) {
    tryCatch({
      cli::cli_alert_info("Executing SQL statement:\n{.pre {sql}}")
      pool::dbExecute(pool, sql)
      cli::cli_alert_success("Successfully executed SQL statement.")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to execute SQL statement:\n\n{.pre {sql}}")
      cli::cli_alert_danger("Error message:\n\n{.pre {e$message}}")
    })
  })
}

db_init <- function() {



}

db_seed_tbl <- function(pool, tbl_name, tbl_data) {
  check_db_conn(pool)
  tbl_exists <- pool::dbExistsTable(pool, DBI::SQL(tbl_name))
  if (!tbl_exists) {
    cli::cli_abort("Table {.field {tbl_name}} does not exist.")
  }
  tryCatch({
    cli::cli_alert_info("Seeding table {.field {tbl_name}}.")
    pool::dbWriteTable(pool, DBI::SQL(tbl_name), tbl_data, append = TRUE, row.names = FALSE)
    cli::cli_alert_success("Table {.field {tbl_name}} seeded.")
  }, error = function(e) {
    cli::cli_abort(
      c(
        "Failed to seed table {.field {tbl_name}}.",
        "Error: {.error_msg {e$message}}"
      )
    )
  })
}

db_create_tbl <- function(tbl, pool, overwrite = FALSE) {
  check_db_conn(pool)
  tbl_exists <- pool::dbExistsTable(pool, DBI::SQL(tbl))

  if (tbl_exists) {
    if (overwrite) {
      cli::cli_alert_warning("Overwriting table {.field {tbl}}.")
      tryCatch({
        pool::dbRemoveTable(pool, DBI::SQL(tbl))
        cli::cli_alert_success("Table {.field {tbl}} removed.")
      }, error = function(e) {
        cli::cli_abort(
          c(
            "Failed to remove table {.field {tbl}}.",
            "Error: {.error_msg {e$message}}"
          )
        )
      })
    } else {
      cli::cli_abort("Table already exists. Use `overwrite = TRUE` to overwrite.")
    }
  }

  tryCatch({
    cli::cli_alert_info("Creating table {.field {tbl}}.")
    pool::dbWriteTable(pool, DBI::SQL(tbl), value = get(tbl))
  })

}

db_read_tbl <- function(pool, tbl_name, collect = TRUE) {
  check_db_conn(pool)
  tbl_exists <- pool::dbExistsTable(pool, DBI::SQL(tbl_name))
  if (!tbl_exists) {
    cli::cli_abort("Table {.field {tbl_name}} does not exist in the database.")
  }
  if (stringr::str_detect(tbl_name, ".")) {
    schema <- stringr::str_split(tbl_name, "\\.")[[1]][1]
    name <- stringr::str_split(tbl_name, "\\.")[[1]][2]
    schema_tbl <- dbplyr::in_schema(schema, name)
  } else {
    schema_tbl <- tbl_name
  }
  hold <- dplyr::tbl(pool, schema_tbl)
  if (!collect) return(hold)
  dplyr::collect(hold)
}


db_archive <- function() {


}


db_reset <- function() {



}

db_run_migrations <- function() {



}
