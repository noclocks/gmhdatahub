
get_db_conn_str <- function(schemas = NULL, db_config = get_db_config()) {

  validate_db_config(db_config)

  if (!is.null(schemas)) {
    schemas <- rlang::arg_match(
      schemas,
      c("public", "app", "auth", "gmh", "entrata", "survey", "mkt", "logs"),
      multiple = TRUE
    ) |>
      stringr::str_c(collapse = ",")
    schemas <- paste0("?schemas=", schemas)
  } else {
    schemas <- ""
  }

  glue::glue(
    "postgresql://{db_config$user}:{URLencode(db_config$password, reserved = TRUE)}@{db_config$host}:{db_config$port}/{db_config$dbname}{schemas}"
  )
}

db2dbml <- function(schemas = NULL, output_file = "dev/gmh.dbml") {

  db_config <- config::get("db")
  exe <- normalizePath("~/AppData/Roaming/npm/db2dbml.cmd")
  conn_str <- get_db_conn_str(schemas, db_config)
  cmd <- glue::glue("{exe} postgres '{conn_str}' -o {output_file}")
  cmd_str <- glue::glue("{basename(exe)} postgres '{conn_str}' -o {output_file}")

  tryCatch({
    cli::cli_alert_info("Running Command: {.field {cmd_str}}")
    processx::run(exe, args = c("postgres", conn_str, "-o", output_file))
    cli::cli_alert_success("Successfully generated DBML file: {.file {output_file}}.")
    file.edit(output_file)
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to generate DBML file: {.file {file}}.\n",
        "Error: {.error {conditionMessage(e)}}"
      )
    )
  })

}

db2dbml(c("gmh", "entrata", "survey"), "dev/gmh.database.dbml")
db2dbml(c("gmh"), "dev/gmh.schema.dbml")
db2dbml(c("gmh", "entrata", "survey"), "dev/gmh.database.dbml")
