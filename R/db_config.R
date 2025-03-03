#  ------------------------------------------------------------------------
#
# Title : Database Configuration
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

#' Database Configuration
#'
#' @name db_config
#'
#' @description
#' Functions to retrieve and validate the database configuration.
#'
#' - `get_db_config()`: retrieves the database configuration from the specified
#'   configuration file and returns the configuration as a list. You can
#'   optionally specify a key to retrieve a specific configuration value.
#'
#' - `validate_db_config()`: validates the database configuration to ensure all
#'   required configuration keys are present.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' - `get_db_config()`: A list, or vector (if `key` is specified), corresponding to the contents of the `db`
#'    configuration key's values (i.e. `dbname`, `host`, `port`, `username`, and `password`). If `key` is specified,
#'    the function will return the value corresponding to the specified key.
#'
#' - `validate_db_config()`: An invisible `NULL` if the configuration is valid. The function will throw an error if the
#'   configuration is invalid.
#'
#' @seealso [db_connect()]
NULL

#' @rdname db_config
#' @export
#' @importFrom cli cli_abort
#' @importFrom config get
#' @importFrom rlang arg_match
get_db_config <- function(
    key = NULL,
    file = Sys.getenv("R_CONFIG_FILE", pkg_sys("config/config.yml")),
    config = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  # normalize path to config file
  file <- normalizePath(file, mustWork = FALSE)

  # ensure config file exists
  if (!file.exists(file)) {
    cli::cli_abort(
      c(
        "Provided configuration file: {.field {basename(file)}} not found ",
        "under path: {.path {dirname(file)}}. Please ensure the file exists."
      )
    )
  }

  # attempt to read the configuration file
  cfg <- tryCatch(
    {
      config::get(
        value = "db",
        file = file,
        config = config
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Error reading configuration file {.field {basename(file)}}.",
          "Ensure the file contains a {.code db} configuration block for ",
          " the {.field {config}} configuration.",
          "Error: {.error_message {e}}"
        )
      )
    }
  )

  keys <- names(cfg)

  if (!is.null(key)) {
    key <- rlang::arg_match(key, keys)
    return(cfg[[key]])
  }

  return(cfg)
}

#' @rdname db_config
#' @export
#' @importFrom cli cli_abort
validate_db_config <- function(cfg, arg = rlang::caller_arg(cfg), call = rlang::caller_env()) {
  if (!is.list(cfg)) {
    cli::cli_abort("Invalid {.arg cfg}: Must be a list.", call = call)
  }
  required_keys <- c("dbname", "host", "port", "user", "password")
  missing_keys <- required_keys[!required_keys %in% names(cfg)]
  if (length(missing_keys) > 0) {
    cli::cli_abort(
      "Missing required configuration keys: {.field {missing_keys}}. ",
      call = call
    )
  }
  return(invisible(NULL))
}


# connection string -------------------------------------------------------

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
