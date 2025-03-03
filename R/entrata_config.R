
#  ------------------------------------------------------------------------
#
# Title : Entrata Configuration
#    By : Jimmy Briggs
#  Date : 2024-11-24
#
#  ------------------------------------------------------------------------

#' Entrata API Configuration
#'
#' @name entrata_config
#'
#' @family Entrata
#' @family Configuration
#'
#' @description
#' Entrata API configuration functions for setting, retrieving, and validating the
#' Entrata API configuration.
#'
#' - `get_entrata_config()`: This function retrieves the Entrata configuration
#'   from the specified configuration file and returns the configuration as a list.
#'   You can optionally specify a key to retrieve a specific configuration value.
#'
#' - `validate_entrata_config()`: This function validates the Entrata configuration
#'   to ensure that all required fields are present and that they are of the correct
#'   type. If the configuration is invalid, an error will be thrown in the caller
#'   function's caller environment.
#'
#' @details
#' The Entrata API configuration functions are used to retrieve and validate
#' the Entrata API configuration. The configuration is expected to be stored
#' in a YAML configuration file under the `entrata` key. The configuration file
#' should contain the following fields:
#'
#' - `base_url` - The base URL for the Entrata API.
#' - `username` - The username to use for authenticating with the Entrata API.
#' - `password` - The password to use for authenticating with the Entrata API.
#'
#' The configuration file should be stored in the project directory and should
#' be named `config.yml`. The configuration file should have the following
#' structure:
#'
#' ```yaml
#' default:
#'  entrata:
#'    base_url: "https://gmhcommunities.entrata.com/api/v1/"
#'    username: "<Entrata API Username>"
#'    password: "<Entrata API Password>"
#' ```
#'
#' For validation, the following validations are performed on the configuration:
#'
#' 1. The Entrata configuration must contain the following keys:
#'   `username`, `password`, and `base_url`.
#' 2. All required keys must not be empty or `NULL`.
#' 3. The `base_url` key must be a valid `entrata.com` domain URL.
#'
#' To validate the credentials provided in the configuration use the
#' [entrata_status()] function which pings the API to check the status.
#'
#' @param key A character string representing the configuration key to retrieve.
#'   Defaults to `NULL` which returns the full `entrata` specific configuration
#'   object as a list.
#' @param cfg A list containing the Entrata configuration values or a character
#'   string representing the path to the configuration file.
#' @inheritParams config::get
#' @inheritParams rlang::args_error_context
#'
#' @seealso [config::get()] - the primary workhorse for retrieving configuration
#'   values from configuration files.
#'
#' @return
#' - `get_entrata_config()`: Returns a list, or vector (if `key` is specified),
#'   corresponding to the contents of the `entrata` configuration key's values
#'   (i.e. `base_url`, `username`, and `password`).
#'
#' - `validate_entrata_config()`: Returns the validated configuration invisibly.
NULL

# get configuration -------------------------------------------------------

#' @rdname entrata_config
#' @export
#' @importFrom cli cli_abort
#' @importFrom config get
#' @importFrom rlang arg_match
get_entrata_config <- function(
    key = NULL,
    file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
    config = Sys.getenv("R_CONFIG_ACTIVE", "default")
) {

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
  cfg <- tryCatch({
    config::get(
      value = "entrata",
      file = file,
      config = config
    )
  }, error = function(e) {
    cli::cli_abort(
      c(
        "Error reading configuration file {.field {basename(file)}}.",
        "Ensure the file contains an {.code {entrata}} configuration block for ",
        " the {.field {config}} configuration.",
        "Error: {.error_message {e}}"
      )
    )
  })

  keys <- names(cfg)

  if (!is.null(key)) {
    key <- rlang::arg_match(key, keys)
    return(cfg[[key]])
  }

  return(cfg)

}

# validate ----------------------------------------------------------------

#' @rdname entrata_config
#' @export
#' @importFrom rlang caller_env caller_arg
#' @importFrom cli cli_abort
#' @importFrom yaml read_yaml
#' @importFrom purrr pluck
validate_entrata_config <- function(
    cfg,
    arg = rlang::caller_arg(cfg),
    call = rlang::caller_env()
) {

  # copy provided cfg
  cfg_orig <- cfg

  # check if list or file path, and read if necessary
  if (!is.list(cfg) && file.exists(cfg)) {

    # read whole yaml
    cfg <- yaml::read_yaml(cfg)

    # ensure default key is present
    if (!("default" %in% names(cfg))) {

      # throw error condition
      cli::cli_abort(
        c(
          "Invalid Configuration Provided: {.arg {arg}}",
          "Configuration is missing required {.field default} configuration."
        ),
        call = call
      )

    }

    # extract default config values
    default_cfg <- purrr::pluck(cfg, "default")

    # ensure entrata key is present
    if (!("entrata" %in% names(default_cfg))) {
      cli::cli_abort(
        c(
          "Invalid Configuration Provided: {.arg {arg}}",
          "Configuration is missing required {.field entrata} field."
        ),
        call = call
      )
    }

    # replace cfg with entrata values from the file used
    cfg <- default_cfg$entrata
  }

  # if cfg has default, go one level deeper
  if ("default" %in% names(cfg)) {
    cfg <- purrr::pluck(cfg, "default")
  }

  # if cfg has entrata, go one level deeper
  if ("entrata" %in% names(cfg)) {
    cfg <- purrr::pluck(cfg, "entrata")
  }

  # validations -------------------------------------------------------------

  # validate list
  if (!is.list(cfg)) {
    cli::cli_abort(
      c(
        "Invalid Configuration Provided: {.arg {arg}}. ",
        "Configuration must be a list."
      ),
      call = call
    )
  }

  # validate required fields
  req_fields <- c("username", "password", "base_url")
  cfg_fields <- names(cfg)
  missing_fields <- req_fields[!req_fields %in% cfg_fields]
  num_missing_fields <- length(missing_fields)

  if (num_missing_fields > 0) {
    # error message
    msg <- c(
      "Invalid Configuration Provided: {.arg {arg}}. ",
      "Provided configuration is missing required ",
      if (num_missing_fields == 1) {
        paste0("{.field ", missing_fields, "}")
      } else {
        paste0("fields: ", paste0("{.field ", missing_fields, "}", collapse = ", "))
      }
    )

    # throw error
    cli::cli_abort(msg, call = call)
  }

  # return original cfg invisibly
  invisible(cfg_orig)

}


