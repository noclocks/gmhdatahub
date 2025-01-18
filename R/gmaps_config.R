get_gmaps_config <- function(
    key = NULL,
    file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
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
        value = "gmaps",
        file = file,
        config = config
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Error reading configuration file {.field {basename(file)}}.",
          "Ensure the file contains an {.code {gmaps}} configuration block for ",
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
