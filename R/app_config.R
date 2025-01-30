#  ------------------------------------------------------------------------
#
# Title : App Configuration
#    By : Jimmy Briggs
#  Date : 2025-01-28
#
#  ------------------------------------------------------------------------

get_app_config <- function(
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
        value = "app",
        file = file,
        config = config
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Error reading configuration file {.field {basename(file)}}.",
          "Ensure the file contains a {.code app} configuration block for ",
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

get_auth_config <- function(
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
        value = "auth",
        file = file,
        config = config
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Error reading configuration file {.field {basename(file)}}.",
          "Ensure the file contains a {.code auth} configuration block for ",
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



# app info ----------------------------------------------------------------

#' App Information
#'
#' @description
#' This function provides information about the app.
#'
#' @param ... The information to retrieve.
#'
#' @returns
#' The requested information.
#'
#' @export
#'
#' @examples
#' app_info()
app_info <- function(...) {
  info <- list(
    name = "gmhdatahub",
    title = "GMH Data Hub",
    company = "GMH Communities",
    version = "1.0",
    logo = "www/gmh-logo.svg",
    symbol = "www/gmh-icon.png",
    repo_url = "https://github.com/noclocks/gmhdatahub",
    docs_url = "https://docs.noclocks.dev/gmhdatahub"
  )

  # Return the requested info
  if (missing(...)) {
    return(info)
  } else {
    return(info[[...]])
  }
}
