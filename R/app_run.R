
#  ------------------------------------------------------------------------
#
# Title : Run Shiny App
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

#' Run the Shiny Application
#'
#' @description
#' Run the Shiny Application.
#'
#' @param ui,server The UI and server functions.
#' @param port,host The port and host to run the app on.
#' @param on_start A function to run when the app starts.
#' @param options A list of options to pass to the Shiny app.
#' @param enable_bookmarking Enable bookmarking.
#' @param ui_pattern A regular expression to match the UI.
#' @param ... Additional arguments to pass to `shiny::shinyApp`.
#'
#' @export
#'
#' @returns
#' Runs the shiny app.
#'
#' @importFrom shiny shinyApp
run_app <- function(
  port = 8080,
  host = "0.0.0.0",
  on_start = NULL,
  options = app_opts(port = port, host = host),
  enable_bookmarking = NULL,
  ui_pattern = ".*",
  ...
) {
  # run app
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = on_start,
    options = options,
    enableBookmarking = enable_bookmarking,
    uiPattern = ui_pattern,
    ...
  )
}

#' Set Shiny App Options
#'
#' @description
#' Set Shiny App Options.
#'
#' @param ... Named options to set.
#'
#' @export
#'
#' @returns
#' Returns previously set options invisibly.
app_opts <- function(...) {

  # collect user options
  user_options <- list(...)

  # ensure options are named
  if (!rlang::is_named(user_options)) {
    cli::cli_abort(
      "All options must be named. For example: {.code app_opts(port = 1234)}."
    )
  }

  # filter out invalid options
  valid_options <- user_options[names(user_options) %in% .shiny_opts]
  if (length(valid_options) < length(user_options)) {
    invalid_options <- setdiff(names(user_options), .shiny_opts)
    cli::cli_warn(c(
      "The following options are not valid Shiny options and will be ignored:",
      "{.field {invalid_options}}"
    ))
  }
  names(valid_options) <- paste0("shiny.", names(valid_options))

  # get current options
  current_options <- options()[grep("^shiny\\.", names(options()))]
  current_options_names <- sub("^shiny\\.", "", names(current_options))

  opts_to_override <- intersect(names(valid_options), names(current_options))

  if (length(opts_to_override) > 0) {
    cli::cli_alert_warning(c(
      "The following Shiny options are already set and will be overridden:",
      "{.field {opts_to_override}}"
    ))
  }

  # set options
  do.call(options, valid_options)

  # invisibly return any previously set options
  invisible(current_options)
}

.shiny_opts <- c(
  "autoreload",
  "autoreload.interval",
  "autoreload.pattern",
  "bookmarkStore",
  "customMessageHandlers",
  "defaultBrowserHeight",
  "defaultBrowserWidth",
  "deprecation.messages",
  "devmode",
  "disconnected",
  "error",
  "fullstacktrace",
  "host",
  "http.headers",
  "json.digits",
  "launch.browser",
  "maxRequestSize",
  "minified",
  "port",
  "reactlog",
  "sanitize.errors",
  "stacktraceoffset",
  "suppressMissingContextError",
  "table.class",
  "testmode",
  "trace",
  "usecairo"
)
