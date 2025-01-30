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
    app_config = get_app_config(),
    auth_config = get_auth_config(),
    on_start = NULL,
    options = app_opts(port = port, host = host),
    enable_bookmarking = NULL,
    ui_pattern = ".*",
    ...) {
  # Set `noClocksAuthR` API URL conditionally based on environment
  noClocksAuthR:::set_api_url(api_url = auth_config$base_url)

  ui <- noClocksAuthR::secure_ui(
    ui = app_ui,
    sign_in_page_ui = custom_sign_in_ui,
    custom_admin_button_ui = noClocksAuthR::admin_button_ui(align = "left")
  ) |>
    healthcheck_ui()

  server <- noClocksAuthR::secure_server(
    server = app_server,
    custom_sign_in_server = noClocksAuthR::sign_in_module_2
  )

  on_start <- function() {
    noClocksAuthR::noclocksauthr_config(
      app_name = app_config$id,
      api_key = auth_config$api_key,
      is_invite_required = ifelse(Sys.getenv("R_CONFIG_ACTIVE") == "production", TRUE, FALSE),
      is_email_verification_required = FALSE
    )
  }

  # run app
  shiny::shinyApp(
    ui = ui,
    server = server,
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
#'
#' @importFrom cli cli_abort cli_warn cli_alert_warning
#' @importFrom rlang is_named
app_opts <- function(...) {
  user_options <- list(...)
  if (!rlang::is_named(user_options)) {
    cli::cli_abort(
      "All options must be named. For example: {.code app_opts(port = 1234)}."
    )
  }
  valid_options <- user_options[names(user_options) %in% .shiny_opts]
  if (length(valid_options) < length(user_options)) {
    invalid_options <- setdiff(names(user_options), .shiny_opts)
    cli::cli_warn(c(
      "The following options are not valid Shiny options and will be ignored:",
      "{.field {invalid_options}}"
    ))
  }
  names(valid_options) <- paste0("shiny.", names(valid_options))
  current_options <- options()[grep("^shiny\\.", names(options()))]
  current_options_names <- sub("^shiny\\.", "", names(current_options))
  opts_to_override <- intersect(names(valid_options), names(current_options))
  if (length(opts_to_override) > 0) {
    cli::cli_alert_warning(c(
      "The following Shiny options are already set and will be overridden:",
      "{.field {opts_to_override}}"
    ))
  }
  do.call(options, valid_options)
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
