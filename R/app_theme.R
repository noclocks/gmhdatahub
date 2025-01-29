
#  ------------------------------------------------------------------------
#
# Title : App Assets
#    By : Jimmy Briggs
#  Date : 2025-01-27
#
#  ------------------------------------------------------------------------

#' GMH Colors
#'
#' @param ... A list of colors to return.
#'
#' @returns
#' A list of GMH colors.
#'
#' @export
gmh_colors <- function(...) {
  colors <- c(
    primary = "#0e2b4c",
    secondary = "#6c757d",
    success = "#28a745",
    danger = "#dc3545",
    warning = "#ffc107",
    info = "#17a2b8",
    light = "#f8f9fa",
    dark = "#343a40",
    white = "#ffffff",
    black = "#000000",
    gray = "#6c757d"
  )
  dots <- list(...)
  if (length(dots) == 0) {
    return(colors)
  } else {
    requested_colors <- unlist(dots)
    return(unname(unlist(colors[requested_colors])))
  }
}

#' App Theme
#'
#' @description
#' This function defines the theme for the Shiny application.
#'
#' @returns
#' The theme for the Shiny application.
#'
#' @export
#'
#' @importFrom bslib bs_theme
app_theme_ui <- function() {
  bslib::bs_theme(
    version = 5,
    primary = gmh_colors("primary"),
    "enable-shadows" = TRUE
  )
}
