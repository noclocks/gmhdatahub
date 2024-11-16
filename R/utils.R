
#  ------------------------------------------------------------------------
#
# Title : Utilities
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------


# package utilities -------------------------------------------------------

#' Package System File
#'
#' @name pkg_sys
#'
#' @description
#' This function is a wrapper for the `system.file` function. It is used to
#' retrieve the path to a file within the package directory.
#'
#' @param ... A character vector of subdirectories and file name.
#'
#' @return A character string of the file path.
#'
#' @export
#'
#' @examples
#' pkg_sys("www", "styles", "css", "styles.min.css")
#' pkg_sys("config", "config.yml")
#' pkg_sys("extdata")
pkg_sys <- function(...) {
  system.file(..., package = "gmhdatahub")
}

#' @rdname pkg_sys
#' @export
pkg_sys_assets <- function(...) {
  pkg_sys("www", ...)
}

#' @rdname pkg_sys
#' @export
pkg_sys_config <- function(...) {
  pkg_sys("config", ...)
}

#' @rdname pkg_sys
#' @export
pkg_sys_templates <- function(...) {
  pkg_sys("templates", ...)
}

#' @rdname pkg_sys
#' @export
pkg_sys_examples <- function(...) {
  pkg_sys("examples", ...)
}


# shiny utilities ---------------------------------------------------------

#' Icon Text
#'
#' @description
#' This function is used to create a simple icon and text element.
#'
#' @param icon The icon to display. This can be a character string or an icon
#'   object.
#' @param text The text to display next to the icon.
#' @param .function The function to use to create the icon. Default is `shiny::icon`.
#'
#' @return A `tagList` object containing the icon and text.
#'
#' @export
#'
#' @examples
#' icon_text("user", "User")
#' icon_text(shiny::icon("user"), "User")
#'
#' @importFrom shiny icon
#' @importFrom htmltools tagList tags
icon_text <- function(icon, text, .function = shiny::icon) {
  if (is.character(icon)) i <- .function(icon) else i <- icon
  t <- paste0(" ", text)
  htmltools::tagList(htmltools::tags$div(i, t))
}

# GIS utilities -----------------------------------------------------------

# get coordinates from address:

geocode_address <- function(
  address,
  gmaps_api_key = get_gmaps_api_key()
) {

  # ensure has gmaps api key registered
  if (!ggmap::has_google_key()) {
    if (is.null(gmaps_api_key)) {
      cli::cli_abort("No Google Maps API key provided.")
    }
    ggmap::register_google(key = gmaps_api_key)
  }

  # geocode address
  ggmap::geocode(address)

}

