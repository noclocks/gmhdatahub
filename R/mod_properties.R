
#  ------------------------------------------------------------------------
#
# Title : Properties Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Properties Shiny Module
#'
#' @name mod_properties
#'
#' @description
#' This is the shiny module for the properties page/tab in the shiny app.
#'
#' It includes the UI and server functions:
#' - `mod_properties_ui()`
#' - `mod_properties_server()`
#'
#' The module includes the following children modules for its major functional
#' areas:
#' - [mod_property_tbl] - For listing/displaying all properties.
#' - [mod_property_map] - For displaying a map of all properties.
#' - [mod_property_details] - For displaying the details of a single property.
#'
#' @param id The module id.
#' @param ... Additional parameters.
#'
#' @return
#' - `mod_properties_ui()`: Shiny UI.
#' - `mod_properties_server()`: Shiny Server.
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_properties
#' @export
mod_properties_ui <- function(id, ...) {

  ns <- shiny::NS(id)

  shiny::tagList(
    mod_title_ui(id),
    mod_property_tbl_ui(ns("property_tbl")),
    mod_property_map_ui(ns("property_map")),
    mod_property_details_ui(ns("property_details"))
  )

}
