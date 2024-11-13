
#  ------------------------------------------------------------------------
#
# Title : Home (Page) Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Home (Page) Shiny Module
#'
#' @name mod_home
#'
#' @description
#' A Shiny module for creating the home page tab of the GMH Data Hub's Leasing
#' Dashboard.
#'
#' This module includes the UI and Server functions:
#' - `mod_home_ui()`
#' - `mod_home_server()`
#'
#' @param id The module id.
#'
#' @return
#' - `mod_home_ui()`: The UI HTML wrapped in a [htmltools::tagList()].
#' - `mod_home_server()`: A shiny server function.
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_home
#' @export
#' @importFrom htmltools tags tagList
#' @importFrom bslib card card_body card_title layout_columns layout_column_wrap
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS textOutput uiOutput
mod_home_ui <- function(id, ...) {

  ns <- shiny::NS(id)

  # value boxes -------------------------------------------------------------
  htmltools::tagList(
    bslib::layout_column_wrap(
      width = 1/3,
      heights_equal = "all",
      bslib::value_box(
        id = ns("properties_value_box"),
        title = "Properties",
        value = shiny::textOutput(ns("properties_value_box"), inline = TRUE),
        shiny::uiOutput(ns("properties_value_box_details")),
        showcase = bsicons::bs_icon("building-check"),
        showcase_layout = "left center",
        full_screen = TRUE,
        theme = "info",
        height = NULL,
        max_height = NULL,
        min_height = NULL,
        fill = TRUE,
        class = NULL
      ),
      bslib::value_box(
        id = ns("leases_value_box"),
        title = "Leases",
        value = shiny::textOutput(ns("leases_value_box"), inline = TRUE),
        shiny::uiOutput(ns("leases_value_box_details")),
        showcase = bsicons::bs_icon("file-earmark-text-fill"),
        showcase_layout = "left center",
        full_screen = TRUE,
        theme = "success",
        height = NULL,
        max_height = NULL,
        min_height = NULL,
        fill = TRUE,
        class = NULL
      ),
      bslib::value_box(
        id = ns("rate_value_box"),
        title = "Occupancy Rate",
        value = shiny::textOutput(ns("rate_value_box"), inline = TRUE),
        shiny::uiOutput(ns("rate_value_box_details")),
        showcase = bsicons::bs_icon("house-check-fill"),
        showcase_layout = "left center",
        full_screen = TRUE,
        theme = "primary",
        height = NULL,
        max_height = NULL,
        min_height = NULL,
        fill = TRUE,
        class = NULL
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(bs_icon("graph-up"), "Key Metrics"),
        bslib::card_body(
          "This is where you would display additional metrics or charts."
        ),
        full_screen = TRUE
      ),
      bslib::card(
        bslib::card_header(bs_icon("clock-history"), "Recent Activity"),
        bslib::card_body(
          "This section could show recent updates or activities."
        ),
        full_screen = TRUE
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(bs_icon("calendar2-check"), "Upcoming Events"),
        bslib::card_body(
          "This section could show upcoming events or important dates."
        ),
        full_screen = TRUE
      ),
      bslib::card(
        bslib::card_header(bs_icon("chat-dots"), "Messages"),
        bslib::card_body(
          "This section could show recent messages or notifications."
        ),
        full_screen = TRUE
      )
    )
  )

}


# Server ------------------------------------------------------------------

#' @rdname mod_home
#' @export
#'
mod_home_server <- function(id, filters, data, user) {
  shiny::server <- function(input, output, session) {

    ns <- session$ns

    # reactive values -------------------------------------------------------
    properties <- shiny::reactive({
      shiny::req(input$properties)
      shiny::req(input$leases)
      shiny::req(input$rate)

      list(
        properties = input$properties,
        leases = input$leases,
        rate = input$rate
      )
    })

  }
}
