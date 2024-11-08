
#  ------------------------------------------------------------------------
#
# Title : App Sidebar Module
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

.app_choices <- list(
  properties = c("Property 1", "Property 2", "Property 3", "Property 4")
)

# topic -------------------------------------------------------------------

#' App Sidebar Module
#'
#' @name mod_sidebar
#'
#' @description
#' This module creates a sidebar for the app that contains filters, options, and about information.
#'
#' It includes the UI and server functions:
#'
#' - `mod_sidebar_ui()`
#' - `mod_sidebar_server()`
#'
#' @param id The module id.
#' @param ... Additional parameters.
#'
#' @return
#' - `mod_sidebar_ui()`: A shiny UI function.
#' - `mod_sidebar_server()`: A shiny server function.


# UI ----------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib sidebar accordion accordion_panel
#' @importFrom htmltools tags
#' @importFrom shiny NS
#' @importFrom shinyWidgets pickerInput
mod_sidebar_ui <- function(id, ...) {

  ns <- shiny::NS(id)

  bslib::sidebar(
    htmltools::tags$div(
      class = "sidebar-title",
      htmltools::tags$h5("Sidebar"),
      htmltools::tags$p("Use the sidebar for filtering and other controls.")
    ),
    bslib::accordion(
      id = ns("accordion"),
      bslib::accordion_panel(
        title = "Filters",
        value = ns("filters"),
        icon = bsicons::bs_icon("filter"),
        htmltools::tags$div(
          class = "text-center",
          shinyWidgets::pickerInput(
            ns("properties"),
            label = icon_text("building", "Select Properties"),
            choices = .app_choices$properties,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `live-search-normalize` = TRUE,
              `live-search-placeholder` = 'Search...',
              `selected-text-format` = 'count > 3',
              `count-selected-text` = 'Properties Selected'
            )
          )
        ),
        bslib::accordion_panel(
          title = "Options",
          value = ns("options"),
          icon = bsicons::bs_icon("gear"),
          htmltools::tags$div(
            class = "text-center"#,
            # TODO: Add options here
          )
        )
      ),
      bslib::accordion_panel(
        title = "About",
        value = ns("about"),
        icon = bsicons::bs_icon("info"),
        htmltools::tags$div(
          class = "text-center",
          htmltools::tags$p("App Version: 0.0.1", align = "center"),
          htmltools::tags$p("Author: No Clocks, LLC", align = "center"),
          htmltools::tags$p("Date: 2024-11-08", align = "center")
        )
      )
    )
  )

}

# server ------------------------------------------------------------------

#' @rdname mod_sidebar
#' @export
#' @importFrom shiny moduleServer reactive req
mod_sidebar_server <- function(id, ...) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Server Logic
      out <- shiny::reactive({
        shiny::req(input$properties)
        list(
          properties = input$properties
        )
      })

      # Return Reactive Values with Options and Filters
      return(out)

    }
  )
}
