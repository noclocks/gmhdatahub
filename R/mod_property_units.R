#  ------------------------------------------------------------------------
#
# Title : Property Units Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Property Units Shiny Module
#'
#' @name mod_property_units
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Property Units page.
#'
#' Includes the following functions:
#'
#' - `mod_property_units_ui()`: User Interface (UI) for the module.
#' - `mod_property_units_server()`: Server logic for the module.
#' - `mod_property_units_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_property_units_ui()`: UI output
#' - `mod_property_units_server()`: List of reactive expressions.
#' - `mod_property_units_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_property_units_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_property_units
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_property_units_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_property_units
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_property_units_server <- function(
    id,
    pool = NULL,
    global_filters = NULL) {
  # check database connection
  if (is.null(pool)) pool <- db_connect()
  check_db_conn(pool)

  # validation of reactives
  if (!is.null(global_filters)) {
    stopifnot(shiny::is.reactive(global_filters))
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_property_units_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_property_units
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_property_units_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Property Units",
    window_title = "Demo: Property Units",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Property Units",
      value = "property_units",
      icon = bsicons::bs_icon("house"),
      mod_property_units_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_property_units_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
