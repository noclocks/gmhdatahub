
#  ------------------------------------------------------------------------
#
# Title : Properties Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Properties Shiny Module
#'
#' @name mod_properties
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Properties page.
#'
#' Includes the following functions:
#'
#' - `mod_properties_ui()`: User Interface (UI) for the module.
#' - `mod_properties_server()`: Server logic for the module.
#' - `mod_properties_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_properties_ui()`: UI output
#' - `mod_properties_server()`: List of reactive expressions.
#' - `mod_properties_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_properties_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_properties
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_properties_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(

    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_properties
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_properties_server <- function(
  id,
  pool = NULL,
  global_filters = NULL
) {

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
      cli::cat_rule("[Module]: mod_properties_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_properties
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_properties_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Properties",
    window_title = "Demo: Properties",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Properties",
      value = "properties",
      icon = bsicons::bs_icon("house"),
      mod_properties_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_properties_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------

