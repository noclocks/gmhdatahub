#  ------------------------------------------------------------------------
#
# Title : Performance Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Performance Shiny Module
#'
#' @name mod_performance
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Performance page.
#'
#' Includes the following functions:
#'
#' - `mod_performance_ui()`: User Interface (UI) for the module.
#' - `mod_performance_server()`: Server logic for the module.
#' - `mod_performance_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_performance_ui()`: UI output
#' - `mod_performance_server()`: List of reactive expressions.
#' - `mod_performance_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_performance_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_performance
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_performance_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_performance
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_performance_server <- function(
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
      cli::cat_rule("[Module]: mod_performance_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_performance
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_performance_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Performance",
    window_title = "Demo: Performance",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Performance",
      value = "performance",
      icon = bsicons::bs_icon("house"),
      mod_performance_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_performance_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
