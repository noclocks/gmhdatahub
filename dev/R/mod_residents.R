#  ------------------------------------------------------------------------
#
# Title : Residents Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Residents Shiny Module
#'
#' @name mod_residents
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Residents page.
#'
#' Includes the following functions:
#'
#' - `mod_residents_ui()`: User Interface (UI) for the module.
#' - `mod_residents_server()`: Server logic for the module.
#' - `mod_residents_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_residents_ui()`: UI output
#' - `mod_residents_server()`: List of reactive expressions.
#' - `mod_residents_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_residents_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_residents
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_residents_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_residents
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_residents_server <- function(
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
      cli::cat_rule("[Module]: mod_residents_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_residents
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_residents_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Residents",
    window_title = "Demo: Residents",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Residents",
      value = "residents",
      icon = bsicons::bs_icon("house"),
      mod_residents_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_residents_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
