#  ------------------------------------------------------------------------
#
# Title : Floorplans Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Floorplans Shiny Module
#'
#' @name mod_floorplans
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Floorplans page.
#'
#' Includes the following functions:
#'
#' - `mod_floorplans_ui()`: User Interface (UI) for the module.
#' - `mod_floorplans_server()`: Server logic for the module.
#' - `mod_floorplans_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_floorplans_ui()`: UI output
#' - `mod_floorplans_server()`: List of reactive expressions.
#' - `mod_floorplans_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_floorplans_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_floorplans
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_floorplans_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_floorplans
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_floorplans_server <- function(
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
      cli::cat_rule("[Module]: mod_floorplans_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_floorplans
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_floorplans_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Floorplans",
    window_title = "Demo: Floorplans",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Floorplans",
      value = "floorplans",
      icon = bsicons::bs_icon("house"),
      mod_floorplans_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_floorplans_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
