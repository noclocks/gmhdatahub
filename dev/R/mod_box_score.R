#  ------------------------------------------------------------------------
#
# Title : Box Score Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Box Score Shiny Module
#'
#' @name mod_box_score
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Box Score page.
#'
#' Includes the following functions:
#'
#' - `mod_box_score_ui()`: User Interface (UI) for the module.
#' - `mod_box_score_server()`: Server logic for the module.
#' - `mod_box_score_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_box_score_ui()`: UI output
#' - `mod_box_score_server()`: List of reactive expressions.
#' - `mod_box_score_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_box_score_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_box_score
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_box_score_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_box_score
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_box_score_server <- function(
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
      cli::cat_rule("[Module]: mod_box_score_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_box_score
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_box_score_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Box Score",
    window_title = "Demo: Box Score",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Box Score",
      value = "box_score",
      icon = bsicons::bs_icon("house"),
      mod_box_score_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_box_score_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
