#  ------------------------------------------------------------------------
#
# Title : SWOT Analysis Module
#    By : Jimmy Briggs
#  Date : 2024-11-20
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Survey Insights Comparison Module
#'
#' @name mod_survey_insights_comparison
#'
#' @description
#' This module provides comparison features related to the market survey data.
#'
#' The following functions are implemented:
#'
#' - `mod_survey_insights_comparison_ui()`: User interface for the SWOT Analysis Module
#' - `mod_survey_insights_comparison_server()`: Server logic for the SWOT Analysis Module
#' - `mod_survey_insights_comparison_demo()`: Demo the SWOT Analysis Module
#'
#' @param id The module ID.
#'
#' @returns
#' - `mod_survey_insights_comparison_ui()`: User interface for the module
#' - `mod_survey_insights_comparison_server()`: Server logic for the module
#' - `mod_survey_insights_comparison_demo()`: Demo the module
#'
#' @seealso
#' [mod_survey_insights()]
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights_comparison
#' @export
#' @importFrom bslib nav_panel layout_columns card card_header
mod_survey_insights_comparison_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::layout_columns(
        bslib::card(

        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_survey_insights_comparison
#' @export
#' @importFrom shiny moduleServer reactiveValues observe observeEvent
mod_survey_insights_comparison_server <- function(id, pool = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cli_rule("[Module]: mod_survey_insights_comparison_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      return(
        list(

        )
      )

    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_insights_comparison
#' @export
#' @importFrom shiny shinyApp
mod_survey_insights_comparison_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_survey_insights_comparison_ui("demo")
  )

  server <- function(input, output, session) {
    mod_survey_insights_comparison_server("demo")
  }

  shiny::shinyApp(ui, server)

}
