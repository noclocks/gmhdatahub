#  ------------------------------------------------------------------------
#
# Title : Survey Fees Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Fees Shiny Module
#'
#' @name mod_survey_fees
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Fees page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_fees_ui()`: User Interface (UI) for the module.
#' - `mod_survey_fees_server()`: Server logic for the module.
#' - `mod_survey_fees_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_fees_ui()`: UI output
#' - `mod_survey_fees_server()`: List of reactive expressions.
#' - `mod_survey_fees_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_fees_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_fees
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_fees_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_fees
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_fees_server <- function(
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
      cli::cat_rule("[Module]: mod_survey_fees_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_fees
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_fees_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Fees",
    window_title = "Demo: Survey Fees",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Fees",
      value = "survey_fees",
      icon = bsicons::bs_icon("house"),
      mod_survey_fees_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_fees_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
