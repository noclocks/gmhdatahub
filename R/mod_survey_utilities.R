#  ------------------------------------------------------------------------
#
# Title : Survey Utilities Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Utilities Shiny Module
#'
#' @name mod_survey_utilities
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Utilities page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_utilities_ui()`: User Interface (UI) for the module.
#' - `mod_survey_utilities_server()`: Server logic for the module.
#' - `mod_survey_utilities_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_utilities_ui()`: UI output
#' - `mod_survey_utilities_server()`: List of reactive expressions.
#' - `mod_survey_utilities_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_utilities_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_utilities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_utilities_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_utilities
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_utilities_server <- function(
    id,
    pool = NULL,
    selected_property_id = NULL,
    selected_competitor_id = NULL,
    edit_survey_section = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_utilities_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_utilities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_utilities_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Utilities",
    window_title = "Demo: Survey Utilities",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Utilities",
      value = "survey_utilities",
      icon = bsicons::bs_icon("house"),
      mod_survey_utilities_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_utilities_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
