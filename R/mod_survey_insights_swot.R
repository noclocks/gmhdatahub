
#  ------------------------------------------------------------------------
#
# Title : SWOT Analysis Module
#    By : Jimmy Briggs
#  Date : 2024-11-20
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' SWOT Analysis Module
#'
#' @name mod_survey_insights_swot
#'
#' @description
#' This module provides a mechanism for conducting a SWOT analysis in a Shiny
#' App and includes functions for the UI and server:
#'
#' - `mod_survey_insights_swot_ui()`: User interface for the SWOT Analysis Module
#' - `mod_survey_insights_swot_server()`: Server logic for the SWOT Analysis Module
#' - `mod_survey_insights_swot_demo()`: Demo the SWOT Analysis Module
#'
#' @details
#' The SWOT Analysis Module provides a structured approach to evaluating the
#' internal strengths and weaknesses, and external opportunities and threats
#' of a business or project. This module is designed to facilitate the
#' collection and organization of SWOT analysis data for use in strategic
#' planning and decision-making processes.
#'
#' @param id The module ID.
#'
#' @returns
#' - `mod_survey_insights_swot_ui()`: User interface for the SWOT Analysis Module
#' - `mod_survey_insights_swot_server()`: Server logic for the SWOT Analysis Module
#' - `mod_survey_insights_swot_demo()`: Demo the SWOT Analysis Module
#'
#' @seealso
#' [mod_survey_insights()]
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights_swot
#' @export
#' @importFrom bslib nav_panel layout_columns card card_header
#' @importFrom shiny textAreaInput NS
mod_survey_insights_swot <- function(id) {

  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "SWOT Analysis",
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(
        bslib::card_header("Strengths"),
        style = "background-color: #d4edda;",
        shiny::textAreaInput(
          ns("strengths"),
          NULL,
          width = "100%",
          height = "150px",
          value = "1. Prime location\n2. High occupancy rates\n3. Quality amenities"
        )
      ),
      bslib::card(
        bslib::card_header("Weaknesses"),
        style = "background-color: #f8d7da;",
        shiny::textAreaInput(
          ns("weaknesses"),
          NULL,
          width = "100%",
          height = "150px",
          value = "1. Aging infrastructure\n2. Limited parking\n3. Higher operating costs")
      ),
      bslib::card(
        bslib::card_header("Opportunities"),
        style = "background-color: #cce5ff;",
        shiny::textAreaInput(
          ns("opportunities"),
          NULL,
          width = "100%",
          height = "150px",
          value = "1. Market expansion\n2. Renovation potential\n3. New target demographics")
      ),
      bslib::card(
        bslib::card_header("Threats"),
        style = "background-color: #fff3cd;",
        shiny::textAreaInput(
          ns("threats"),
          NULL,
          width = "100%",
          height = "150px",
          value = "1. New competition\n2. Economic downturn\n3. Changing regulations")
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_swot_analysis
#'
#' @export
#'
#' @importFrom shiny moduleServer reactiveValues observe observeEvent updateTextAreaInput
#' @importFrom logger info
mod_survey_insights_swot_server <- function(id, pool = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # reactive values
      rv <- shiny::reactiveValues(
        swot = list(
          strengths = "1. Prime location\n2. High occupancy rates\n3. Quality amenities",
          weaknesses = "1. Aging infrastructure\n2. Limited parking\n3. Higher operating costs",
          opportunities = "1. Market expansion\n2. Renovation potential\n3. New target demographics",
          threats = "1. New competition\n2. Economic downturn\n3. Changing regulations"
        )
      )

      # update reactive values
      shiny::observeEvent(input$strengths, {
        before <- rv$swot$strengths
        rv$swot$strengths <- input$strengths
        after <- rv$swot$strengths
        logger::info("[SWOT Analysis]: Strengths changed from {.field {before}}' to '{.field {after}}'.")
      })

      shiny::observeEvent(input$weaknesses, {
        before <- rv$swot$weaknesses
        rv$swot$weaknesses <- input$weaknesses
        after <- rv$swot$weaknesses
        logger::info("[SWOT Analysis]: Weaknesses changed from {.field {before}}' to '{.field {after}}'.")
      })

      shiny::observeEvent(input$opportunities, {
        before <- rv$swot$opportunities
        rv$swot$opportunities <- input$opportunities
        after <- rv$swot$opportunities
        logger::info("[SWOT Analysis]: Opportunities changed from {.field {before}}' to '{.field {after}}'.")
      })

      shiny::observeEvent(input$threats, {
        before <- rv$swot$threats
        rv$swot$threats <- input$threats
        after <- rv$swot$threats
        logger::info("[SWOT Analysis]: Threats changed from {.field {before}}' to '{.field {after}}'.")
      })

      # render reactive values
      shiny::observe({
        shiny::updateTextAreaInput(
          session,
          session$ns("strengths"),
          value = rv$swot$strengths
        )
      })

      shiny::observe({
        shiny::updateTextAreaInput(
          session,
          session$ns("weaknesses"),
          value = rv$swot$weaknesses
        )
      })

      shiny::observe({
        shiny::updateTextAreaInput(
          session,
          session$ns("opportunities"),
          value = rv$swot$opportunities
        )
      })

      shiny::observe({
        shiny::updateTextAreaInput(
          session,
          session$ns("threats"),
          value = rv$swot$threats
        )
      })

      # return -------------------------------------------------------------
      return(
        list(
          strengths = rv$swot$strengths,
          weaknesses = rv$swot$weaknesses,
          opportunities = rv$swot$opportunities,
          threats = rv$swot$threats
        )
      )
    }
  )

}

mod_survey_insights_swot_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Insights SWOT",
    window_title = "Demo: Survey Insights SWOT",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "SWOT Analysis",
      value = "survey_insights_swot",
      icon = bsicons::bs_icon("house"),
      mod_survey_insights_swot_ui("demo")
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    mod_survey_insights_swot_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)

}

# helpers -----------------------------------------------------------------


#' Observe SWOT Change
#'
#' @description
#' Observe changes to a SWOT analysis field and log the change.
#'
#' @param input_field The input field to observe.
#' @param field_name The name of the field to log.
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom shiny observeEvent
#' @importFrom logger info
#'
#' @examples
#' if (interactive()) {
#'   # in a shiny app's server:
#'   observe_swot_change("strengths", "Strengths")
#'   observe_swot_change("weaknesses", "Weaknesses")
#'   observe_swot_change("opportunities", "Opportunities")
#'   observe_swot_change("threats", "Threats")
#' }
observe_swot_change <- function(input_field, field_name) {

  shiny::observeEvent(input[[input_field]], {
    before <- rv$swot[[field_name]]
    rv$swot[[field_name]] <- input[[input_field]]
    after <- rv$swot[[field_name]]
    logger::info("[SWOT Analysis]: {field_name} changed from '{before}' to '{after}'.")
  })

}
