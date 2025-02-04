#  ------------------------------------------------------------------------
#
# Title : Survey Insights Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Insights Shiny Module
#'
#' @name mod_survey_insights
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Insights page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_insights_ui()`: User Interface (UI) for the module.
#' - `mod_survey_insights_server()`: Server logic for the module.
#' - `mod_survey_insights_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_insights_ui()`: UI output
#' - `mod_survey_insights_server()`: List of reactive expressions.
#' - `mod_survey_insights_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_insights_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_insights_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::navset_card_tab(
        id = ns("nav"),
        title = htmltools::tags$span(
          bsicons::bs_icon("bar-chart"),
          "Survey Insights"
        ),
        sidebar = bslib::sidebar(
          title = icon_text("filter", "Filters"),
          width = 300#,
          # shiny::selectizeInput(
          #   ns("property"),
          #   label = icon_text("building", "Property"),
          #   choices = app_choices_lst$properties,
          #   selected = app_choices_lst$properties[["1047 Commonwealth Avenue"]]
          # ),
          # shiny::selectizeInput(
          #   ns("competitor"),
          #   label = icon_text("building", "Competitor"),
          #   choices = c("None" = "none")
          # ) |>
          #   shinyjs::disabled(),
          # shiny::dateInput(
          #   ns("leasing_week"),
          #   label = icon_text("calendar", "Leasing Week"),
          #   value = current_week_start,
          #   weekstart = 1,
          #   daysofweekdisabled = c(0, 2:6)
          # )
        ),
        bslib::nav_panel(
          title = "Trends",
          value = ns("nav_trends"),
          mod_survey_insights_trends_ui(ns("trends"))
        ),
        bslib::nav_panel(
          title = "Comparison",
          value = ns("nav_comparison"),
          mod_survey_insights_comparison_ui(ns("comparison"))
        ),
        bslib::nav_panel(
          title = "SWOT Analysis",
          value = ns("nav_swot"),
          mod_survey_insights_swot_ui(ns("swot"))
        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_survey_insights
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_insights_server <- function(
    id,
    pool = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_insights_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # modules
      trends_data <- mod_survey_insights_trends_server("trends", pool = pool)
      comparison_data <- mod_survey_insights_comparison_server("comparison", pool = pool)
      swot_data <- mod_survey_insights_swot_server("swot", pool = pool)

      return(
        list(
          trends = trends_data,
          comparison = comparison_data,
          swot = swot_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_insights
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_insights_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Insights",
    window_title = "Demo: Survey Insights",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Insights",
      value = "survey_insights",
      icon = bsicons::bs_icon("house"),
      mod_survey_insights_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_insights_server("demo")
  }

  shiny::shinyApp(ui, server)
}
