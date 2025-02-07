#  ------------------------------------------------------------------------
#
# Title : Survey Rents Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Rents Shiny Module
#'
#' @name mod_survey_rents
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Rents page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_rents_ui()`: User Interface (UI) for the module.
#' - `mod_survey_rents_server()`: Server logic for the module.
#' - `mod_survey_rents_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_rents_ui()`: UI output
#' - `mod_survey_rents_server()`: List of reactive expressions.
#' - `mod_survey_rents_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_rents_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_rents
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_rents_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::card(
        class = "mx-auto mb-4",
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h2(
            bsicons::bs_icon("house"),
            "Rents by Floorplan",
            shiny::actionButton(
              ns("refresh"),
              "Refresh Data",
              icon = shiny::icon("sync"),
              class = "btn-sm btn-outline-light float-end",
              style = "width: auto;"
            )
          ),
          htmltools::tags$p(
            class = "lead mb-0",
            "View rents by floorplan."
          )
        ),
        bslib::card_body(
          reactable::reactableOutput(
            ns("rents_by_floorplan")
          )
        ),
        bslib::card_footer(
          shiny::textOutput(ns("rents_by_floorplan_last_updated"))
        )
      ),
      bslib::card(
        class = "mx-auto mb-4",
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h2(
            bsicons::bs_icon("house"),
            "Average Rents by Unit Type",
            class = "mb-2"
          ),
          htmltools::tags$p(
            class = "lead mb-0",
            "View average rents by unit type."
          )
        ),
        bslib::card_body(
          reactable::reactableOutput(
            ns("avg_rents_by_unit_type")
          )
        ),
        bslib::card_footer(
          shiny::textOutput(ns("avg_rents_last_updated"))
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_rents
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_rents_server <- function(
    id,
    pool = NULL,
    survey_data = NULL,
    selected_filters = NULL,
    db_trigger_func = NULL,
    edit_survey_section = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # setup ------------------------------------------------------------
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_rents_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      # filters -----------------------------------------------------------------
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------
      initial_data <- shiny::reactiveVal(0)
      input_changes <- shiny::reactiveVal(0)

      # selected property/competitor ID
      current_id <- shiny::reactive({
        selected_property_id()
      })
      current_name <- shiny::reactive({
        get_property_name_by_id(selected_property_id())
      })

      # rents by floorplan data
      rents_data <- shiny::reactive({
        shiny::req(pool, current_id())

        db_read_tbl(pool, "survey.rents_by_floorplan") |>
          dplyr::filter(.data$property_name == current_name())
      })

      # output - rents by floorplan
      output$rents_by_floorplan <- reactable::renderReactable({
        shiny::req(rents_data())
        tbl_rents_by_floorplan(rents_data())
      })

      # output - average rents by unit type
      output$avg_rents_by_unit_type <- reactable::renderReactable({
        shiny::req(rents_data())
        tbl_avg_rents_by_unit_type(rents_data())
      })

      # return reactive values
      return(
        list()
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_rents
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_rents_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Rents",
    window_title = "Demo: Survey Rents",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Rents",
      value = "survey_rents",
      icon = bsicons::bs_icon("house"),
      mod_survey_rents_ui("demo"),
      shiny::actionButton(
        "edit_survey_section",
        "Edit",
        icon = shiny::icon("edit"),
        style = "width: auto;",
        class = "btn-sm btn-primary"
      )
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    edit_survey_section <- shiny::reactive({
      input$edit_survey_section
    })
    mod_survey_rents_server(
      "demo",
      pool = pool,
      edit_survey_section = edit_survey_section
    )
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
