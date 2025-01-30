#  ------------------------------------------------------------------------
#
# Title : Survey Amenities Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Amenities Shiny Module
#'
#' @name mod_survey_unit_amenities
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Amenities page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_unit_amenities_ui()`: User Interface (UI) for the module.
#' - `mod_survey_unit_amenities_server()`: Server logic for the module.
#' - `mod_survey_unit_amenities_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_unit_amenities_ui()`: UI output
#' - `mod_survey_unit_amenities_server()`: List of reactive expressions.
#' - `mod_survey_unit_amenities_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_unit_amenities_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_unit_amenities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_unit_amenities_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      reactable::reactableOutput(ns('survey_unit_amenities_tbl'))
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_unit_amenities
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_unit_amenities_server <- function(
    id,
    pool = NULL,
    selected_property_id = NULL,
    edit_survey_section = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_unit_amenities_server()")

      # check database connection
      if (is.null(pool)) pool <- db_connect()
      check_db_conn(pool)

      # handle selected property ID
      if (is.null(selected_property_id)) {
        # property_id <- db_read_tbl(pool, "mkt.properties", collect = FALSE) |>
        #   dplyr::filter(.data$is_competitor == FALSE) |>
        #   dplyr::pull("property_id")
        selected_property_id <- shiny::reactive({
          # property_id
          session$userData$selected_survey_property()
        })
      }

      db_refresh_trigger <- shiny::reactiveVal(0)

      unit_amentities_data <- shiny::reactive({
        shiny::req(pool, selected_property_id(), session$userData$leasing_week())

        # browser()

        db_read_mkt_unit_amenities(
          pool,
          property_id = selected_property_id(),
          leasing_week = session$userData$leasing_week()
        )
      })

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_unit_amenities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_unit_amenities_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Amenities",
    window_title = "Demo: Survey Amenities",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Amenities",
      value = "survey_unit_amenities",
      icon = bsicons::bs_icon("house"),
      mod_survey_unit_amenities_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_unit_amenities_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
