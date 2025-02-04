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
    bslib::card(
      reactable::reactableOutput(ns('survey_utilities_tbl')),
      reactable::reactableOutput(ns('survey_other_utilities_tbl'))
    )
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

      # handle selected property ID
      if (is.null(selected_property_id)) {
        selected_property_id <- shiny::reactive({ get_property_id_by_name("1047 Commonwealth Avenue") })
      }

      db_refresh_trigger <- shiny::reactiveVal(0)

      utilities_data <- shiny::reactive({
        shiny::req(pool, selected_property_id())

        db_read_tbl(pool, "survey.utilities") |>
          dplyr::filter(property_id == selected_property_id()) |>
          dplyr::select(
            property_name,
            utility_name,
            utility_category,
            utility_per,
            utility_available,
            utility_capped,
            utility_allowance
          )
      })

      output$survey_utilities_tbl <- reactable::renderReactable({
        data <- utilities_data()

        if (nrow(data) == 0) {
          data <- tibble::tibble(
            property_name = "No Data Available",
            utility_name = NA_character_,
            utility_category = NA_character_,
            utility_per = NA_character_,
            utility_available = NA_character_,
            utility_capped = NA_character_,
            utility_allowance = NA_real_
          )
        }

        reactable::reactable(
          data = data,
          defaultPageSize = nrow(data),
          searchable = TRUE,
          columns = list(
            property_name = reactable::colDef(
              show = FALSE
            ),
            utility_name = reactable::colDef(
              name = "Utility",
            ),
            utility_category = reactable::colDef(
              show = FALSE
            ),
            utility_per = reactable::colDef(
              name = "Per Bed/Unit"
            ),
            utility_available = reactable::colDef(
              name = "Available?"
            ),
            utility_capped = reactable::colDef(
              name = "Capped?"
            ),
            utility_allowance = reactable::colDef(
              name = "Allowance ($)",
              format = reactable::colFormat(prefix = "$", digits = 0, separators = TRUE)
            )
          ),
          highlight = TRUE
        )
      })

      return(
        list(
          utilities_data = utilities_data
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
    theme = app_theme_ui(),
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
