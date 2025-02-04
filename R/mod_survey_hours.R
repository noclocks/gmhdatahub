#  ------------------------------------------------------------------------
#
# Title : Survey Hours Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-30
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Hours Shiny Module
#'
#' @name mod_survey_hours
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Hours page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_hours_ui()`: User Interface (UI) for the module.
#' - `mod_survey_hours_server()`: Server logic for the module.
#' - `mod_survey_hours_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_hours_ui()`: UI output
#' - `mod_survey_hours_server()`: List of reactive expressions.
#' - `mod_survey_hours_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_hours_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_hours
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_hours_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::card(
        bslib::card_header("Hours of Operation"),
        bslib::card_body(
          rhandsontable::rHandsontableOutput(
            ns("hours_table")
          ) |>
            with_loader()
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_hours
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_hours_server <- function(
    id,
    pool = NULL,
    selected_property_id = NULL,
    selected_competitor_id = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_hours_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # handle selected property ID
      if (is.null(selected_property_id)) {
        prop_id <- get_property_id_by_name("1047 Commonwealth Avenue")
        selected_property_id <- shiny::reactive({
          prop_id
        })
      }

      selected_property_name <- shiny::reactive({
        get_property_name_by_id(prop_id)
      })

      # handle selected competitor ID
      if (is.null(selected_competitor_id)) {
        selected_competitor_id <- shiny::reactive({
          "none"
        })
      } else if (shiny::is.reactive(selected_competitor_id)) {
        if (selected_competitor_id() == "none") {
          selected_competitor_id <- shiny::reactive({
            "none"
          })
        }
      }

      # initialize reactives
      initial_data <- shiny::reactiveVal()
      input_changes <- shiny::reactiveVal(0)
      db_refresh_trigger <- shiny::reactiveVal(0)

      # initial data
      hours_data <- shiny::reactive({
        shiny::req(pool)
        db_read_survey_hours_tbl(pool, selected_property_name()) |>
          dplyr::select(
            day_of_week,
            open_time,
            close_time
          ) |>
          dplyr::mutate(
            open_time = format(open_time, "%H:%M"),
            close_time = format(close_time, "%H:%M")
          )
      })

      # render hours table
      output$hours_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          hours_data(),
          # colHeaders = c("Day of Week", "Open Time", "Close Time"),
          rowHeaders = NULL,
          comments = TRUE,
          useTypes = TRUE,
          width = "100%",
          height = 500
        ) |>
          rhandsontable::hot_col("day_of_week", readOnly = TRUE) |>
          rhandsontable::hot_col("open_time", type = "date", format = "HH:mm", allowInvalid = FALSE) |>
          rhandsontable::hot_col("close_time", type = "date", format = "HH:mm", allowInvalid = FALSE) |>
          rhandsontable::hot_table(
            highlightCol = TRUE,
            highlightRow = TRUE
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

#' @rdname mod_survey_hours
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_hours_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Hours",
    window_title = "Demo: Survey Hours",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Hours",
      value = "survey_hours",
      icon = bsicons::bs_icon("house"),
      mod_survey_hours_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_hours_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
