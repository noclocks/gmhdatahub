#  ------------------------------------------------------------------------
#
# Title : Survey Parking Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Parking Shiny Module
#'
#' @name mod_survey_parking
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Parking page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_parking_ui()`: User Interface (UI) for the module.
#' - `mod_survey_parking_server()`: Server logic for the module.
#' - `mod_survey_parking_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_parking_ui()`: UI output
#' - `mod_survey_parking_server()`: List of reactive expressions.
#' - `mod_survey_parking_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_parking_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_parking
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_parking_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      reactable::reactableOutput(ns('survey_parking_tbl'))
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_parking
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_parking_server <- function(
    id,
    pool = NULL,
    selected_property_id = NULL,
    edit_survey_section = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_parking_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # handle selected property ID
      if (is.null(selected_property_id)) {
        selected_property_id <- shiny::reactive({
          get_property_id_by_name("1047 Commonwealth Avenue")
        })
      }

      db_refresh_trigger <- shiny::reactiveVal(0)

      parking_data <- shiny::reactive({
        shiny::req(pool, selected_property_id())#, session$userData$leasing_week())

        db_read_mkt_parking(
          pool,
          property_id = selected_property_id(),
          competitor_id = session$userData$selected_survey_competitor()#,
          # leasing_week = session$userData$leasing_week()
        ) #|>
        # # Remove `is_` from column names
        # dplyr::rename_with(~ gsub('is_', '', .)) |>
        # janitor::clean_names(case = 'title')
      })

      output$survey_parking_tbl <- reactable::renderReactable({
        data <- parking_data() # |>
        # Change `FALSE` to `'No'` & `TRUE` to `'Yes'`

        if (nrow(data) == 0) {
          data <- tibble::tribble(
            ~parking_type, ~is_required, ~is_included, ~amount,
            'Surface', 'No', 'No', '$0',
            'Reserved', 'No', 'No', '$0',
            'Covered', 'No', 'No', '$0',
            'Garage', 'No', 'No', '$0',
            'Other', 'No', 'No', '$0'
          )
        }

        reactable::reactable(
          data,
          defaultPageSize = nrow(data),
          searchable = TRUE,
          columns = list(
            parking_type = reactable::colDef(
              name = 'Parking Type',
              align = 'left'
            ),
            is_required = reactable::colDef(
              name = 'Is Required',
              align = 'center'
            ),
            is_included = reactable::colDef(
              name = 'Is Included',
              align = 'center'
            ),
            amount = reactable::colDef(
              name = 'Amount',
              align = 'right'
            )
          ),
          # bordered = TRUE,
          # striped = TRUE,
          # hover = TRUE
          highlight = TRUE
        )
      })

      # Edit ####
      shiny::observeEvent(edit_survey_section(), {
        if (session$userData$selected_survey_tab() != "nav_parking") {
          return()
        }

        # TODO:
        #   `iv` (?)

        shiny::showModal(
          shiny::modalDialog(
            title = "Parking",
            size = "l",
            bslib::layout_columns(
              col_widths = c(12),
              rhandsontable::rHandsontableOutput(ns("modal_survey_parking_table"))
            ),
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save_changes"),
                "Save",
                class = "btn-primary"
              ), # |>
              # shinyjs::disabled(),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      output$modal_survey_parking_table <- rhandsontable::renderRHandsontable({
        data <- parking_data()

        if (nrow(data) == 0) {
          data <- tibble::tribble(
            ~parking_type, ~is_required, ~is_included, ~amount,
            'Surface', FALSE, FALSE, '$0',
            'Reserved', FALSE, FALSE, '$0',
            'Covered', FALSE, FALSE, '$0',
            'Garage', FALSE, FALSE, '$0',
            'Other', FALSE, FALSE, '$0'
          )
        }

        rhandsontable::rhandsontable(
          data = data,
          contextMenu = FALSE,
          rowHeaders = NULL,
          colHeaders = c('Parking', 'Required', 'Included', 'Amount'),
          width = 400#,
          # height = height
        ) |>
          rhandsontable::hot_cols(
            colWidths = c(150, 75, 75, 100)
          ) |>
          rhandsontable::hot_col(
            col = 1,
            readOnly = TRUE,
            renderer = "
              function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);

                // Set text to black
                td.style.color = 'black';
              }
            "
          ) |>
          rhandsontable::hot_col(
            col = 2,
            type = 'checkbox'
            # type = 'dropdown',
            # source = c('Yes', 'No')
          ) |>
          rhandsontable::hot_col(
            col = 3,
            type = 'checkbox'
            # type = 'dropdown',
            # source = c('Yes', 'No')
          ) |>
          rhandsontable::hot_col(
            col = 4,
            type = 'numeric'
          )
      })

      shiny::observeEvent(input$save_changes, {
        shiny::req(pool, selected_property_id(), session$userData$leasing_week())

        new_values <- rhandsontable::hot_to_r(input$modal_survey_parking_table) |>
          dplyr::mutate(
            property_id = selected_property_id(),
            leasing_week = session$userData$leasing_week()
          ) |>
          dplyr::select(
            property_id,
            leasing_week,
            dplyr::everything()
          )

        # browser()

        db_update_mkt_parking(
          pool,
          property_id = selected_property_id(),
          leasing_week = session$userData$leasing_week(),
          new_values = new_values
        )

        db_refresh_trigger(db_refresh_trigger() + 1)

        shiny::removeModal()
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

#' @rdname mod_survey_parking
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_parking_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Parking",
    window_title = "Demo: Survey Parking",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Parking",
      value = "survey_parking",
      icon = bsicons::bs_icon("house"),
      mod_survey_parking_ui("demo")
    ),
    shiny::actionButton(
      "edit_survey_section",
      "Edit",
      icon = shiny::icon("edit"),
      style = "width: auto;",
      class = "btn-sm btn-primary"
    )
  )

  server <- function(input, output, session) {
    mod_survey_parking_server(
      "demo",
      pool = db_connect(),
      edit_survey_section = shiny::reactive(input$edit_survey_section)
    )
  }

  shiny::shinyApp(ui, server)
}
