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
    bslib::card(
      reactable::reactableOutput(ns("survey_fees_tbl"))
    )
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
    global_filters = NULL,
    selected_property_id = NULL,
    edit_survey_section = NULL) {
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

      fees_data <- shiny::reactive({
        shiny::req(pool, selected_property_id(), session$userData$leasing_week())

        db_read_mkt_fees(
          pool,
          property_id = selected_property_id(),
          leasing_week = session$userData$leasing_week()
        )
      })

      output$survey_fees_tbl <- reactable::renderReactable({
        # query data
        # survey_fees <- db_query_survey_fees(pool)

        data <- fees_data()

        # browser()

        if (nrow(data) == 0) {
          data <- tibble::tribble(
            ~"Fees", ~"Amount", ~"Frequency",
            "Application Fee", "$0", "Monthly",
            "Administration Fee", "$0", "Monthly",
            "Fee Structure", "Both Fees Waived", "Monthly",
            "Utility Set Up Fee", "$0", "Monthly",
            "Utility Deposit", "$0", "Monthly",
            "Amenity Fee", "$0", "Monthly",
            "Common Area Fee", "$0", "Monthly",
            "Smart Home Fee", "$0", "Monthly",
            "Restoration Fee", "$0", "Monthly",
            "Security Deposit", "$1000", "Monthly",
            "Pet Fee", "$0", "Monthly",
            "Pet Deposit", "$0", "Monthly",
            "Pet Rent", "$0", "Monthly"
          )
        }

        # render table
        reactable::reactable(
          data = data,
          defaultPageSize = nrow(data),
          searchable = TRUE
        )
      })

      # Edit ####
      shiny::observeEvent(edit_survey_section(), {
        if (session$userData$selected_survey_tab() != "nav_fees") {
          return()
        }

        # data <- fees_data()

        # TODO:
        #   `iv` (?)

        shiny::showModal(
          shiny::modalDialog(
            title = "Fees",
            size = "l",
            bslib::layout_columns(
              col_widths = c(12),
              rhandsontable::rHandsontableOutput(ns("modal_survey_fees_table"))
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

      output$modal_survey_fees_table <- rhandsontable::renderRHandsontable({
        data <- fees_data()

        if (nrow(data) == 0) {
          data <- tibble::tribble(
            ~"Fees", ~"Amount", ~"Frequency",
            "Application Fee", "$0", "Monthly",
            "Administration Fee", "$0", "Monthly",
            "Fee Structure", "Both Fees Waived", "Monthly",
            "Utility Set Up Fee", "$0", "Monthly",
            "Utility Deposit", "$0", "Monthly",
            "Amenity Fee", "$0", "Monthly",
            "Common Area Fee", "$0", "Monthly",
            "Smart Home Fee", "$0", "Monthly",
            "Restoration Fee", "$0", "Monthly",
            "Security Deposit", "$1000", "Monthly",
            "Pet Fee", "$0", "Monthly",
            "Pet Deposit", "$0", "Monthly",
            "Pet Rent", "$0", "Monthly"
          )
        }

        rhandsontable::rhandsontable(
          data = data,
          contextMenu = FALSE,
          rowHeaders = NULL,
          colHeaders = c("Fees", "Amount", "Frequency") # ,
          # width = width,
          # height = height
        ) |>
          rhandsontable::hot_cols(
            colWidths = 200
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
            renderer = "
              function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);

                if (col === 0) {
                  // Don't allow editing
                  cellProperties.readOnly = true;
                } else if (col == 1 && row == 2) {
                  cellProperties.type = 'dropdown';
                  cellProperties.source = ['App Fee Waived-Admin Due', 'Admin Fee Waived-App Due', 'Both Fees Waived', 'Both Fees Due']
                }
              }"
          ) |>
          rhandsontable::hot_col(
            col = 3,
            type = "dropdown",
            source = c("Monthly", "Annual")
          )
      })

      shiny::observeEvent(input$save_changes, {
        shiny::req(pool, selected_property_id(), session$userData$leasing_week())

        new_values <- rhandsontable::hot_to_r(input$modal_survey_fees_table) |>
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

        db_update_mkt_fees(
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
