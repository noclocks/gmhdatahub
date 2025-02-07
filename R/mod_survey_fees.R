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
    # page --------------------------------------------------------------------
    bslib::page_fluid(
      # card --------------------------------------------------------------------
      bslib::card(
        full_screen = TRUE,
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("currency-dollar"),
            htmltools::tags$span(
              "Fees - ",
              shiny::textOutput(ns("property_name_title"), inline = TRUE)
            ),
            shiny::actionButton(
              ns("refresh"),
              "Refresh Data",
              icon = shiny::icon("sync"),
              class = "btn-sm btn-outline-light float-end",
              style = "width: auto;"
            )
          )
        ),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(12),
            reactable::reactableOutput(ns("survey_fees_tbl")) |>
              with_loader()
          )
        ),
        bslib::card_footer(
          class = "text-center",
          htmltools::tags$p(
            "Last Updated: ",
            htmltools::tags$span(
              shiny::textOutput(ns("last_updated")),
              class = "fw-bold"
            )
          )
        )
      )
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
      cli::cat_rule("[Module]: mod_survey_fees_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      # filters
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------
      fees_data <- shiny::reactive({
        shiny::req(survey_data$fees)
        browser()
        survey_data$fees
      })

      # table -------------------------------------------------------------------
      output$survey_fees_tbl <- reactable::renderReactable({
        shiny::req(fees_data())
        tbl_survey_fees(fees_data())
      })

      # edit --------------------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        if (session$userData$selected_survey_tab() != "nav_fees") {
          return()
        }

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
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      output$modal_survey_fees_table <- rhandsontable::renderRHandsontable({
        shiny::req(fees_data())

        tbl_data <- fees_data() |>
          dplyr::select(tidyselect::contains("fee_"))

        rhandsontable::rhandsontable(
          data = tbl_data,
          contextMenu = FALSE,
          rowHeaders = NULL,
          colHeaders = c("Fee Name", "Amount", "Frequency")
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

      shiny::observeEvent(input$save, {

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NULL
          comp_id <- selected_filters$competitor_id
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NULL
        }

        new_values <- rhandsontable::hot_to_r(input$modal_survey_fees_table) |>
          dplyr::mutate(
            property_id = prop_id,
            competitor_id = comp_id,
            property_name = selected_filters$property_name,
            leasing_week_id = selected_filters$leasing_week_id,
            updated_by = session$userData$user_id
          ) |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            leasing_week_id,
            fee_name,
            fee_amount,
            fee_frequency,
            updated_by
          )

        db_update_survey_fees(pool, new_values)

        # Trigger a refresh of the property data
        db_refresh_trigger(db_refresh_trigger() + 1)
        db_trigger_func()
        shiny::removeModal()

        new_values <-

        # browser()

        db_update_mkt_fees(
          pool,
          property_id = selected_property_id(),
          leasing_week = session$userData$leasing_week(),
          new_values = new_values
        )

        db_refresh_trigger(db_refresh_trigger() + 1)
        db_trigger_func()
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
