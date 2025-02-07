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
    # page --------------------------------------------------------------------
    bslib::page_fluid(
      # card --------------------------------------------------------------------
      bslib::card(
        full_screen = TRUE,
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("car-front"),
            htmltools::tags$span(
              "Parking - ",
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
            reactable::reactableOutput(ns("survey_parking_tbl")) |>
              with_loader()
          )
        ),
        bslib::card_footer(
          class = "text-muted d-flex justify-content-between align-items-center",
          htmltools::tags$small(
            "Last Updated:",
            htmltools::tags$span(
              class = "fw-bold",
              shiny::textOutput(ns("last_updated_at"), inline = TRUE)
            )
          )
        )
      )
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
      cli::cat_rule("[Module]: mod_survey_parking_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      # filters ----------------------------------------------------------
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------
      parking_data <- shiny::reactive({
        shiny::req(survey_data$parking)

        if (nrow(survey_data$parking) == 0) {
          default_tbl_survey_parking()
        } else {
          survey_data$parking |>
            dplyr::select(
              parking_type,
              is_required,
              is_included,
              amount
            )
        }
      })

      # table -------------------------------------------------------------------
      output$survey_parking_tbl <- reactable::renderReactable({
        shiny::req(parking_data())

        tbl_data <- parking_data()

        reactable::reactable(
          data,
          defaultPageSize = nrow(data),
          searchable = TRUE,
          bordered = TRUE,
          striped = TRUE,
          highlight = TRUE,
          columns = list(
            parking_type = reactable::colDef(
              name = "Parking",
              cell = reactablefmtr::pill_buttons(data = tbl_data),
            ),
            is_required = reactable::colDef(
              name = "Required?",
              align = "center",
              cell = function(value) format_boolean(value)
            ),
            is_included = reactable::colDef(
              name = "Included?",
              align = "center",
              cell = function(value) format_boolean(value)
            ),
            amount = reactable::colDef(
              name = "Amount",
              align = "right",
              format = reactable::colFormat(currency = "USD")
            )
          )
        )
      })

      # edit --------------------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        if (session$userData$selected_survey_tab() != "nav_parking") {
          return()
        }

        iv$initialize()
        iv$enable()

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
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      # edit table --------------------------------------------------------------
      output$modal_survey_parking_table <- rhandsontable::renderRHandsontable({
        shiny::req(parking_data())

        tbl_data <- parking_data()

        rhandsontable::rhandsontable(
          data = tbl_data,
          contextMenu = TRUE,
          rowHeaders = NULL,
          colHeaders = c("Parking", "Required", "Included", "Amount"),
          width = 400
        ) |>
          rhandsontable::hot_cols(
            colWidths = c(150, 75, 75, 100)
          ) |>
          rhandsontable::hot_col(col = 1, readOnly = TRUE) |>
          rhandsontable::hot_col(col = 2, type = "checkbox") |>
          rhandsontable::hot_col(col = 3, type = "checkbox") |>
          rhandsontable::hot_col(col = 4, type = "numeric")
      })

      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NA_integer_
          comp_id <- selected_filters$competitor_id
          prop_name <- selected_filters$competitor_name
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NA_integer_
          prop_name <- selected_filters$property_name
        }

        initial_values <- survey_data$parking |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            parking_type,
            is_required,
            is_included,
            amount,
            updated_by
          )

        new_values <- rhandsontable::hot_to_r(input$modal_survey_parking_table) |>
          dplyr::mutate(
            property_id = as.integer(prop_id),
            competitor_id = as.integer(comp_id),
            property_name = prop_name,
            updated_by = selected_filters$user_id
          ) |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            parking_type,
            is_required,
            is_included,
            amount,
            updated_by
          )

        changed_values <- dplyr::anti_join(
          new_values,
          initial_values,
          by = c(
            "property_id",
            "competitor_id",
            "property_name",
            "parking_type",
            "is_required",
            "is_included",
            "amount",
            "updated_by"
          )
        )

        if (nrow(changed_values) == 0) {
          shiny::showNotification("No changes detected.")
          shiny::removeModal()
          return()
        } else {
          shiny::withProgress(
            message = "Saving changes...",
            detail = "Please wait...",
            value = 0,
            {
              db_update_survey_parking(pool, changed_values)
              shiny::setProgress(value = 1, detail = "Changes saved.")
              db_trigger_func()
              shiny::removeModal()
            }
          )
        }
      })

      return(
        list(
          parking_data = parking_data
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
