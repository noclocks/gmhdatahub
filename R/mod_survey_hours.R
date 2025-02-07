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
          reactable::reactableOutput(ns("hours_table")) |>
            with_loader()
        ),
        bslib::card_footer(
          shiny::textOutput(ns("last_updated_at"), inline = TRUE)
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
      cli::cat_rule("[Module]: mod_survey_hours_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      # reactives
      initial_data <- shiny::reactiveVal()
      input_changes <- shiny::reactiveVal(0)

      # filters
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------

      # initial data
      hours_data <- shiny::reactive({
        shiny::req(survey_data$hours)
        if (nrow(survey_data$hours) == 0) {
          default_tbl_survey_hours()
        } else {
          survey_data$hours |>
            dplyr::mutate(
              open_time = format(as.POSIXct(.data$open_time, format = "%H:%M"), "%I:%M %p"),
              close_time = format(as.POSIXct(.data$close_time, format = "%H:%M"), "%I:%M %p"),
              day_of_week_temp = factor(
                .data$day_of_week,
                levels = c(
                  "Monday",
                  "Tuesday",
                  "Wednesday",
                  "Thursday",
                  "Friday",
                  "Saturday",
                  "Sunday"
                )
              )
            ) |>
            dplyr::arrange(day_of_week_temp) |>
            dplyr::select(day_of_week, open_time, close_time)
        }
      })

      # outputs -----------------------------------------------------------------
      output$property_name_title <- shiny::renderText({
        shiny::req(selected_filters)
        prop_id <- selected_filters$property_id
        comp_id <- selected_filters$competitor_id
        prop_name <- selected_filters$property_name
        if (is.na(comp_id)) {
          paste0(prop_name, "(", prop_id, ")")
        } else {
          paste0(prop_name, "(Competitor #", comp_id, ")")
        }
      })

      output$last_updated_at <- shiny::renderText({
        shiny::req(survey_data$hours)
        survey_data$hours |>
          dplyr::pull("updated_at") |>
          max(na.rm = TRUE) |>
          format("%Y-%m-%d %H:%M:%S")
      })

      # tables ------------------------------------------------------------------
      output$hours_table <- reactable::renderReactable({
        shiny::req(hours_data())

        tbl_data <- hours_data()

        reactable::reactable(
          data = tbl_data,
          defaultPageSize = nrow(tbl_data),
          searchable = TRUE,
          highlight = TRUE,
          sortable = TRUE,
          striped = TRUE,
          bordered = TRUE,
          columns = list(
            day_of_week = reactable::colDef(
              name = "Day of Week",
              align = "center",
              cell = reactablefmtr::pill_buttons(tbl_data)
            ),
            open_time = reactable::colDef(
              name = "Open Time",
              align = "center",
              format = reactable::colFormat(time = TRUE)
            ),
            close_time = reactable::colDef(
              name = "Close Time",
              align = "center",
              format = reactable::colFormat(time = TRUE)
            )
          )
        )
      })

      output$modal_hours_table <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(
          hours_data(),
          colHeaders = c("Day of Week", "Open Time", "Close Time"),
          rowHeaders = NULL,
          comments = TRUE,
          useTypes = TRUE
        ) |>
          rhandsontable::hot_col(1, readOnly = TRUE) |>
          rhandsontable::hot_col(2) |>
          rhandsontable::hot_col(3)
      })

      # edit --------------------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab())

        if (session$userData$selected_survey_tab() != "nav_hours") {
          return()
        }

        iv$initialize()
        iv$enable()

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Hours",
            size = "l",
            easyClose = TRUE,
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            ),
            rhandsontable::rHandsontableOutput(ns("modal_hours_table"))
          )
        )
      })

      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {
        shiny::req(input$modal_hours_table)

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NA_integer_
          comp_id <- selected_filters$competitor_id
          prop_name <- selected_filters$competitor_name
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NA_integer_
          prop_name <- selected_filters$property_name
        }

        initial_values <- survey_data$horus |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            day_of_week,
            open_time,
            close_time,
            updated_by
          )

        new_values <- rhandsontable::hot_to_r(input$modal_hours_table) |>
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
            day_of_week,
            open_time,
            close_time,
            updated_by
          )

        changed_values <- dplyr::anti_join(
          new_values,
          initial_values,
          by = c(
            "property_id",
            "competitor_id",
            "property_name",
            "day_of_week",
            "open_time",
            "close_time",
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
              db_update_survey_hours(pool, changed_values)
              shiny::setProgress(value = 1, detail = "Changes saved.")
              db_trigger_func()
              shiny::removeModal()
            }
          )
        }
      })

      # return -----------------------------------------------------------------
      return(
        list(
          hours_data = hours_data
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
