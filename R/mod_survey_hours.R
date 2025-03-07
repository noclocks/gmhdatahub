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
#' @importFrom bslib page_fluid card card_header card_body card_footer
#' @importFrom htmltools tagList tags HTML
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS textOutput
mod_survey_hours_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      # add CSS to fix rhandsontable context menu:
      htmltools::tags$style(htmltools::HTML(".htMenu { z-index: 1051; }")),
      # JS for re-rendering tables in modals:
      htmltools::tags$script(
        src = "www/scripts/survey_hours/mod_survey_hours.js",
        type = "text/javascript"
      )
    ),
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
#' @importFrom cli cat_rule
#' @importFrom dplyr select arrange mutate pull anti_join
#' @importFrom htmltools tagAppendAttributes tagList
#' @importFrom reactable renderReactable reactable colDef
#' @importFrom reactablefmtr pill_buttons
#' @importFrom rhandsontable renderRHandsontable hot_col rhandsontable rHandsontableOutput hot_to_r
#' @importFrom shiny moduleServer reactiveVal observe req reactive renderText observeEvent showModal
#' @importFrom shiny modalDialog actionButton modalButton showNotification removeModal withProgress setProgress
#' @importFrom shinyvalidate InputValidator
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

      # reset trigger
      need_reset <- shiny::reactiveVal(FALSE)

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

      # helper function for property info
      get_property_info <- function() {
        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          list(
            prop_id = NA_integer_,
            comp_id = selected_filters$competitor_id,
            prop_name = selected_filters$competitor_name
          )
        } else {
          list(
            prop_id = selected_filters$property_id,
            comp_id = NA_integer_,
            prop_name = selected_filters$property_name
          )
        }
      }

      # data --------------------------------------------------------------------

      # initial data
      hours_data <- shiny::reactive({
        shiny::req(survey_data$hours)
        db_refresh_trigger()
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
              align = "center"
            ),
            close_time = reactable::colDef(
              name = "Close Time",
              align = "center"
            )
          )
        )
      })

      output$modal_hours_table <- rhandsontable::renderRHandsontable({

        shiny::req(hours_data())

        if (need_reset()) { need_reset(FALSE) }

        tbl_data <- hours_data()

        rhandsontable::rhandsontable(
          tbl_data,
          colHeaders = c("Day of Week", "Open Time", "Close Time"),
          rowHeaders = NULL
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

        # create table outputs first (to avoid re-rendering issues)
        modal_hours_hot <- rhandsontable::rHandsontableOutput(ns("modal_hours_table")) |>
          htmltools::tagAppendAttributes(.cssSelector = "div", style = "width: 100%; height: auto;")

        shiny::showModal(
          shiny::modalDialog(
            title = htmltools::tags$span(
              style = "font-size: 1.5rem; font-weight: bold;",
              "Edit Hours"
            ),
            size = "l",
            easyClose = FALSE,
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::actionButton(
                ns("cancel_button"),
                "Cancel",
                class = "btn-danger"
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              htmltools::tags$div(
                class = "hot-container",
                style = "width: 100%; padding: 10px;",
                modal_hours_hot
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              bslib::card(
                bslib::card_header("Review Changes"),
                bslib::card_body(
                  shiny::uiOutput(ns("changes_preview"))
                )
              )
            )
          )
        )
      })

      # resize trigger
      shiny::observeEvent(input$modal_hours_table_shown, {
        session$sendCustomMessage("resize_handsontable", list(id = "modal_hours_table"))
      })

      # changes ----------------------------------------------
      changes <- shiny::reactive({
        shiny::req(hours_data(), input$modal_hours_table)

        prop_info <- get_property_info()

        original_data <- hours_data()
        new_data <- rhandsontable::hot_to_r(input$modal_hours_table)

        compare_cols <- c("day_of_week", "open_time", "close_time")

        changes_list <- list()

        # Compare each row
        for (i in 1:nrow(original_data)) {
          for (col in compare_cols) {
            old_val <- original_data[[col]][i]
            new_val <- new_data[[col]][i]

            # If values are different, add to changes list
            if (!isTRUE(all.equal(old_val, new_val))) {
              day_of_week <- original_data$day_of_week[i]
              field_name <- paste(day_of_week, col, sep = "_")

              changes_list[[field_name]] <- list(
                day_of_week = day_of_week,
                field = col,
                old = old_val,
                new = new_val
              )
            }
          }
        }

        changes_list
      })

      output$changes_preview <- shiny::renderUI({
        shiny::req(changes())

        changes_data <- changes()

        if (length(changes_data) == 0) {
          return(htmltools::tags$p("No changes detected. Edit the table to see changes here.",
                                   class = "text-muted"))
        }

        # Create UI elements for each change
        changes_ui <- lapply(names(changes_data), function(change_id) {
          change <- changes_data[[change_id]]

          htmltools::tags$div(
            class = "change-item mb-2 p-2 border rounded",
            htmltools::tags$p(
              class = "mb-1",
              htmltools::tags$strong(
                paste0(change$day_of_week, ": ", tools::toTitleCase(gsub("_", " ", change$field)))
              )
            ),
            htmltools::tags$div(
              class = "d-flex justify-content-between",
              htmltools::tags$span(
                class = "old-value",
                paste("Current:", change$old),
                style = "color: #666;"
              ),
              htmltools::tags$span(
                "→",
                style = "margin: 0 10px;"
              ),
              htmltools::tags$span(
                class = "new-value",
                paste("New:", change$new),
                style = "color: #007bff; font-weight: bold;"
              )
            )
          )
        })

        do.call(htmltools::tagList, changes_ui)
      })

      # cancel ------------------------------------------------------------------
      shiny::observeEvent(input$cancel_button, {
        if (length(changes()) > 0) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Unsaved Changes",
              "You have unsaved changes. Are you sure you want to cancel?",
              footer = htmltools::tagList(
                shiny::actionButton(ns("confirm_cancel"), "Yes, Cancel"),
                shiny::modalButton("No, Continue Editing")
              ),
              size = "s"
            )
          )
        } else {
          # No changes, just close the modal
          shiny::removeModal()
        }
      })

      shiny::observeEvent(input$confirm_cancel, {
        need_reset(TRUE)
        shiny::removeModal()
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
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar nav_spacer nav_panel
#' @importFrom pkgload load_all
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

