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
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid card card_header card_body layout_columns card_footer
#' @importFrom htmltools tagList tags HTML
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS textOutput actionButton icon
mod_survey_parking_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$script(
        src = "www/scripts/survey_parking/mod_survey_parking.js",
        type = "text/javascript"
      ),
      htmltools::tags$link(
        rel = "stylesheet",
        href = "www/scripts/survey_parking/mod_survey_parking.css"
      )
    ),
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
#' @importFrom bslib layout_columns
#' @importFrom cli cat_rule
#' @importFrom dplyr select mutate anti_join
#' @importFrom htmltools tagAppendAttributes tagList
#' @importFrom reactable renderReactable reactable colDef colFormat
#' @importFrom reactablefmtr pill_buttons
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_col hot_cols rhandsontable hot_to_r
#' @importFrom shiny moduleServer reactiveVal observe req reactive observeEvent showModal modalDialog actionButton
#' @importFrom shiny modalButton showNotification removeModal withProgress setProgress
#' @importFrom shinyvalidate InputValidator
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

      # reset flag for table data
      need_reset <- shiny::reactiveVal(FALSE)

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

      # filters ----------------------------------------------------------
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------

      # initial section data
      parking_data <- shiny::reactive({
        shiny::req(survey_data$parking)
        db_refresh_trigger()
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

      # inputs/rhandsontable data
      rhandsontable_data <- shiny::reactive({
        shiny::req(survey_data$parking)

        if (!is.null(input$modal_survey_parking_table)) {
          hot_data <- rhandsontable::hot_to_r(input$modal_survey_parking_table)
        } else {
          hot_data <- parking_data()
        }

        prop_info <- get_property_info()

        hot_data |>
          dplyr::mutate(
            property_id = as.integer(prop_info$prop_id),
            competitor_id = as.integer(prop_info$comp_id),
            property_name = prop_info$prop_name,
            updated_by = selected_filters$user_id
          ) |>
          dplyr::select(
            "property_id",
            "competitor_id",
            "property_name",
            "parking_type",
            "is_required",
            "is_included",
            "amount",
            "updated_by"
          )

      })

      # outputs -----------------------------------------------------------------

      # property name
      output$property_name_title <- shiny::renderText({
        prop_info <- get_property_info()
        prop_id <- prop_info$prop_id
        comp_id <- prop_info$comp_id
        name <- prop_info$prop_name
        if (is.na(comp_id)) {
          paste0(name, " (", prop_id, ")")
        } else {
          paste0(name, " (Competitor #", comp_id, ")")
        }
      })

      # last updated
      output$last_updated_at <- shiny::renderText({
        shiny::req(survey_data$parking)
        if (nrow(survey_data$parking) == 0) {
          "Never"
        } else {
          survey_data$parking |>
            dplyr::pull(.data$updated_at) |>
            max(na.rm = TRUE) |>
            as.POSIXct() |>
            format("%Y-%m-%d %H:%M:%S")
        }
      })

      # table -------------------------------------------------------------------
      output$survey_parking_tbl <- reactable::renderReactable({
        shiny::req(parking_data())
        tbl_data <- parking_data()

        reactable::reactable(
          tbl_data,
          defaultPageSize = nrow(tbl_data),
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

        # create table outputs first (to avoid re-rendering issues)
        modal_parking_hot <- rhandsontable::rHandsontableOutput(ns("modal_survey_parking_table")) |>
          htmltools::tagAppendAttributes(.cssSelector = "div", style = "width: 100%; height: auto;")

        shiny::showModal(
          shiny::modalDialog(
            title = htmltools::tags$span(
              style = "font-size: 1.5rem; font-weight: bold;",
              "Edit Parking"
            ),
            size = "l",
            easyClose = FALSE,
            bslib::layout_columns(
              col_widths = c(12),
              htmltools::tags$div(
                class = "hot-container",
                style = "width: 100%; padding: 10px;",
                modal_parking_hot
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              bslib::card(
                bslib::card_header("Review Changes"),
                bslib::card_body(
                  shiny::uiOutput(session$ns("changes_preview"))
                )
              )
            ),
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
            )
          )
        )
      })

      # resize trigger
      shiny::observeEvent(input$modal_survey_parking_table_shown, {
        session$sendCustomMessage("resize_handsontable", list(id = "modal_survey_parking_table"))
      })

      # edit table --------------------------------------------------------------
      output$modal_survey_parking_table <- rhandsontable::renderRHandsontable({
        shiny::req(parking_data())

        if (need_reset()) { need_reset(FALSE) }

        tbl_data <- parking_data()

        rhandsontable::rhandsontable(
          data = tbl_data,
          contextMenu = FALSE,
          rowHeaders = NULL,
          stretchH = "all",
          width = "100%",
          height = "auto",
          highlightRow = TRUE,
          highlightCol = TRUE,
          overflow = "hidden",
          colHeaders = c("Parking", "Required", "Included", "Amount")
        ) |>
          rhandsontable::hot_cols(
            colWidths = c(150, 75, 75, 100),
            halign = "htCenter",
            valign = "htMiddle"
          ) |>
          rhandsontable::hot_col(col = 1, readOnly = TRUE) |>
          rhandsontable::hot_col(col = 2, type = "checkbox") |>
          rhandsontable::hot_col(col = 3, type = "checkbox") |>
          rhandsontable::hot_col(col = 4, type = "numeric", format = "$0,0.00")
      })


      # changes -----------------------------------------------------------------
      changes <- shiny::reactive({

        shiny::req(parking_data(), input$modal_survey_parking_table)

        prop_info <- get_property_info()

        original_data <- parking_data() |>
          dplyr::mutate(
            property_id = as.integer(prop_info$prop_id),
            competitor_id = as.integer(prop_info$comp_id),
            property_name = prop_info$prop_name,
            updated_by = selected_filters$user_id
          )

        new_data <- rhandsontable::hot_to_r(input$modal_survey_parking_table)

        compare_cols <- c("parking_type", "is_required", "is_included", "amount")

        changes_list <- list()

        # Compare each row
        for (i in 1:nrow(original_data)) {
          for (col in compare_cols) {
            old_val <- original_data[[col]][i]
            new_val <- new_data[[col]][i]

            # Handle numeric values properly
            if (is.numeric(old_val)) {
              old_val <- round(old_val, 2)
              new_val <- round(new_val, 2)
            }

            # If values are different, add to changes list
            if (!isTRUE(all.equal(old_val, new_val))) {
              parking_type <- original_data$parking_type[i]
              field_name <- paste(parking_type, col, sep = "_")

              changes_list[[field_name]] <- list(
                parking_type = parking_type,
                field = col,
                old = old_val,
                new = new_val
              )
            }
          }
        }

        changes_list

      })

      # log changes for debugging
      shiny::observe({
        shiny::req(changes())
        num_changes <- length(changes())
        cli::cli_alert_info("{.field {num_changes}} changes detected in the Survey Parking Module.")
        dplyr::glimpse(changes())
      })

      # force changes reactive to update when table is edited
      shiny::observeEvent(input$modal_survey_parking_table, {
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # reset the table when the modal is closed
      shiny::observeEvent(input$cancel_changes, {
      })

      # change preview UI
      output$changes_preview <- shiny::renderUI({
        shiny::req(changes())

        changes_data <- changes()

        if (length(changes_data) == 0) {
          return(htmltools::tags$p("No changes detected. Edit the table to see changes here.", class = "text-muted"))
        }

        # Create UI elements for each change
        changes_ui <- lapply(names(changes_data), function(change_id) {
          change <- changes_data[[change_id]]

          # Format the display values based on type
          old_display <- if (is.logical(change$old)) {
            ifelse(change$old, "Yes", "No")
          } else if (is.numeric(change$old) && change$field == "amount") {
            paste0("$", format(change$old, nsmall = 2))
          } else {
            as.character(change$old)
          }

          new_display <- if (is.logical(change$new)) {
            ifelse(change$new, "Yes", "No")
          } else if (is.numeric(change$new) && change$field == "amount") {
            paste0("$", format(change$new, nsmall = 2))
          } else {
            as.character(change$new)
          }

          # Create the change display element
          htmltools::tags$div(
            class = "change-item mb-2 p-2 border rounded",
            htmltools::tags$p(
              class = "mb-1",
              htmltools::tags$strong(
                paste0(change$parking_type, ": ", tools::toTitleCase(gsub("_", " ", change$field)))
              )
            ),
            htmltools::tags$div(
              class = "d-flex justify-content-between",
              htmltools::tags$span(
                class = "old-value",
                paste("Current:", old_display),
                style = "color: #666;"
              ),
              htmltools::tags$span(
                "→",
                style = "margin: 0 10px;"
              ),
              htmltools::tags$span(
                class = "new-value",
                paste("New:", new_display),
                style = "color: #007bff; font-weight: bold;"
              )
            )
          )
        })

        do.call(htmltools::tagList, changes_ui)

      })

      # Add a confirmation dialog if changes exist
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

      # If they confirm cancellation
      shiny::observeEvent(input$confirm_cancel, {
        need_reset(TRUE)
        shiny::removeModal()
      })


      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {

        prop_info <- get_property_info()

        initial_values <- parking_data() |>
          dplyr::mutate(
            property_id = as.integer(prop_info$prop_id),
            competitor_id = as.integer(prop_info$comp_id),
            property_name = prop_info$prop_name,
            updated_by = selected_filters$user_id
          ) |>
          dplyr::select(
            "property_id",
            "competitor_id",
            "property_name",
            "parking_type",
            "is_required",
            "is_included",
            "amount",
            "updated_by"
          )

        new_values <- rhandsontable_data() |>
          dplyr::mutate(
            property_id = as.integer(prop_info$prop_id),
            competitor_id = as.integer(prop_info$comp_id),
            property_name = prop_info$prop_name,
            updated_by = selected_filters$user_id
          ) |>
          dplyr::select(
            "property_id",
            "competitor_id",
            "property_name",
            "parking_type",
            "is_required",
            "is_included",
            "amount",
            "updated_by"
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
              db_refresh_trigger(db_refresh_trigger() + 1)
              db_trigger_func()
              shiny::removeModal()
            }
          )
        }
      })

      # refresh -------------------
      shiny::observeEvent(list(input$refresh, db_refresh_trigger()), {
        shiny::withProgress(
          message = "Refreshing data...",
          detail = "Please wait...",
          value = 0,
          {
            shiny::incProgress(1 / 2, detail = "Refreshing Data...")
            survey_data$parking <- db_read_survey_parking(
              pool,
              property_id = selected_filters$property_id,
              competitor_id = selected_filters$competitor_id
            )
            shiny::incProgress(1 / 2, detail = "Data Refreshed")
          }
        )
        shiny::showNotification("Data refreshed.")
      }, ignoreInit = TRUE)

      # return -----------------
      return(
        list(
          parking_data = parking_data,
          changes = changes
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_parking
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar nav_spacer nav_panel
#' @importFrom pkgload load_all
#' @importFrom shiny actionButton icon reactive shinyApp
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
