#  ------------------------------------------------------------------------
#
# Title : Survey Rents Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Rents Shiny Module
#'
#' @name mod_survey_rents
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Rents page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_rents_ui()`: User Interface (UI) for the module.
#' - `mod_survey_rents_server()`: Server logic for the module.
#' - `mod_survey_rents_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_rents_ui()`: UI output
#' - `mod_survey_rents_server()`: List of reactive expressions.
#' - `mod_survey_rents_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_rents_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_rents
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid card card_header card_body card_footer
#' @importFrom htmltools tagList tags HTML
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS actionButton icon textOutput
mod_survey_rents_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      # add CSS to fix rhandsontable context menu:
      htmltools::tags$style(htmltools::HTML(".htMenu { z-index: 1051; }")),
      # JS for re-rendering tables in modals:
      htmltools::tags$script(
        src = "www/scripts/survey_rents/mod_survey_rents.js",
        type = "text/javascript"
      )
    ),
    bslib::page_fluid(
      bslib::card(
        class = "mx-auto mb-4",
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h2(
            bsicons::bs_icon("house"),
            "Rents by Floorplan",
            shiny::actionButton(
              ns("refresh"),
              "Refresh Data",
              icon = shiny::icon("sync"),
              class = "btn-sm btn-outline-light float-end",
              style = "width: auto;"
            )
          ),
          htmltools::tags$p(
            class = "lead mb-0",
            "View rents by floorplan."
          )
        ),
        bslib::card_body(
          reactable::reactableOutput(
            ns("rents_by_floorplan")
          ) |> with_loader()
        ),
        bslib::card_footer(
          shiny::textOutput(ns("rents_by_floorplan_last_updated"))
        )
      ),
      bslib::card(
        class = "mx-auto mb-4",
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h2(
            bsicons::bs_icon("house"),
            "Average Rents by Unit Type",
            class = "mb-2"
          ),
          htmltools::tags$p(
            class = "lead mb-0",
            "View average rents by unit type."
          )
        ),
        bslib::card_body(
          reactable::reactableOutput(
            ns("avg_rents_by_unit_type")
          ) |> with_loader()
        ),
        bslib::card_footer(
          shiny::textOutput(ns("avg_rents_last_updated"))
        )
      )
    )
  )
}

# server ------------------------------------------------------------------

#' @rdname mod_survey_rents
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib accordion accordion_panel layout_columns card
#' @importFrom cli cat_rule
#' @importFrom dplyr select mutate left_join distinct anti_join
#' @importFrom htmltools tagAppendAttributes tagList
#' @importFrom reactable renderReactable
#' @importFrom rhandsontable renderRHandsontable hot_validate_numeric hot_col hot_table rhandsontable
#' @importFrom rhandsontable rHandsontableOutput hot_to_r
#' @importFrom shiny moduleServer reactiveVal observe req reactive observeEvent showModal modalDialog
#' @importFrom shiny actionButton modalButton showNotification removeModal withProgress setProgress
#' @importFrom shinyvalidate InputValidator
mod_survey_rents_server <- function(
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
      cli::cat_rule("[Module]: mod_survey_rents_server()")

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

      # filters -----------------------------------------------------------------
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------

      # rents by floorplan data
      rents_data <- shiny::reactive({
        shiny::req(survey_data$rents)

        db_refresh_trigger()

        if (nrow(survey_data$rents) == 0) {
          default_tbl_survey_rents_by_floorplan()
        } else {
          survey_data$rents |>
            dplyr::select(
              "floorplan_type",
              "floorplan_id",
              "square_feet",
              "number_of_beds",
              "number_of_baths",
              "total_units_count",
              "square_feet_per_bed",
              "available",
              "market_rent_per_bed",
              "market_rent_per_square_foot",
              "concessions_gift_card",
              "concessions_one_time_rent",
              "concessions_monthly_rent",
              "effective_rent_per_bed",
              "effective_rent_per_square_foot",
              "expenses_furniture",
              "expenses_tv",
              "expenses_electricity_gas",
              "expenses_water",
              "expenses_cable_internet",
              "expenses_trash_valet",
              "expenses_parking",
              "expenses_total",
              "bundled_rent_per_bed",
              "bundled_rent_per_square_foot"
            )
        }
      })

      # outputs --------------------------

      # output - rents by floorplan
      output$rents_by_floorplan <- reactable::renderReactable({
        shiny::req(rents_data())
        tbl_rents_by_floorplan(rents_data())
      })

      # output - average rents by unit type
      output$avg_rents_by_unit_type <- reactable::renderReactable({
        shiny::req(rents_data())
        tbl_avg_rents_by_unit_type(rents_data())
      })

      output$modal_rents_floorplans <- rhandsontable::renderRHandsontable({
        shiny::req(rents_data())

        if (need_reset()) { need_reset(FALSE) }

        tbl_data <- rents_data() |>
          dplyr::select(
            "floorplan_type",
            "floorplan_id",
            "square_feet",
            "number_of_beds",
            "number_of_baths",
            "total_units_count",
            "square_feet_per_bed",
            "available"
          )

        rhandsontable::rhandsontable(
          tbl_data,
          rowHeaders = FALSE,
          colHeaders = c(
            "Floorplan Type",
            "Floorplan ID",
            "Square Feet",
            "Number of Beds",
            "Number of Baths",
            "Total Units Count",
            "Square Feet per Bed",
            "Available"
          ),
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE) |>
          rhandsontable::hot_col(1, type = "dropdown", source = get_survey_choices("floorplans", "floorplan_type"), halign = "htCenter") |>
          rhandsontable::hot_col(2, halign = "htCenter") |>
          rhandsontable::hot_col(3, type = "numeric", halign = "htCenter") |>
          rhandsontable::hot_col(4, type = "numeric", halign = "htCenter") |>
          rhandsontable::hot_col(5, type = "numeric", halign = "htCenter") |>
          rhandsontable::hot_col(6, type = "numeric", halign = "htCenter") |>
          rhandsontable::hot_col(7, readOnly = TRUE, halign = "htCenter") |>
          rhandsontable::hot_col(8, type = "checkbox", halign = "htCenter") |>
          rhandsontable::hot_validate_numeric(cols = c(3:6), min = 0)
      })

      output$modal_rents <- rhandsontable::renderRHandsontable({
        shiny::req(rents_data())

        modal_tab_reset()

        tbl_data <- rents_data() |>
          dplyr::select(
            "floorplan_type",
            "floorplan_id",
            "market_rent_per_bed",
            "market_rent_per_square_foot"
          )

        rhandsontable::rhandsontable(
          tbl_data,
          rowHeaders = FALSE,
          colHeaders = c(
            "Floorplan Type",
            "Floorplan ID",
            "Market Rent per Bed",
            "Market Rent per Square Foot"
          ),
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE) |>
          rhandsontable::hot_col(1, type = "dropdown", source = get_survey_choices("floorplans", "floorplan_type"), halign = "htCenter") |>
          rhandsontable::hot_col(2, halign = "htCenter") |>
          rhandsontable::hot_col(3, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(4, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_validate_numeric(cols = c(3:4), min = 0)
      })

      output$modal_concessions_expenses <- rhandsontable::renderRHandsontable({
        shiny::req(rents_data())

        modal_tab_reset()

        tbl_data <- rents_data() |>
          dplyr::select(
            "floorplan_type",
            "floorplan_id",
            "concessions_gift_card",
            "concessions_one_time_rent",
            "concessions_monthly_rent",
            "expenses_furniture",
            "expenses_tv",
            "expenses_electricity_gas",
            "expenses_water",
            "expenses_cable_internet",
            "expenses_trash_valet",
            "expenses_parking",
          )

        rhandsontable::rhandsontable(
          tbl_data,
          rowHeaders = FALSE,
          colHeaders = c(
            "Floorplan Type",
            "Floorplan ID",
            "Concessions Gift Card",
            "Concessions One Time Rent",
            "Concessions Monthly Rent",
            "Expenses Furniture",
            "Expenses TV",
            "Expenses Electricity Gas",
            "Expenses Water",
            "Expenses Cable Internet",
            "Expenses Trash Valet",
            "Expenses Parking"
          ),
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE, contextMenu = FALSE) |>
          rhandsontable::hot_col(1, type = "dropdown", source = get_survey_choices("floorplans", "floorplan_type"), halign = "htCenter") |>
          rhandsontable::hot_col(2, halign = "htCenter") |>
          rhandsontable::hot_col(3, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(4, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(5, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(6, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(7, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(8, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(9, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(10, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(11, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(12, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_validate_numeric(cols = c(3:12), min = 0)
      })

      # edit --------------------------------------------------------------------

      modal_tab_reset <- shiny::reactiveVal(0)

      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab())

        if (session$userData$selected_survey_tab() != "nav_rents") {
          return()
        }

        modal_tab_reset(modal_tab_reset() + 1)

        iv$initialize()
        iv$enable()

        # create table outputs first (to avoid re-rendering issues)
        modal_rents_floorplans_hot <- rhandsontable::rHandsontableOutput(ns("modal_rents_floorplans")) |>
          htmltools::tagAppendAttributes(.cssSelector = "div", style = "width: 100%; height: auto;")
        modal_rents_hot <- rhandsontable::rHandsontableOutput(ns("modal_rents")) |>
          htmltools::tagAppendAttributes(.cssSelector = "div", style = "width: 100%; height: auto;")
        modal_concessions_expenses_hot <- rhandsontable::rHandsontableOutput(ns("modal_concessions_expenses")) |>
          htmltools::tagAppendAttributes(.cssSelector = ".rhandsontable", style = "width: 100% !IMPORTANT; height: auto !IMPORTANT;")

        shiny::showModal(
          shiny::modalDialog(
            title = htmltools::tags$span(
              style = "font-size: 1.5rem; font-weight: bold;",
              "Edit Rents"
            ),
            size = "xl",
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
            bslib::navset_card_tab(
              id = ns("rents_modal_tabs"),
              bslib::nav_panel(
                title = "Floorplans",
                icon = bsicons::bs_icon("house"),
                value = ns("floorplans"),
                bslib::layout_columns(
                  col_widths = c(12),
                  htmltools::tags$div(
                    class = "hot-container",
                    style = "width: 100%; padding: 10px;",
                    modal_rents_floorplans_hot
                  )
                )
              ),
              bslib::nav_panel(
                title = "Market Rents",
                icon = bsicons::bs_icon("bar-chart-line"),
                value = ns("market_rents"),
                bslib::layout_columns(
                  col_widths = c(12),
                  htmltools::tags$div(
                    class = "hot-container",
                    style = "width: 100%; padding: 10px;",
                    modal_rents_hot
                  )
                )
              ),
              bslib::nav_panel(
                title = "Concessions & Expenses",
                icon = bsicons::bs_icon("currency-dollar"),
                value = "concessions_expenses",
                modal_concessions_expenses_hot
              )
            ),
            # changes preview
            bslib::layout_columns(
              col_widths = c(12),
              bslib::card(
                bslib::card_header("Review Changes"),
                bslib::card_body(
                  shiny::uiOutput(session$ns("changes_preview"))
                )
              )
            )
          )
        )
      })

      # resize triggers
      shiny::observeEvent(input$modal_rents_floorplans_shown, {
        session$sendCustomMessage("resize_handsontable", list(id = "modal_rents_floorplans"))
      })

      shiny::observeEvent(input$modal_rents_shown, {
        session$sendCustomMessage("resize_handsontable", list(id = "modal_rents"))
      })

      shiny::observeEvent(input$modal_concessions_expenses_shown, {
        session$sendCustomMessage("resize_handsontable", list(id = "modal_concessions_expenses"))
      })

      # changes ----------------------------------------------------------------
      changes <- shiny::reactive({
          shiny::req(rents_data(), input$modal_rents_floorplans)

          prop_info <- get_property_info()

          original_data <- rents_data()

          new_floorplans <- rhandsontable::hot_to_r(input$modal_rents_floorplans)

          new_rents <- rhandsontable::hot_to_r(input$modal_rents)

          # `new_rents` is NULL if the Tab with this `rhandsontable` hasn't rendered yet
          if (is.null(new_rents)) {
            new_rents <- rents_data() |>
              dplyr::select(
                "floorplan_type",
                "floorplan_id",
                "market_rent_per_bed",
                "market_rent_per_square_foot"
              )
          }

          new_concessions <- rhandsontable::hot_to_r(input$modal_concessions_expenses)

          # `new_concessions` is NULL if the Tab with this `rhandsontable` hasn't rendered yet
          if (is.null(new_concessions)) {
            new_concessions <- rents_data() |>
              dplyr::select(
                "floorplan_type",
                "floorplan_id",
                "concessions_gift_card",
                "concessions_one_time_rent",
                "concessions_monthly_rent",
                "expenses_furniture",
                "expenses_tv",
                "expenses_electricity_gas",
                "expenses_water",
                "expenses_cable_internet",
                "expenses_trash_valet",
                "expenses_parking",
              )
          }

          # merge
          new_data <- original_data |>
            dplyr::select("floorplan_type", "floorplan_id") |>
            dplyr::distinct() |>
            dplyr::left_join(new_floorplans, by = c("floorplan_type", "floorplan_id")) |>
            dplyr::left_join(new_rents, by = c("floorplan_type", "floorplan_id")) |>
            dplyr::left_join(new_concessions, by = c("floorplan_type", "floorplan_id"))

          # compare columns that can be edited
          compare_cols <- c(
            "square_feet",
            "number_of_beds",
            "number_of_baths",
            "total_units_count",
            "available",
            "market_rent_per_bed",
            "market_rent_per_square_foot",
            "concessions_gift_card",
            "concessions_one_time_rent",
            "concessions_monthly_rent",
            "expenses_furniture",
            "expenses_tv",
            "expenses_electricity_gas",
            "expenses_water",
            "expenses_cable_internet",
            "expenses_trash_valet",
            "expenses_parking"
          )

          changes_list <- list()

          # Compare each row
          for (i in 1:nrow(original_data)) {
            floorplan_type <- original_data$floorplan_type[i]
            floorplan_id <- original_data$floorplan_id[i]

            # Find matching row in new data
            new_row_idx <- which(new_data$floorplan_type == floorplan_type &
                                   new_data$floorplan_id == floorplan_id)

            if (length(new_row_idx) == 1) {
              for (col in compare_cols) {
                if (col %in% names(original_data) && col %in% names(new_data)) {
                  old_val <- original_data[[col]][i]
                  new_val <- new_data[[col]][new_row_idx]

                  # Handle numeric values properly
                  if (is.numeric(old_val)) {
                    old_val <- round(old_val, 2)
                    new_val <- round(new_val, 2)
                  }

                  # If values are different, add to changes list
                  if (!isTRUE(all.equal(old_val, new_val))) {
                    field_name <- paste(floorplan_type, floorplan_id, col, sep = "_")

                    changes_list[[field_name]] <- list(
                      floorplan_type = floorplan_type,
                      floorplan_id = floorplan_id,
                      field = col,
                      old = old_val,
                      new = new_val
                    )
                  }
                }
              }
            }
          }

          changes_list
        })

      output$changes_preview <- shiny::renderUI({
        shiny::req(changes())

        changes_data <- changes()

        if (length(changes_data) == 0) {
          return(htmltools::tags$p("No changes detected. Edit the tables to see changes here.",
                                   class = "text-muted"))
        }

        # Create UI elements for each change
        changes_ui <- lapply(names(changes_data), function(change_id) {
          change <- changes_data[[change_id]]

          # Format display values based on field type
          format_value <- function(val, field) {
            if (is.logical(val)) {
              return(ifelse(val, "Yes", "No"))
            } else if (is.numeric(val)) {
              if (grepl("rent|amount|expenses|concessions", field)) {
                return(paste0("$", format(val, nsmall = 2)))
              } else {
                return(format(val, big.mark = ","))
              }
            } else {
              return(as.character(val))
            }
          }

          old_display <- format_value(change$old, change$field)
          new_display <- format_value(change$new, change$field)

          # Create the change display element
          htmltools::tags$div(
            class = "change-item mb-2 p-2 border rounded",
            htmltools::tags$p(
              class = "mb-1",
              htmltools::tags$strong(
                paste0(change$floorplan_type, " (ID: ", change$floorplan_id, "): ",
                       tools::toTitleCase(gsub("_", " ", change$field)))
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

        # Add a changes summary at the top
        changes_summary <- htmltools::tags$div(
          class = "alert alert-info",
          htmltools::tags$strong(paste(length(changes_data), "changes detected"))
        )

        # Combine summary with changes list
        htmltools::tagList(
          changes_summary,
          do.call(htmltools::tagList, changes_ui)
        )

      })

      # shiny::observeEvent(input$cancel_button, {
      #   if (length(changes()) > 0) {
      #     shiny::showModal(
      #       shiny::modalDialog(
      #         title = "Unsaved Changes",
      #         "You have unsaved changes. Are you sure you want to cancel?",
      #         footer = htmltools::tagList(
      #           shiny::actionButton(ns("confirm_cancel"), "Yes, Cancel"),
      #           shiny::modalButton("No, Continue Editing")
      #         ),
      #         size = "s"
      #       )
      #     )
      #   } else {
      #     # No changes, just close the modal
      #     shiny::removeModal()
      #   }
      # })
      #
      # # Add confirm cancel handler
      # shiny::observeEvent(input$confirm_cancel, {
      #   need_reset(TRUE)
      #   shiny::removeModal()
      # })


      shiny::observeEvent(input$cancel_button, {
        need_reset(TRUE)
        shiny::removeModal()
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

        if (is.null(selected_filters$leasing_week_id) || is.na(selected_filters$leasing_week_id)) {
          week_date <- get_leasing_week_start_date()
          week_id <- get_leasing_week_id_by_date(week_date)
        } else {
          week_id <- selected_filters$leasing_week_id
          week_date <- selected_filters$leasing_week_date
        }

        initial_values <- survey_data$rents |>
          dplyr::select(
            property_id,
            competitor_id,
            leasing_week_id,
            property_name,
            floorplan_type,
            floorplan_id,
            square_feet,
            number_of_beds,
            number_of_baths,
            total_units_count,
            available,
            market_rent_per_bed,
            concessions_gift_card,
            concessions_one_time_rent,
            concessions_monthly_rent,
            expenses_furniture,
            expenses_tv,
            expenses_electricity_gas,
            expenses_water,
            expenses_cable_internet,
            expenses_trash_valet,
            expenses_parking,
            updated_by
          )

        new_values_floorplans <- rhandsontable::hot_to_r(input$modal_rents_floorplans)
        new_values_rents <- rhandsontable::hot_to_r(input$modal_rents)
        new_values_concessions_expenses <- rhandsontable::hot_to_r(input$modal_concessions_expenses)

        # merge new_values_* into table like initial data
        new_values <- initial_values |>
          dplyr::select("floorplan_type", "floorplan_id") |>
          dplyr::distinct() |>
          dplyr::left_join(new_values_floorplans, by = c("floorplan_type", "floorplan_id")) |>
          dplyr::left_join(new_values_rents, by = c("floorplan_type", "floorplan_id")) |>
          dplyr::left_join(new_values_concessions_expenses, by = c("floorplan_type", "floorplan_id")) |>
          dplyr::mutate(
            property_id = as.integer(.env$prop_id),
            competitor_id = as.integer(.env$comp_id),
            leasing_week_id = as.integer(.env$week_id),
            property_name = .env$prop_name,
            updated_by = .env$selected_filters$user_id
          ) |>
          dplyr::select(
            property_id,
            competitor_id,
            leasing_week_id,
            property_name,
            floorplan_type,
            floorplan_id,
            square_feet,
            number_of_beds,
            number_of_baths,
            total_units_count,
            available,
            market_rent_per_bed,
            concessions_gift_card,
            concessions_one_time_rent,
            concessions_monthly_rent,
            expenses_furniture,
            expenses_tv,
            expenses_electricity_gas,
            expenses_water,
            expenses_cable_internet,
            expenses_trash_valet,
            expenses_parking,
            updated_by
          )

        changed_values <- dplyr::anti_join(
          new_values,
          initial_values,
          by = c(
            "property_id",
            "competitor_id",
            "leasing_week_id",
            "property_name",
            "floorplan_type",
            "floorplan_id",
            "square_feet",
            "number_of_beds",
            "number_of_baths",
            "total_units_count",
            "available",
            "market_rent_per_bed",
            "concessions_gift_card",
            "concessions_one_time_rent",
            "concessions_monthly_rent",
            "expenses_furniture",
            "expenses_tv",
            "expenses_electricity_gas",
            "expenses_water",
            "expenses_cable_internet",
            "expenses_trash_valet",
            "expenses_parking"
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
              db_update_survey_rents_by_floorplan(pool, changed_values)
              shiny::setProgress(value = 1, detail = "Changes saved.")
              db_trigger_func()
              shiny::removeModal()
            }
          )
        }
      })

      # return ------------------------------------------------------------------
      return(
        list(
          rents_data = rents_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_rents
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar nav_spacer nav_panel
#' @importFrom pkgload load_all
#' @importFrom shiny actionButton icon reactive shinyApp
mod_survey_rents_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Rents",
    window_title = "Demo: Survey Rents",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Rents",
      value = "survey_rents",
      icon = bsicons::bs_icon("house"),
      mod_survey_rents_ui("demo"),
      shiny::actionButton(
        "edit_survey_section",
        "Edit",
        icon = shiny::icon("edit"),
        style = "width: auto;",
        class = "btn-sm btn-primary"
      )
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    edit_survey_section <- shiny::reactive({
      input$edit_survey_section
    })
    mod_survey_rents_server(
      "demo",
      pool = pool,
      edit_survey_section = edit_survey_section
    )
  }

  shiny::shinyApp(ui, server)
}
