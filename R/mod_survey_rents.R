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
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_rents_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
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
          )
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
          )
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
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
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
          contextMenu = TRUE,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) |>
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
          contextMenu = TRUE,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) |>
          rhandsontable::hot_col(1, type = "dropdown", source = get_survey_choices("floorplans", "floorplan_type"), halign = "htCenter") |>
          rhandsontable::hot_col(2, halign = "htCenter") |>
          rhandsontable::hot_col(3, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_col(4, type = "numeric", halign = "htCenter", format = "$0,0.00") |>
          rhandsontable::hot_validate_numeric(cols = c(3:4), min = 0)
      })

      output$modal_concessions_expenses <- rhandsontable::renderRHandsontable({
        shiny::req(rents_data())

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
          contextMenu = TRUE,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE) |>
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

      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab())

        if (session$userData$selected_survey_tab() != "nav_rents") {
          return()
        }

        iv$initialize()
        iv$enable()

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Rents",
            size = "xl",
            easyClose = TRUE,
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            ),
            bslib::accordion(
              bslib::accordion_panel(
                title = "Edit Floorplans",
                icon = bsicons::bs_icon("house"),
                rhandsontable::rHandsontableOutput(ns("modal_rents_floorplans"))
              ),
              bslib::accordion_panel(
                "Edit Market Rents",
                icon = bsicons::bs_icon("bar-chart-line"),
                rhandsontable::rHandsontableOutput(ns("modal_rents"))
              ),
              bslib::accordion_panel(
                "Edit Concessions & Expenses",
                icon = bsicons::bs_icon("currency-dollar"),
                rhandsontable::rHandsontableOutput(ns("modal_concessions_expenses"))
              )
            )
          )
        )
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
            square_feet_per_bed,
            available,
            market_rent_per_bed,
            market_rent_per_square_foot,
            concessions_gift_card,
            concessions_one_time_rent,
            concessions_monthly_rent,
            effective_rent_per_bed,
            effective_rent_per_square_foot,
            expenses_furniture,
            expenses_tv,
            expenses_electricity_gas,
            expenses_water,
            expenses_cable_internet,
            expenses_trash_valet,
            expenses_parking,
            expenses_total,
            bundled_rent_per_bed,
            bundled_rent_per_square_foot,
            updated_by
          )

        new_values_floorplans <- rhandsontable::hot_to_r(input$modal_rents_floorplans)
        new_values_rents <- rhandsontable::hot_to_r(input$modal_rents)
        new_values_concessions_expenses <- rhandsontable::hot_to_r(input$modal_concessions_expenses)

        # merge new_values_* into table like initial data
        new_data <- initial_values |>
          dplyr::select("floorplan_type", "floorplan_id") |>
          dplyr::distinct() |>
          dplyr::left_join(new_values_floorplans, by = c("floorplan_type", "floorplan_id")) |>
          dplyr::left_join(new_values_rents, by = c("floorplan_type", "floorplan_id")) |>
          dplyr::left_join(new_values_concessions_expenses, by = c("floorplan_type", "floorplan_id")) |>
          dplyr::mutate(
            property_id = prop_id,
            competitor_id = comp_id,
            leasing_week_id = week_id,
            property_name = prop_name,
            updated_by = selected_filters$user_id
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
            square_feet_per_bed,
            available,
            market_rent_per_bed,
            market_rent_per_square_foot,
            concessions_gift_card,
            concessions_one_time_rent,
            concessions_monthly_rent,
            effective_rent_per_bed,
            effective_rent_per_square_foot,
            expenses_furniture,
            expenses_tv,
            expenses_electricity_gas,
            expenses_water,
            expenses_cable_internet,
            expenses_trash_valet,
            expenses_parking,
            expenses_total,
            bundled_rent_per_bed,
            bundled_rent_per_square_foot,
            updated_by
          )

        changed_values <- dplyr::anti_join(
          new_values,
          initial_values,
          by = names(new_values)
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
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
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

# utilities ---------------------------------------------------------------
