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
#' @param survey_data Reactive data for the survey.
#' @param map_data Reactive data for the map.
#' @param selected_filters Reactive data for the selected filters.
#' @param db_trigger_func Reactive Function to trigger database updates.
#' @param edit_survey_section Reactive Function representing the action button to edit the survey section.
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
          # fee structure
          htmltools::tags$h5("Fee Structure"),
          htmltools::tags$div(
            class = "d-flex justify-content-between float-end",
            htmltools::tags$span(
              class = "badge bg-primary",
              bsicons::bs_icon("info-circle-fill"),
              shiny::textOutput(ns("fee_structure"), inline = TRUE)
            )
          ),
          htmltools::tags$hr(),
          # fees table
          reactable::reactableOutput(ns("survey_fees_tbl")) |>
            with_loader()
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
        survey_data$fees
      })

      fee_structure_data <- shiny::reactive({
        shiny::req(survey_data$fee_structures)
        survey_data$fee_structures
      })

      # UI outputs -------------------------------------------------------------

      # last updated
      output$last_updated <- shiny::renderText({
        shiny::req(fees_data())
        fees_data()$updated_at |>
          max(na.rm = TRUE) |>
          format("%B %d, %Y %I:%M %p")
      })

      output$property_name_title <- shiny::renderText({
        shiny::req(fees_data())

        prop_id <- fees_data()$property_id |> unique()
        comp_id <- fees_data()$competitor_id |> unique()
        name <- fees_data()$property_name |> unique()

        if (is.na(comp_id)) {
          paste0(name, " (", prop_id, ")")
        } else {
          paste0(name, " (Competitor #", comp_id, ")")
        }
      })

      output$fee_structure <- shiny::renderText({
        shiny::req(fee_structure_data())
        fee_structure_data()$fee_structure |> unique()
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

        inputs <- purrr::map(
          fees_data()$fee_name |> unique(),
          ~ {
            fee_name <- .x
            fee_amount <- fees_data()$fee_amount[fees_data()$fee_name == fee_name][1]
            fee_frequency <- fees_data()$fee_frequency[fees_data()$fee_name == fee_name][1]

            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              row_heights = 1,
              htmltools::tags$h5(fee_name),
              shiny::numericInput(
                ns(paste0("fee_amount_", .x)),
                label = "Amount:",
                value = fee_amount,
                min = 0,
                step = 1,
              ),
              shiny::selectInput(
                ns(paste0("fee_frequency_", .x)),
                label = "Frequency:",
                choices = get_survey_choices("fees", "fee_frequency"),
                selected = fee_frequency
              )
            )
          }
        )

        shiny::showModal(
          shiny::modalDialog(
            title = "Fees",
            size = "xl",
            easyClose = TRUE,
            # fee structure
            htmltools::tags$h5("Fee Structure"),
            shiny::selectInput(
              ns("fee_structure"),
              "Fee Structure:",
              choices = get_survey_choices("fees", "fee_structure"),
              selected = fee_structure_data()$fee_structure[1]
            ),
            htmltools::tags$hr(),
            htmltools::tags$h5("Fee Amounts and Frequencies"),
            do.call(htmltools::tagList, inputs),
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

      # save ------------------------------------------------------------------------------------------------------------
      shiny::observeEvent(input$save, {

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NA_integer_
          comp_id <- selected_filters$competitor_id
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NA_integer_
        }

        property_name <- selected_filters$property_name
        leasing_week_id <- selected_filters$leasing_week_id
        user_id <- selected_filters$user_id

        fee_names <- fees_data()$fee_name |> unique()
        fee_amounts <- purrr::map_dbl(fee_names, ~ input[[paste0("fee_amount_", .x)]])
        fee_frequencies <- purrr::map_chr(fee_names, ~ input[[paste0("fee_frequency_", .x)]])

        new_fees_data <- tibble::tibble(
          property_id = prop_id,
          competitor_id = comp_id,
          property_name = property_name,
          leasing_week_id = leasing_week_id,
          fee_name = fee_names,
          fee_amount = fee_amounts,
          fee_frequency = fee_frequencies,
          updated_at = lubridate::now(tzone = "UTC"),
          updated_by = user_id
        )

        new_fee_structure_data <- tibble::tibble(
          property_id = prop_id,
          competitor_id = comp_id,
          property_name = property_name,
          fee_structure = input$fee_structure,
          updated_at = lubridate::now(tzone = "UTC"),
          updated_by = user_id
        )

        tryCatch({
          db_update_survey_fees(pool, new_fees_data)
          db_update_survey_fee_structure(pool, new_fee_structure_data)

          # Trigger a refresh of the database data
          db_refresh_trigger(db_refresh_trigger() + 1)
          db_trigger_func()

        }, error = function(e) {

          cli::cli_alert_danger(
            c(
              "An error occurred while saving the survey.fees and survey.fee_structures data.\n",
              "Details: {conditionMessage(e)}"
            )
          )

        }, finally = {
          shiny::removeModal()
        })

      })

      # refresh -------------------------------------------------------------------------------------------
      shiny::observeEvent(input$refresh, {
        db_refresh_trigger(db_refresh_trigger() + 1)
        db_trigger_func()
      })

      # return -------------------------------------------------------------------------------------------
      return(
        list(
          fees_data = fees_data,
          fee_structure_data = fee_structure_data
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
