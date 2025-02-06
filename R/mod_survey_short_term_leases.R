#  ------------------------------------------------------------------------
#
# Title : Survey Short Term Leases Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Short Term Leases Shiny Module
#'
#' @name mod_survey_short_term_leases
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Short Term Leases page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_short_term_leases_ui()`: User Interface (UI) for the module.
#' - `mod_survey_short_term_leases_server()`: Server logic for the module.
#' - `mod_survey_short_term_leases_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_short_term_leases_ui()`: UI output
#' - `mod_survey_short_term_leases_server()`: List of reactive expressions.
#' - `mod_survey_short_term_leases_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_short_term_leases_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_short_term_leases
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_short_term_leases_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("5 Month Term"),
        bslib::card_body(
          shiny::textOutput(ns("five_month_available_text")),
          shiny::textOutput(ns("five_month_premium_text")),
          shiny::textOutput(ns("five_month_quantity_text"))
        )
      ),
      bslib::card(
        bslib::card_header("10 Month Term"),
        bslib::card_body(
          shiny::textOutput(ns("ten_month_available_text")),
          shiny::textOutput(ns("ten_month_premium_text")),
          shiny::textOutput(ns("ten_month_quantity_text"))
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_short_term_leases
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_short_term_leases_server <- function(
    id,
    pool = NULL,
    survey_data = NULL,
    selected_filters = NULL,
    edit_survey_section = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_short_term_leases_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      db_refresh_trigger <- shiny::reactiveVal(0)

      short_term_leases_data <- shiny::reactive({
        # db_read_mkt_short_term_leases(
        #   pool,
        #   property_id = selected_property_id(),
        #   leasing_week = session$userData$leasing_week()
        # )
      })

      output$five_month_available_text <- shiny::renderText({
        out <- short_term_leases_data() |>
          dplyr::pull("five_month_available")

        paste0("5 Month Term Available: ", out)
      })

      output$five_month_premium_text <- shiny::renderText({
        out <- short_term_leases_data() |>
          dplyr::pull("five_month_premium")

        paste0("5 Month Term Premium: ", out)
      })

      output$five_month_quantity_text <- shiny::renderText({
        out <- short_term_leases_data() |>
          dplyr::pull("five_month_quantity")

        paste0("5 Month Term Quantity: ", out)
      })

      output$ten_month_available_text <- shiny::renderText({
        out <- short_term_leases_data() |>
          dplyr::pull("ten_month_available")

        paste0("10 Month Term Available: ", out)
      })

      output$ten_month_premium_text <- shiny::renderText({
        out <- short_term_leases_data() |>
          dplyr::pull("ten_month_premium")

        paste0("10 Month Term Premium: ", out)
      })

      output$ten_month_quantity_text <- shiny::renderText({
        out <- short_term_leases_data() |>
          dplyr::pull("ten_month_quantity")

        paste0("10 Month Term Quantity: ", out)
      })

      inputs_data <- shiny::reactive({
        tibble::tibble(
          property_id = selected_property_id(),
          leasing_week = session$userData$leasing_week(),
          five_month_available = input$five_month_available,
          five_month_premium = input$five_month_premium,
          five_month_quantity = input$five_month_quantity,
          ten_month_available = input$ten_month_available,
          ten_month_premium = input$ten_month_premium,
          ten_month_quantity = input$ten_month_quantity
        )
      })

      shiny::observeEvent(edit_survey_section(), {
        if (session$userData$selected_survey_tab() != "nav_short_term_leases") {
          return()
        }

        data <- short_term_leases_data()

        # TODO:
        #   `iv` (?)

        shiny::showModal(
          shiny::modalDialog(
            title = "Short Term Leases",
            size = "l",
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shiny::checkboxInput(
                ns("five_month_available"),
                label = "5 Month Term Available",
                value = ifelse(
                  length(data$five_month_available) == 0,
                  FALSE,
                  data$five_month_available
                )
              ),
              shiny::numericInput(
                ns("five_month_premium"),
                label = "5 Month Term Premium",
                min = 0,
                max = Inf,
                step = 10,
                value = ifelse(
                  length(data$five_month_premium) == 0,
                  0,
                  data$five_month_premium
                )
              ) |> shinyjs::disabled(),
              shiny::numericInput(
                ns("five_month_quantity"),
                label = "5 Month Term Quantity",
                min = 0,
                max = Inf,
                step = 1,
                value = ifelse(
                  length(data$five_month_quantity) == 0,
                  0,
                  data$five_month_quantity
                )
              ) |> shinyjs::disabled(),
              shiny::checkboxInput(
                ns("ten_month_available"),
                label = "10 Month Term Available",
                value = ifelse(
                  length(data$ten_month_available) == 0,
                  FALSE,
                  data$ten_month_available
                )
              ),
              shiny::numericInput(
                ns("ten_month_premium"),
                label = "10 Month Term Premium",
                min = 0,
                max = Inf,
                step = 10,
                value = ifelse(
                  length(data$ten_month_premium) == 0,
                  0,
                  data$ten_month_premium
                )
              ) |> shinyjs::disabled(),
              shiny::numericInput(
                ns("ten_month_quantity"),
                label = "10 Month Term Quantity",
                min = 0,
                max = Inf,
                step = 1,
                value = ifelse(
                  length(data$ten_month_quantity) == 0,
                  0,
                  data$ten_month_quantity
                )
              ) |> shinyjs::disabled()
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

      shiny::observeEvent(input$five_month_available, {
        if (input$five_month_available) {
          shinyjs::enable("five_month_premium")
          shinyjs::enable("five_month_quantity")
        } else {
          shinyjs::disable("five_month_premium")
          shinyjs::disable("five_month_quantity")
        }
      })

      shiny::observeEvent(input$ten_month_available, {
        if (input$ten_month_available) {
          shinyjs::enable("ten_month_premium")
          shinyjs::enable("ten_month_quantity")
        } else {
          shinyjs::disable("ten_month_premium")
          shinyjs::disable("ten_month_quantity")
        }
      })

      shiny::observeEvent(input$save_changes, {
        shiny::req(pool, inputs_data(), selected_property_id(), session$userData$leasing_week())

        new_values <- inputs_data()

        db_update_mkt_short_term_leases(
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

#' @rdname mod_survey_short_term_leases
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_short_term_leases_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Short Term Leases",
    window_title = "Demo: Survey Short Term Leases",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Short Term Leases",
      value = "survey_short_term_leases",
      icon = bsicons::bs_icon("house"),
      mod_survey_short_term_leases_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_short_term_leases_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
