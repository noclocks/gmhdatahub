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
    # page --------------------------------------------------------------------
    bslib::page_fluid(
      # card --------------------------------------------------------------------
      bslib::card(
        full_screen = TRUE,
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("calendar"),
            htmltools::tags$span(
              "Short Term Leases - ",
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
        ),
        bslib::card_footer(
          shiny::textOutput(ns("last_updated_at"), inline = TRUE)
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
    map_data = NULL,
    selected_filters = NULL,
    db_trigger_func = NULL,
    edit_survey_section = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # setup ------------------------------------------------------------
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_short_term_leases_server()")

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
      short_term_leases_data <- shiny::reactive({
        shiny::req(survey_data$short_term_leases)

        if (nrow(survey_data$short_term_leases == 0)) {
          default_tbl_survey_short_term_leases()
        } else {
          survey_data$short_term_leases |>
            dplyr::select(
              term_months,
              is_available,
              premium,
              quantity
            )
        }
      })

      # outputs -----------------------------------------------------------------
      output$five_month_available_text <- shiny::renderText({
        val <- short_term_leases_data() |>
          dplyr::filter(.data$term_months == 5) |>
          dplyr::pull("is_available")

        paste0(
          "5 Month Term Available: ",
          ifelse(val, "Yes", "No")
        )
      })

      output$five_month_premium_text <- shiny::renderText({
        val <- short_term_leases_data() |>
          dplyr::filter(.data$term_months == 5) |>
          dplyr::pull("premium")

        paste0(
          "5 Month Term Premium: ",
          scales::dollar(val, accuracy = 0.01)
        )
      })

      output$five_month_quantity_text <- shiny::renderText({
        val <- short_term_leases_data() |>
          dplyr::filter(.data$term_months == 5) |>
          dplyr::pull("quantity")

        paste0(
          "5 Month Term Quantity: ",
          scales::comma(val)
        )
      })

      output$ten_month_available_text <- shiny::renderText({
        val <- short_term_leases_data() |>
          dplyr::filter(.data$term_months == 10) |>
          dplyr::pull("is_available")

        paste0(
          "10 Month Term Available: ",
          ifelse(val, "Yes", "No")
        )
      })

      output$ten_month_premium_text <- shiny::renderText({
        val <- short_term_leases_data() |>
          dplyr::filter(.data$term_months == 10) |>
          dplyr::pull("premium")

        paste0(
          "10 Month Term Premium: ",
          scales::dollar(val, accuracy = 0.01)
        )
      })

      output$ten_month_quantity_text <- shiny::renderText({
        val <- short_term_leases_data() |>
          dplyr::filter(.data$term_months == 10) |>
          dplyr::pull("quantity")

        paste0(
          "10 Month Term Quantity: ",
          scales::comma(val)
        )
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
        shiny::req(session$userData$selected_survey_tab())

        if (session$userData$selected_survey_tab() != "nav_short_term_leases") {
          return()
        }

        data <- short_term_leases_data()

        iv$initialize()
        iv$enable()

        shiny::showModal(
          shiny::modalDialog(
            title = "Short Term Leases",
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
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shiny::checkboxInput(
                ns("five_month_available"),
                label = "5 Month Term Available",
                value = data |>
                  dplyr::filter(.data$term_months == 5) |>
                  dplyr::pull("is_available")
              ),
              shiny::numericInput(
                ns("five_month_premium"),
                label = "5 Month Term Premium",
                value = data |>
                  dplyr::filter(.data$term_months == 5) |>
                  dplyr::pull("premium"),
                min = 0,
                step = 0.01
              ),
              shiny::numericInput(
                ns("five_month_quantity"),
                label = "5 Month Term Quantity",
                value = data |>
                  dplyr::filter(.data$term_months == 5) |>
                  dplyr::pull("quantity"),
                min = 0,
                step = 1
              ),
              shiny::checkboxInput(
                ns("ten_month_available"),
                label = "10 Month Term Available",
                value = data |>
                  dplyr::filter(.data$term_months == 10) |>
                  dplyr::pull("is_available")
              ),
              shiny::numericInput(
                ns("ten_month_premium"),
                label = "10 Month Term Premium",
                value = data |>
                  dplyr::filter(.data$term_months == 10) |>
                  dplyr::pull("premium"),
                min = 0,
                step = 0.01
              ),
              shiny::numericInput(
                ns("ten_month_quantity"),
                label = "10 Month Term Quantity",
                value = data |>
                  dplyr::filter(.data$term_months == 10) |>
                  dplyr::pull("quantity"),
                min = 0,
                step = 1
              )
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

        if (!is.na(selected_filters$leasing_week_id) && !is.null(selected_filters$leasing_week_id)) {
          week_id <- selected_filters$leasing_week_id
          week_date <- selected_filters$leasing_week_name
        } else {
          week_date <- get_leasing_week_start_date()
          week_id <- get_leasing_week_id_by_date(week_date)
        }

        initial_values <- survey_data$short_term_leases |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            leasing_week_id,
            term_months,
            is_available,
            premium,
            quantity,
            updated_by
          )

        new_values <- inputs_data() |>
          dplyr::mutate(
            property_id = as.integer(prop_id),
            competitor_id = as.integer(comp_id),
            property_name = prop_name,
            leasing_week_id = as.integer(week_id),
            updated_by = selected_filters$user_id
          ) |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            leasing_week_id,
            term_months,
            is_available,
            premium,
            quantity,
            updated_by
          )

        changed_values <- dplyr::anti_join(
          new_values,
          initial_values,
          by = c(
            "property_id",
            "competitor_id",
            "property_name",
            "leasing_week_id",
            "term_months",
            "is_available",
            "premium",
            "quantity",
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
              db_update_survey_short_term_leases(pool, changed_values)
              shiny::setProgress(value = 1, detail = "Changes saved.")
              db_trigger_func()
              shiny::removeModal()
            }
          )
        }
      })

      return(
        list(
          short_term_leases_data = short_term_leases_data
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
