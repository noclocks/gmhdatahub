
#  ------------------------------------------------------------------------
#
# Title : Survey Leasing Summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Leasing Summary Shiny Module
#'
#' @name mod_survey_leasing_summary
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Leasing Summary page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_leasing_summary_ui()`: User Interface (UI) for the module.
#' - `mod_survey_leasing_summary_server()`: Server logic for the module.
#' - `mod_survey_leasing_summary_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_leasing_summary_ui()`: UI output
#' - `mod_survey_leasing_summary_server()`: List of reactive expressions.
#' - `mod_survey_leasing_summary_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_leasing_summary_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_leasing_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_leasing_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  current_leasing_week <- get_leasing_week_start_date()
  min_leasing_week <- current_leasing_week - lubridate::years(1)
  max_leasing_week <- get_leasing_week_end_date()

  htmltools::tagList(
    bslib::card(
      # bslib::card_header(
      #   class = "d-flex justify-content-between align-items-center",
      #   htmltools::tags$h3(
      #     class = "m-0",
      #     shiny::textOutput(ns("property_name_title"))
      #   ),
      #   shiny::actionButton(
      #     ns("edit"),
      #     "Edit",
      #     icon = shiny::icon("edit"),
      #     class = "btn-sm btn-primary"
      #   )
      # ),
      bslib::card_body(
        class = "p-0",
        bslib::layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          bslib::card(
            shiny::selectInput(
              ns("reporting_cycle"),
              label = htmltools::tags$span(
                bsicons::bs_icon("calendar-week"),
                "Reporting Cycle"
              ),
              choices = get_survey_choices(section = "leasing_summary", type = "reporting_cycle"),
              selected = get_survey_choices(section = "leasing_summary", type = "reporting_cycle")[[3]]
            ),
            shiny::dateInput(
              ns("lease_launch_date"),
              label = htmltools::tags$span(
                bsicons::bs_icon("calendar-check"),
                "Lease Launch Date"
              ),
              value = NULL
            ),
            shiny::dateInput(
              ns("renewal_launch_date"),
              label = htmltools::tags$span(
                bsicons::bs_icon("calendar-plus"),
                "Renewal Launch Date"),
              value = NULL
            ),
            shiny::numericInput(
              ns("current_occupancy"),
              label = htmltools::tags$span(bsicons::bs_icon("percent"), "Current Occupancy (%)"),
              value = 0,
              min = 0,
              max = 100,
              step = 1
            ),
            shiny::numericInput(
              ns("prior_year_occupancy"),
              label = htmltools::tags$span(bsicons::bs_icon("clock-history"), "Prior Year Occupancy (%)"),
              value = 0,
              min = 0,
              max = 100,
              step = 1
            ),
            shiny::numericInput(
              ns("current_prelease"),
              label = htmltools::tags$span(bsicons::bs_icon("graph-up"), "Current Pre-Lease (%)"),
              value = 38.8
            ),
            shiny::numericInput(
              ns("last_year_prelease"),
              label = htmltools::tags$span(bsicons::bs_icon("clock-history"), "Prior Year Pre-Lease (%)"),
              value = 0,
              min = 0,
              max = 100,
              step = 1
            )
          ),
          bslib::card(
            shiny::numericInput(
              ns("total_renewals"),
              label = htmltools::tags$span(bsicons::bs_icon("arrow-repeat"), "Total Renewals"),
              value = 0,
              min = 0,
              step = 1
            ),
            shiny::numericInput(
              ns("total_new_leases"),
              label = htmltools::tags$span(bsicons::bs_icon("file-earmark-plus"), "Total New Leases"),
              value = 0,
              min = 0,
              step = 1
            ),
            shiny::numericInput(
              ns("weekly_traffic"),
              label = htmltools::tags$span(bsicons::bs_icon("people"), "Total Weekly Traffic"),
              value = NULL,
              min = 0,
              step = 1
            ),
            shiny::selectInput(
              ns("current_incentive"),
              label = htmltools::tags$span(bsicons::bs_icon("gift"), "Current Incentive"),
              choices = get_survey_choices("leasing_summary", "current_incentive"),
              selected = get_survey_choices("leasing_summary", "current_incentive")[[1]]
            ),
            shiny::numericInput(
              ns("incentive_amount"),
              label = htmltools::tags$span(bsicons::bs_icon("cash-stack"), "Incentive Amount ($)"),
              value = 0
            )
          ),
          shiny::dateInput(
            ns("data_last_updated"),
            label = htmltools::tags$span(bsicons::bs_icon("clock"), "Data Last Updated"),
            value = Sys.Date()
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_leasing_summary
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_leasing_summary_server <- function(
  id,
  pool = NULL,
  selected_property_id = NULL
) {

  # check database connection
  if (is.null(pool)) pool <- db_connect()
  check_db_conn(pool)

  # validation of reactives
  if (is.null(selected_property_id)) {
    property_id <- db_read_survey_property_ids(pool)
    selected_property_id <- shiny::reactive({ property_id })
  }

  leasing_week_tbl <- db_read_gmh_leasing_calendar(pool)
  leasing_week_rng <- c(leasing_week_tbl$leasing_week_start_date[[1]], leasing_week_tbl$leasing_week_end_date[[1]])
  selected_leasing_week <- shiny::reactive({ leasing_week_rng })

  stopifnot(
    shiny::is.reactive(selected_property_id),
    shiny::is.reactive(selected_leasing_week)
  )

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_leasing_summary_server()")

      db_refresh_trigger <- shiny::reactiveVal(0)

      leasing_data <- shiny::reactive({
        shiny::req(pool, selected_property_id())
        property_id <- selected_property_id()
        db_read_mkt_leasing_summary(pool, property_id) |>
          dplyr::filter(leasing_week_id == max(.data$leasing_week_id, na.rm = TRUE))
      }) |>
        shiny::bindEvent(selected_property_id(), db_refresh_trigger())

      # inputs_data <- shiny::reactive({
      #   tibble::tibble(
      #     property_id = selected_property_id(),
      #     leasing_week_id = get_leasing_wee
      #     reporting_cycle = input$reporting_cycle,
      #     lease_launch_date = input$lease_launch_date,
      #     renewal_launch_date = input$renewal_launch_date,
      #     current_occupancy = input$current_occupancy,
      #     prior_year_occupancy = input$prior_year_occupancy,
      #     current_prelease = input$current_prelease,
      #     last_year_prelease = input$last_year_prelease,
      #     total_renewals = input$total_renewals,
      #     total_new_leases = input$total_new_leases,
      #     weekly_traffic = input$weekly_traffic,
      #     current_incentive = input$current_incentive,
      #     incentive_amount = input$incentive_amount,
      #     data_last_updated = input$data_last_updated
      #   )

      return(
        list(

        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_leasing_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_leasing_summary_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Leasing Summary",
    window_title = "Demo: Survey Leasing Summary",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Leasing Summary",
      value = "survey_leasing_summary",
      icon = bsicons::bs_icon("house"),
      mod_survey_leasing_summary_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_leasing_summary_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------

