#  ------------------------------------------------------------------------
#
# Title : Survey Trends Analysis Module
#    By : Jimmy Briggs
#  Date : 2024-11-20
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Market Survey Trends Module
#'
#' @name mod_survey_insights_trends
#'
#' @description
#' This module provides insights into market survey trends, including occupancy rates
#' and rent comparisons.
#'
#' It includes the following functions:
#'
#' - `mod_survey_insights_trends_ui()`: UI function for the trends insights module.
#' - `mod_survey_insights_trends_server()`: Server function for the trends insights module.
#' - `mod_survey_insights_trends_demo()`: Run a demo app of the module.
#'
#' @param id The module ID.
#'
#' @returns
#' - `mod_survey_insights_trends_ui()`: UI function for the trends insights module.
#' - `mod_survey_insights_trends_server()`: Server function for the trends insights module.
#' - `mod_survey_insights_trends_demo()`: Run a demo app of the module.
#'
#' @seealso
#' [mod_survey_insights()]
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_survey_insights_trends
#' @export
#' @importFrom bslib nav_panel layout_columns card card_header
mod_survey_insights_trends_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("calendar"),
            "Rent Comparison"
          ),
          bslib::card_body(
            apexcharter::apexchartOutput(ns("rent_comparison"))
          )
        ),
        bslib::card(
          bslib::card_header(
            bsicons::bs_icon("bar-chart-line"),
            "Occupancy Comparison"
          ),
          bslib::card_body(
            apexcharter::apexchartOutput(ns("occupancy_chart"))
          )
        )
      )
    )
  )
}

# server ------------------------------------------------------------------

#' @rdname mod_survey_insights_trends
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_insights_trends_server <- function(
    id,
    pool = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_insights_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      market_data <- shiny::reactive({
        tibble::tibble(
          property = c("Property A", "Property B", "Property C", "Property D"),
          date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01", "2023-01-01")),
          market_rent = c(1500, 1600, 1400, 1700),
          effective_rent = c(1450, 1550, 1350, 1650),
          occupancy = c(95, 92, 98, 94),
          units = c(200, 150, 300, 250)
        )
      })

      output$rent_comparison <- apexcharter::renderApexchart({
        apexcharter::apex(
          data = market_data(),
          mapping = apexcharter::aes(x = property, y = market_rent),
          type = "column"
        ) |>
          apexcharter::ax_title(text = "Market vs Effective Rent") |>
          apexcharter::ax_series(list(
            name = "Market Rent",
            data = market_data()$market_rent
          )) |>
          apexcharter::ax_series(list(
            name = "Effective Rent",
            data = market_data()$effective_rent
          )) |>
          apexcharter::ax_xaxis(title = list(text = "Property")) |>
          apexcharter::ax_yaxis(title = list(text = "Rent ($)"))
      })

      output$occupancy_chart <- apexcharter::renderApexchart({
        apexcharter::apex(
          data = market_data(),
          mapping = apexcharter::aes(x = property, y = occupancy),
          type = "column"
        ) |>
          apexcharter::ax_title(text = "Occupancy Rates") |>
          apexcharter::ax_xaxis(title = list(text = "Property")) |>
          apexcharter::ax_yaxis(title = list(text = "Occupancy (%)"))
      })

      return(
        list(
          market_data = market_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

mod_survey_insights_trends_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_survey_insights_trends_ui("demo")
  )

  server <- function(input, output, session) {
    mod_survey_insights_trends_server("demo")
  }

  shiny::shinyApp(ui, server)
}
