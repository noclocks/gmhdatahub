#  ------------------------------------------------------------------------
#
# Title : Dashboard Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Dashboard Shiny Module
#'
#' @name mod_dashboard
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Dashboard page.
#'
#' Includes the following functions:
#'
#' - `mod_dashboard_ui()`: User Interface (UI) for the module.
#' - `mod_dashboard_server()`: Server logic for the module.
#' - `mod_dashboard_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_dashboard_ui()`: UI output
#' - `mod_dashboard_server()`: List of reactive expressions.
#' - `mod_dashboard_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_dashboard_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::navset_card_underline(
      id = ns("nav"),
      bslib::nav_panel(
        title = icon_text("dashboard", "Overview"),
        bslib::page_fluid(
          bslib::card(
            full_screen = TRUE,
            reactable::reactableOutput(ns("pre_lease_summary_table")) |>
              with_loader()
          )
        )
      ),
      bslib::nav_panel(
        title = icon_text("bar-chart", "Charts"),
        bslib::page_fluid(
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::selectInput(
                ns("metric"),
                "Metric",
                choices = c("Leases" = "leases", "Renewals" = "renewals", "Pre-Lease %" = "prelease"),
                selected = "leases"
              ),
              shiny::sliderInput(
                ns("occupancy_target"),
                "Occupancy Target %",
                min = 0,
                max = 1,
                value = 0.95,
                step = 0.05,
                ticks = TRUE,
                post = "%"
              ),
              shiny::radioButtons(
                ns("group_by"),
                "Group By",
                choices = c("Property" = "property", "Investment Partner" = "partner"),
                selected = "property"
              )
            ),
            bslib::layout_columns(
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("yoy_variance_chart"),
                  height = "400px"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("current_vs_prior_chart"),
                  height = "400px"
                )
              ),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("occupancy_chart"),
                  height = "400px"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("velocity_chart"),
                  height = "400px"
                )
              ),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("pre_lease_rates_chart"),
                  height = "400px"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("partner_distribution_chart"),
                  height = "400px"
                )
              ),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("portfolio_summary_chart"),
                  height = "400px"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("weekly_activity_chart"),
                  height = "400px"
                )
              ),
              bslib::card(
                full_screen = TRUE,
                apexcharter::apexchartOutput(
                  ns("weekly_leasing_breakdown_chart"),
                  height = "400px"
                )
              )
            )
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_dashboard_server <- function(
    id,
    pool = NULL,
    global_filters = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_dashboard_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      summary_data <- shiny::reactive({
        db_read_gmh_pre_lease_summary_tbl(
          pool = pool,
          report_date = NULL,
          property_ids = NULL
        )
      })

      output$pre_lease_summary_table <- reactable::renderReactable({
        shiny::req(summary_data())
        tbl_pre_lease_summary(summary_data())
      })

      pre_lease_data <- shiny::reactive({
        shiny::req(summary_data())
        summary_data()
        # db_read_gmh_pre_lease_summary_tbl(pool)
      })

      # current vs prior
      output$current_vs_prior_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data(), input$metric)
        chart_current_vs_prior(data = pre_lease_data(), metric = input$metric)
      })

      # occupancy
      output$occupancy_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data(), input$occupancy_target, input$group_by)
        chart_occupancy(
          data = pre_lease_data(),
          target = input$occupancy_target,
          by = input$group_by
        )
      })

      # velocity
      output$velocity_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data())
        chart_velocity_comparison(data = pre_lease_data())
      })

      # pre-lease rates
      output$pre_lease_rates_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data())
        chart_pre_lease_rates(data = pre_lease_data())
      })

      # partner distribution
      output$partner_distribution_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data())
        chart_partner_distribution(data = pre_lease_data())
      })

      # portfolio summary
      output$portfolio_summary_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data())
        chart_portfolio_summary(data = pre_lease_data())
      })

      # weekly activity
      output$weekly_activity_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data())
        chart_weekly_activity(data = pre_lease_data())
      })

      # weekly leasing breakdown
      output$weekly_leasing_breakdown_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data())
        chart_weekly_leasing_breakdown(data = pre_lease_data())
      })

      # yoy variance
      output$yoy_variance_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_data(), input$group_by)
        chart_yoy_variance(data = pre_lease_data(), by = input$group_by)
      })

      return(
        list(
          summary_data = summary_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_dashboard
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_dashboard_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Dashboard",
    window_title = "Demo: Dashboard",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Dashboard",
      value = "dashboard",
      icon = bsicons::bs_icon("house"),
      mod_dashboard_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_dashboard_server("demo")
  }

  shiny::shinyApp(ui, server)
}
