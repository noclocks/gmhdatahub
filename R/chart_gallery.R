
#  ------------------------------------------------------------------------
#
# Title : Chart Gallery Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

#' Chart Gallery App Module

mod_chart_gallery_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::page_fluid(
    title = "Chart Gallery",
    theme = bslib::bs_theme(version = 5),
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
}

mod_chart_gallery_server <- function(id, pool = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cli_rule("[Module]: mod_chart_gallery_server()")

      if (is.null(pool)) {
        pool <- db_connect()
      }

      # get pre-lease data
      pre_lease_data <- shiny::reactive({
        shiny::req(pool)
        db_read_gmh_pre_lease_summary_tbl(pool)
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

    }
  )

}

mod_chart_gallery_demo <- function() {
  ui <- mod_chart_gallery_ui("chart_gallery")
  server <- function(input, output, session) {
    pool <- db_connect()
    mod_chart_gallery_server("chart_gallery", pool = pool)
  }
  shiny::shinyApp(ui, server)
}


