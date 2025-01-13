
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

  bslib::navset_card_underline(
    id = ns("nav"),
    bslib::nav_panel(
      title = icon_text("dashboard", "Overview")#,
      # mod_dashboard_overview_ui(ns("overview"))
    ),
    bslib::nav_panel(
      title = icon_text("bar-chart", "Charts")#,
      # mod_dashboard_charts_ui(ns("charts"))
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

  # check database connection
  if (is.null(pool)) pool <- session$userData$pool
  check_db_conn(pool)

  # validation of reactives
  if (!is.null(global_filters)) {
    stopifnot(shiny::is.reactive(global_filters))
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_dashboard_server()")

      summary_data <- shiny::reactive({

        filter_property_ids <- global_filters$properties

        dplyr::tbl(pool, I("gmh.pre_lease_summary")) |>
          dplyr::filter(
            report_date == max(.data$report_date, na.rm = TRUE),
            property_id %in% filter_property_ids
          ) |>
          dplyr::collect()

      }) |>
        shiny::bindEvent(global_filters$properties)

      return(
        list(
          # reactive values
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

# overview ----------------------------------------------------------------


mod_dashboard_overview_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          icon_text("buildings", "Property Summary Metrics", .function = bsicons::bs_icon),
          class = "bg-primary text-white"
        ),
        reactable::reactableOutput(ns("property_summary_table")) |>
          with_loader()
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          icon_text("house-check-fill", "Occupancy by Property", .function = bsicons::bs_icon),
          class = "bg-primary text-white"
        ),
        plotly::plotlyOutput(ns("occupancy_plot")) |>
          with_loader()
      )
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          # icon for velocity
          icon_text("speedometer", "Leasing Velocity"),
          class = "bg-primary text-white"
        ),
        plotly::plotlyOutput(ns("velocity_plot")) |>
          with_loader()
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          # weekly performance
          icon_text("calendar-week", "Weekly Performance"),
          class = "bg-primary text-white"
        ),
        plotly::plotlyOutput(ns("weekly_plot")) |>
          with_loader()
      )
    )
  )

}

mod_dashboard_overview_server <- function(id, summary_data) {

  # validate reactives
  stopifnot(shiny::is.reactive(summary_data))

  # module
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cli::cli_rule("[Module]: Dashboard - Overview")

    # property summary table
    output$property_summary_table <- reactable::renderReactable({
      shiny::req(summary_data())

      data <- summary_data() |>
        dplyr::select(
          "property_name",
          "total_beds",
          "current_occupied",
          "current_preleased_percent",
          "weekly_total",
          "beds_left"
        ) |>
        dplyr::arrange(dplyr::desc(.data$total_beds))

      reactable::reactable(
        data,
        compact = TRUE,
        searchable = TRUE,
        striped = TRUE,
        defaultPageSize = 5,
        columns = list(
          property_name = reactable::colDef(name = "Property Name"),
          total_beds = reactable::colDef(name = "Total Beds"),
          current_occupied = reactable::colDef(name = "Occupied Beds"),
          current_preleased_percent = reactable::colDef(
            name = "Pre-leased %",
            format = reactable::colFormat(percent = TRUE, digits = 1)
          ),
          weekly_total = reactable::colDef(name = "Weekly Leases"),
          beds_left = reactable::colDef(name = "Beds Available")
        )
      )
    })

    # occupancy plot
    output$occupancy_plot <- plotly::renderPlotly({
      shiny::req(summary_data())
      data <- summary_data()

      plotly::plot_ly(
        data,
        x = ~property_name,
        y = ~current_occupancy,
        type = "bar",
        name = "Current Occupancy (%)"
      ) |>
        plotly::layout(
          title = "Occupancy Rate by Property",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "Occupancy Rate (%)", tickformat = ",.1%"),
          margin = list(b = 120)
        )
    })

    # velocity plot
    output$velocity_plot <- plotly::renderPlotly({
      shiny::req(summary_data())
      data <- summary_data()

      plotly::plot_ly(data) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~vel_90,
          name = "90% Velocity",
          type = "bar"
        ) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~vel_95,
          name = "95% Velocity",
          type = "bar"
        ) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~vel_100,
          name = "100% Velocity",
          type = "bar"
        ) |>
        plotly::layout(
          title = "Velocity by Property",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "Velocity % (Beds/Week Needed)"),
          margin = list(b = 120)
        )

    })

    # weekly plot
    output$weekly_plot <- plotly::renderPlotly({
      shiny::req(summary_data())
      data <- summary_data()

      plotly::plot_ly(data) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~weekly_new,
          name = "New Leases",
          type = "bar"
        ) |>
        plotly::add_trace(
          x = ~property_name,
          y = ~weekly_renewal,
          name = "Renewals",
          type = "bar"
        ) |>
        plotly::layout(
          barmode = "stack",
          yaxis = list(title = "Weekly Leases"),
          xaxis = list(title = "", tickangle = 45),
          margin = list(b = 120)
        )

    })

  })
}


# details -----------------------------------------------------------------

# Details Module UI
mod_dashboard_details_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      bslib::card_header("Property Metrics"),
      bslib::layout_columns(
        bslib::value_box(
          title = "Total Beds",
          value = shiny::textOutput(ns("total_beds")),
          showcase = bsicons::bs_icon("building")
        ),
        bslib::value_box(
          title = "Current Occupancy",
          value = shiny::textOutput(ns("occupancy_rate")),
          showcase = bsicons::bs_icon("percent")
        ),
        bslib::value_box(
          title = "Weekly Leases",
          value = shiny::textOutput(ns("weekly_leases")),
          showcase = bsicons::bs_icon("graph-up")
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header("Detailed Metrics"),
          reactable::reactableOutput(ns("details_table"))
        )
      )
    )
  )

}

# Details Module Server
mod_dashboard_details_server <- function(id, property_data) {

  # validate reactives
  stopifnot(shiny::is.reactive(property_data))

  shiny::moduleServer(id, function(input, output, session) {

    output$total_beds <- shiny::renderText({
      shiny::req(property_data())
      sum(property_data()$total_beds) |>
        format_number()
    })

    output$occupancy_rate <- shiny::renderText({
      shiny::req(property_data())
      scales::percent(mean(property_data()$current_occupancy), accuracy = 0.1)
    })

    output$weekly_leases <- shiny::renderText({
      shiny::req(property_data())
      sum(property_data()$weekly_total) |>
        format_currency()
    })

    output$details_table <- reactable::renderReactable({
      shiny::req(property_data())
      data <- property_data() |>
        dplyr::select(
          property_name,
          current_total_new,
          current_total_renewals,
          current_total_leases,
          prior_total_new,
          prior_total_renewals,
          prior_total_leases,
          yoy_variance_count,
          yoy_variance_percent
        )

      reactable::reactable(
        data,
        columns = list(
          property_name = reactable::colDef(name = "Property Name"),
          current_total_new = reactable::colDef(name = "Current New"),
          current_total_renewals = reactable::colDef(name = "Current Renewals"),
          current_total_leases = reactable::colDef(name = "Current Total"),
          prior_total_new = reactable::colDef(name = "Prior New"),
          prior_total_renewals = reactable::colDef(name = "Prior Renewals"),
          prior_total_leases = reactable::colDef(name = "Prior Total"),
          yoy_variance_count = reactable::colDef(name = "YOY Variance Count"),
          yoy_variance_percent = reactable::colDef(
            name = "YOY Variance %",
            format = reactable::colFormat(percent = TRUE, digits = 1)
          )
        ),
        compact = TRUE,
        filterable = TRUE
      )
    })
  })
}


db_get_latest_pre_lease_summary <- function(pool) {

  check_db_conn(pool)

  dplyr::tbl(pool, I("gmh.pre_lease_summary")) |>
    dplyr::filter(
      report_date == max(.data$report_date, na.rm = TRUE)
    )

}
