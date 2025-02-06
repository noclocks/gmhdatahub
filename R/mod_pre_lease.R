#  ------------------------------------------------------------------------
#
# Title : Pre Lease Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Pre-Lease Shiny Module
#'
#' @name mod_pre_lease
#'
#' @description
#' A Shiny Module for the GMH Communities Pre-Lease Summary & Details reports
#' data visualization and review.
#'
#' The following functions are implemented:
#'
#' - `mod_pre_lease_ui()`: User interface (UI) definition
#' - `mod_pre_lease_server()`: Server logic
#' - `mod_pre_lease_demo()`: Demo application for the module
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#' @param selected_property Selected property ID.
#' @param report_date Report date.
#'
#' @returns
#' - `mod_pre_lease_ui()`: UI HTML Output.
#' - `mod_pre_lease_server()`: List of reactive values.
#' - `mod_pre_lease_demo()`: `NULL`, used for the side-effect of a demo app.
#'
#' @examplesIf interactive()
#' mod_pre_lease_demo()
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid layout_columns value_box card card_header card_body card_footer navset_card_underline nav_panel
#' @importFrom htmltools tags
#' @importFrom plotly plotlyOutput
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS textOutput downloadButton
mod_pre_lease_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::navset_card_underline(
      id = ns("nav"),
      bslib::nav_panel(
        title = "Summary",
        icon = bsicons::bs_icon("clipboard-data"),
        value = ns("summary"),
        bslib::card(
          full_screen = TRUE,
          height = "100%",
          class = "mb-4",
          bslib::card_header(
            class = "bg-dark text-white d-flex justify-content-between align-items-center",
            htmltools::div(
              bsicons::bs_icon("table", class = "me-2"),
              "Pre-Lease Summary"
            ),
            bslib::input_task_button(
              id = ns("refresh"),
              icon = bsicons::bs_icon("arrow-clockwise"),
              label = "Refresh",
              class = "btn-info btn-sm",
              style = "margin-left: auto; display: inline-flex; align-items: center; padding: 2.5px 10px;"
            ),
            shiny::downloadButton(
              ns("download"),
              "Export to Excel",
              class = "btn-success btn-sm",
              style = "margin-left: 10px; display: inline-flex; align-items: center; padding: 2.5px 10px;"
            )
          ),
          bslib::card_body(
            class = "p-0",
            reactable::reactableOutput(ns("summary_table"))
          )
        )
      ),
      bslib::nav_panel(
        title = "Charts",
        icon = bsicons::bs_icon("bar-chart"),
        value = "charts",
        bslib::layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          bslib::card(
            full_screen = TRUE,
            class = "h-100",
            bslib::card_header(
              class = "bg-dark text-white",
              htmltools::div(
                bsicons::bs_icon("bar-chart", class = "me-2"),
                "Occupancy vs. Target"
              )
            ),
            bslib::card_body(
              class = "p-3",
              plotly::plotlyOutput(ns("occupancy_chart"), height = "400px")
            )
          ),
          bslib::card(
            full_screen = TRUE,
            class = "h-100",
            bslib::card_header(
              class = "bg-dark text-white",
              htmltools::div(
                bsicons::bs_icon("pie-chart", class = "me-2"),
                "New vs. Renewal Distribution"
              )
            ),
            bslib::card_body(
              class = "p-3",
              plotly::plotlyOutput(ns("lease_type_chart"), height = "400px")
            )
          )
        )
      ),
      bslib::nav_panel(
        title = "About",
        icon = bsicons::bs_icon("info-circle"),
        value = "about",
        bslib::card(
          class = "mb-4",
          bslib::card_header(
            class = "bg-dark text-white",
            htmltools::div(
              bsicons::bs_icon("info-circle", class = "me-2"),
              "About"
            )
          ),
          bslib::card_body(
            htmltools::tags$p(
              bsicons::bs_icon("clipboard-data", class = "me-2"),
              "This pre-lease summary dashboard provides a comprehensive overview of property leasing performance across multiple locations.
              The data includes detailed metrics on current occupancy, lease types, and velocity targets.",
              class = "mb-3"
            ),
            htmltools::tags$ul(
              class = "list-unstyled",
              htmltools::tags$li(bsicons::bs_icon("house-check", class = "me-2"), "Current Occupancy: Shows the current percentage of occupied beds", class = "mb-2"),
              htmltools::tags$li(bsicons::bs_icon("file-earmark-text", class = "me-2"), "Lease Types: Breakdown between new leases and renewals", class = "mb-2"),
              htmltools::tags$li(bsicons::bs_icon("graph-up", class = "me-2"), "YOY Performance: Year-over-year comparison of leasing metrics", class = "mb-2"),
              htmltools::tags$li(bsicons::bs_icon("speedometer", class = "me-2"), "Velocity Targets: Progress towards 90%, 95%, and 100% occupancy goals")
            )
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_pre_lease_server <- function(
    id,
    pool = NULL,
    selected_property_id = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_server()")

      # database
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # handle selected properties
      if (is.null(selected_property_id)) {
        prop_id <- get_property_id_by_name("1047 Commonwealth Avenue")
        selected_property_id <- shiny::reactive({
          prop_id
        })
        selected_property_name <- shiny::reactive({
          get_property_name_by_id(prop_id)
        })
      }

      # initial data
      pre_lease_summary_data <- shiny::reactive({
        db_read_gmh_pre_lease_summary_tbl(pool)
      })

      # summary table
      output$summary_table <- reactable::renderReactable({
        shiny::req(pre_lease_summary_data())
        tbl_pre_lease_summary(pre_lease_summary_data())
      })

      # occupancy chart
      output$occupancy_chart <- plotly::renderPlotly({
        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data()

        plot_height <- 400

        p <- plotly::plot_ly(
          data = df,
          height = plot_height
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_occupancy,
            type = "bar",
            name = "Current Occupancy %",
            marker = list(color = "#2C3E50")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~ rep(0.90, nrow(df)),
            type = "scatter",
            mode = "lines",
            name = "90% Target",
            line = list(color = "#E74C3C", dash = "dash")
          ) |>
          plotly::layout(
            autosize = TRUE,
            height = plot_height,
            margin = list(
              l = 50,
              r = 50,
              b = 100,
              t = 30,
              pad = 4
            ),
            bargap = 0.2,
            xaxis = list(
              tickangle = 45,
              automargin = TRUE,
              title = list(text = "Property")
            ),
            yaxis = list(
              tickformat = ".0%",
              automargin = TRUE,
              title = list(text = "Occupancy %"),
              range = c(0, 1)
            ),
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              y = -0.2
            ),
            barmode = "group"
          )

        p
      })

      # lease type chart
      output$lease_type_chart <- plotly::renderPlotly({
        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data()

        p <- plotly::plot_ly(
          data = df,
          height = 500 # Explicit height
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_total_new,
            type = "bar",
            name = "New Leases",
            marker = list(color = "#2C3E50")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_total_renewals,
            type = "bar",
            name = "Renewals",
            marker = list(color = "#18BC9C")
          ) |>
          plotly::layout(
            autosize = TRUE,
            margin = list(
              l = 50,
              r = 50,
              b = 100,
              t = 50,
              pad = 4
            ),
            xaxis = list(
              tickangle = 45,
              automargin = TRUE
            ),
            yaxis = list(
              automargin = TRUE
            ),
            showlegend = TRUE,
            barmode = "stack"
          )

        p
      })

      output$download <- shiny::downloadHandler(
        filename = function() {
          paste0(Sys.Date(), "Pre-Lease-Summary-Table.xlsx")
        },
        content = function(file) {
          df <- pre_lease_summary_data() |>
            dplyr::collect()

          writexl::write_xlsx(
            x = df,
            path = file
          )
        }
      )

      return(
        list(
          pre_lease_summary_data = shiny::reactive({
            pre_lease_summary_data()
          })
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_pre_lease_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Pre Lease",
    window_title = "Demo: Pre Lease",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Pre Lease",
      value = "pre_lease",
      icon = bsicons::bs_icon("house"),
      mod_pre_lease_ui("demo")
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    mod_pre_lease_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)
}
