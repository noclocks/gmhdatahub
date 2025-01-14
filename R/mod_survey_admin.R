
#  ------------------------------------------------------------------------
#
# Title : Survey Admin Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Admin Shiny Module
#'
#' @name mod_survey_admin
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Admin page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_admin_ui()`: User Interface (UI) for the module.
#' - `mod_survey_admin_server()`: Server logic for the module.
#' - `mod_survey_admin_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_admin_ui()`: UI output
#' - `mod_survey_admin_server()`: List of reactive expressions.
#' - `mod_survey_admin_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_admin_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_admin
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_admin_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box(
          title = "Total Properties",
          value = shiny::textOutput(ns("val_properties")),
          showcase = bsicons::bs_icon("buildings"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Total Competitors",
          value = shiny::textOutput(ns("val_competitors")),
          showcase = bsicons::bs_icon("building"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Total Surveys",
          value = shiny::textOutput(ns("val_surveys")),
          showcase = bsicons::bs_icon("clipboard"),
          theme = "primray"
        )
      ),
      bslib::layout_column_wrap(
        width = 1/3,
        actionButton(ns("add_property"), "Add New Property", icon = shiny::icon("plus"), class = "btn-success"),
        actionButton(ns("add_competitor"), "Add New Competitor", icon = shiny::icon("plus"), class = "btn-primary"),
        actionButton(ns("create_survey"), "Create New Survey", icon = shiny::icon("plus"), class = "btn-info")
      ),
      bslib::navset_card_underline(
        id = ns("nav"),
        bslib::nav_panel(
          title = icon_text("map", "Property Map"),
          bslib::card(
            bslib::card_header("Property Map"),
            bslib::card_body(
              leaflet::leafletOutput(ns("property_map"))
            )
          )
        ),
        bslib::nav_panel(
          title = icon_text("dashboard", "Overview"),
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              bslib::card_header("Properties and Competitors"),
              bslib::card_body(
                # reactable::reactableOutput(ns("properties_table"))
              )
            ),
            bslib::card(
              bslib::card_header("Survey Status"),
              bslib::card_body(
                # reactable::reactableOutput(ns("survey_status_table"))
              )
            )
          ),
          bslib::layout_column_wrap(
            width = 1,
            bslib::card(
              bslib::card_header("Survey Timeline"),
              bslib::card_body(
                # plotly::plotlyOutput(ns("survey_timeline"))
              )
            )
          )
        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_survey_admin
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_admin_server <- function(
  id,
  pool = NULL,
  global_filters = NULL
) {

  # check database connection
  if (is.null(pool)) pool <- db_connect()
  check_db_conn(pool)

  # validation of reactives
  if (!is.null(global_filters)) {
    stopifnot(shiny::is.reactive(global_filters))
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_admin_server()")

      # Reactive values and observers
      properties_data <- shiny::reactive({
        # shiny::req(pool)
        db_read_tbl(pool, "mkt.properties")
      })

      survey_data <- shiny::reactive({
        # shiny::req(pool)
        db_read_tbl(pool, "mkt.surveys")
      })

      universities_data <- shiny::reactive({
        # shiny::req(pool)
        unis <- db_read_tbl(pool, "mkt.universities")
        locs <- db_read_tbl(pool, "mkt.university_locations")
        dplyr::left_join(
          unis,
          locs,
          by = c("university_id" = "university_id")
        ) |>
          dplyr::select(-coordinates)
      })

      map_data <- shiny::reactive({
        # shiny::req(pool)
        properties <- db_read_tbl(pool, "mkt.locations") |>
          dplyr::filter(.data$is_competitor == FALSE)
        competitors <- db_read_tbl(pool, "mkt.locations") |>
          dplyr::filter(.data$is_competitor == TRUE)
        universities <- universities_data()

        list(
          properties = properties,
          competitors = competitors,
          universities = universities
        )
      })

      # Value box outputs
      output$val_properties <- shiny::renderText({
        properties_data()$property_id |>
          length() |>
          scales::comma()
      })

      output$val_competitors <- shiny::renderText({
        map_data()$competitors |>
          nrow() |>
          scales::comma()
      })

      output$val_surveys <- shiny::renderText({
        survey_data()$survey_id |>
          length() |>
          scales::comma()
      })

      # Action button observers
      shiny::observeEvent(input$add_property, {
        # show_add_property_modal()
      })
      shiny::observeEvent(input$add_competitor, {
        # show_add_competitor_modal()
      })
      shiny::observeEvent(input$create_survey, {
        # show_create_survey_modal()
      })

      output$properties_table <- reactable::renderReactable({
        # tbl_survey_properties(properties_data())
      })

      output$survey_status_table <- reactable::renderReactable({
        # tbl_survey_status(survey_data())
      })

      output$property_map <- leaflet::renderLeaflet({

        properties <- map_data()$properties
        competitors <- map_data()$competitors
        universities <- map_data()$universities

        map_survey_locations(
          properties = properties,
          competitors = competitors,
          universities = universities
        )
      })

      output$survey_timeline <- plotly::renderPlotly({
        # plot_survey_timeline(survey_data())
      })

      return(
        list(
          properties_data = properties_data,
          survey_data = survey_data,
          universities_data = universities_data,
          map_data = map_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_admin
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_admin_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Admin",
    window_title = "Demo: Survey Admin",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Admin",
      value = "survey_admin",
      icon = bsicons::bs_icon("house"),
      mod_survey_admin_ui("demo")
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    mod_survey_admin_server("demo", pool)
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------



# bslib::navset_card_underline(
#   id = ns("survey_admin_nav"),
#   title = htmltools::tags$span(bsicons::bs_icon("clipboard"), "Survey Admin"),
#   footer = NULL,
#
#   bslib::nav_panel(
#     title = "Properties Overview",
#     icon = bsicons::bs_icon("buildings"),
#     value = ns("properties_overview"),
#
#     bslib::layout_columns(
#       col_widths = c(8, 4),
#       bslib::card(
#         bslib::card_header("Properties & Competitors"),
#         bslib::card_body(
#           reactable::reactableOutput(ns("properties_competitors_table")) |>
#             with_loader()
#         )
#       ),
#       bslib::card(
#         bslib::card_header("Map"),
#         bslib::card_body(
#           leaflet::leafletOutput(ns("properties_map")) |>
#             with_loader()
#         )
#       )
#     )
#   ),
#
#   bslib::nav_panel(
#     title = "Survey Status",
#     icon = bsicons::bs_icon("clipboard"),
#     value = ns("survey_status"),
#
#
#
#   )
#   bslib::layout_columns(
#     col_widths = c(12),
#     bslib::card(
#       full_screen = TRUE,
#
#     )
#   )
# ),
#
#
#
# # actions
# bslib::card(
#   bslib::card_header("Actions"),
#   bslib::card_body(
#     shiny::actionButton(
#       ns("new_property"),
#       "Add New Property",
#       icon = shiny::icon("plus"),
#       class = "btn-success"
#     ),
#     shiny::actionButton(
#       ns("new_competitor"),
#       "Add New Competitor",
#       icon = shiny::icon("plus"),
#       class = "btn-primary"
#     ),
#     shiny::actionButton(
#       ns("new_survey"),
#       "Log Weekly Data",
#       icon = shiny::icon("plus"),
#       class = "btn-info"
#     )
#   )
# ),
# # nav card for survey admin
#
#
# bslib::nav_panel(
#   title = "Charts",
#   value = ns("charts"),
#   icon = bsicons::bs_icon("bar-chart"),
#   bslib::layout_columns(
#     col_widths = c(6, 6),
#     bslib::card(
#       full_screen = TRUE,
#       bslib::card_body(
#         apexcharter::apexchartOutput(ns("survey_rate_adjustments_chart")) |>
#           with_loader()
#       )
#     ),
#     bslib::card(
#       full_screen = TRUE,
#       bslib::card_body(
#         apexcharter::apexchartOutput(ns("survey_market_velocity_chart")) |>
#           with_loader()
#       )
#     )
#   )
# ),
# bslib::nav_panel(
#   title = "Survey Data",
#   value = ns("survey_data"),
#   icon = bsicons::bs_icon("file-text"),
#   bslib::card(
#     full_screen = TRUE,
#     bslib::card_body(
#       reactable::reactableOutput(ns("survey_data_table")) |>
#         with_loader()
#     )
#   )
# )
#
#   )
# )
