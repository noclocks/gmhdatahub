#  ------------------------------------------------------------------------
#
# Title : Home Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Home Shiny Module
#'
#' @name mod_home
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Home page.
#'
#' - `mod_home_ui()`: The front-end UI for the module.
#' - `mod_home_server()`: The server logic for the module.
#' - `mod_home_demo()`: Function to demo the module in an isolated shiny app context.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_home_ui()`: UI output
#' - `mod_home_server()`: List of reactive expressions.
#' - `mod_home_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_home_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_home
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    # welcome/banner card
    bslib::card(
      id = ns("hero_section"),
      class = "mb-4",
      height = "250px",
      background = "linear-gradient(135deg, #1e4d92 0%, #2c3e50 100%)",
      style = "border: none; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
      htmltools::tags$div(
        class = "d-flex flex-column justify-content-center h-100 p-5",
        style = paste0(
          "background: linear-gradient(135deg, ",
          gmh_colors("primary"),
          " 0%,",
          gmh_colors("primary"),
          " 100%);"
        ),
        htmltools::tags$div(
          class = "mb-2",
          htmltools::tags$span(
            bsicons::bs_icon("building-check", size = "2rem"),
            style = "opacity: 0.9; color: rgba(255, 255, 255, 0.9);"
          )
        ),
        htmltools::tags$h1(
          "Welcome to GMH DataHub",
          class = "display-4 fw-bold mb-3 text-white",
          style = "letter-spacing: -0.02em;"
        ),
        htmltools::tags$p(
          class = "lead mb-4 text-white",
          style = "font-size: 1.3rem; max-width: 800px; opacity: 0.9;",
          "Your centralized platform for leasing data management and analytics. ",
          "Transform property insights into strategic decisions with our comprehensive dashboard suite."
        ),
        htmltools::tags$hr(),
        htmltools::tags$div(
          class = "mt-2",
          shiny::actionButton(
            ns("explore"),
            htmltools::tags$span(
              "Explore Platform Features ",
              bsicons::bs_icon("arrow-right")
            ),
            class = "btn-light btn-lg",
            style = "font-weight: 500; padding: 0.8rem 1.5rem;"
          )
        )
      )
    ),

    # key metrics
    bslib::layout_columns(
      id = ns("metrics_section"),
      fill = FALSE,
      bslib::value_box(
        id = ns("val_active_properties"),
        title = "Active Properties",
        value = shiny::textOutput(ns("active_properties")),
        showcase = bsicons::bs_icon("building"),
        theme = "primary"
      ),
      bslib::value_box(
        id = ns("val_avg_occupancy"),
        title = "Average Occupancy",
        value = shiny::textOutput(ns("avg_occupancy")),
        showcase = bsicons::bs_icon("people"),
        theme = "success"
      ),
      bslib::value_box(
        id = ns("val_pre_lease_rate"),
        title = "Pre-Lease Rate",
        value = shiny::textOutput(ns("pre_lease_rate")),
        showcase = bsicons::bs_icon("calendar-check"),
        theme = "info"
      ),
      bslib::value_box(
        id = ns("val_market_survey"),
        title = "Market Survey",
        value = shiny::textOutput(ns("market_survey")),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "warning"
      )
    ),

    # Quick access features
    bslib::layout_columns(
      id = ns("quick_access"),
      fill = FALSE,
      bslib::card(
        id = ns("property_management"),
        bslib::card_header("Property Management", class = "bg-light"),
        bslib::card_body(
          htmltools::tags$p("Access property details, property unit details, and other property-related data."),
          shiny::actionButton(ns("nav_properties"), "View Properties", class = "btn-primary")
        )
      ),
      bslib::card(
        id = ns("leasing_admin"),
        bslib::card_header("Leasing Analysis", class = "bg-light"),
        bslib::card_body(
          htmltools::tags$p("Perform in-depth analysis of leasing, occupancy, pre-leasing rates, and more."),
          shiny::actionButton(ns("nav_pre_lease"), "View Leasing Reports", class = "btn-primary")
        )
      ),
      bslib::card(
        id = ns("market_survey"),
        bslib::card_header("Market Survey", class = "bg-light"),
        bslib::card_body(
          htmltools::tags$p("Conduct market surveys, analyze competitor data, and track market trends."),
          shiny::actionButton(ns("nav_survey"), "View Market Survey", class = "btn-primary")
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_home
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_home_server <- function(
    id,
    pool = NULL,
    navigate_func = NULL,
    guide = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_home_server()")

      # database
      check_db_conn(pool)

      # value boxes
      output$active_properties <- shiny::renderText({
        num <- db_read_tbl(pool, "gmh.properties", collect = FALSE) |>
          dplyr::filter(.data$property_status == "Active") |>
          dplyr::collect() |>
          nrow()
        paste0(
          scales::comma(num),
          " Properties"
        )
      })
      output$avg_occupancy <- shiny::renderText({
        num <- db_read_tbl(pool, "gmh.pre_lease_summary", collect = FALSE) |>
          dplyr::summarize(avg_occupancy = mean(current_occupancy, na.rm = TRUE)) |>
          dplyr::pull("avg_occupancy")
        scales::percent(num, accuracy = 0.01)
      })
      output$pre_lease_rate <- shiny::renderText({
        num <- db_read_tbl(pool, "gmh.pre_lease_summary", collect = FALSE) |>
          dplyr::summarize(pre_lease = mean(current_preleased_percent, na.rm = TRUE)) |>
          dplyr::pull("pre_lease")
        scales::percent(num, accuracy = 0.01)
      })
      output$market_survey <- shiny::renderText({
        num <- db_read_tbl(pool, "survey.surveys", collect = FALSE) |>
          dplyr::summarize(num_surveys = dplyr::n()) |>
          dplyr::pull("num_surveys") |>
          as.integer()
        paste0(
          scales::comma(num),
          " Surveys"
        )
      })

      # initialize conductor guide
      guide$
        step(
        "Key Metrics Overview",
        "This section shows your portfolio's vital statistics at a glance.",
        el = ns("#metrics_section")
      )$
        step(
        "Active Properties",
        "View the total number of properties in your portfolio.",
        el = ns("#val_active_properties")
      )$
        step(
        "Average Occupancy",
        "Check the average occupancy rate across your properties.",
        el = ns("#val_avg_occupancy")
      )$
        step(
        "Pre-Lease Rate",
        "Monitor the pre-lease rate for your properties.",
        el = ns("#val_pre_lease_rate")
      )$
        step(
        "Market Survey Submissions",
        "Access the number of market surveys conducted.",
        el = ns("#val_market_survey")
      )$
        step(
        "Quick Access Features",
        "Navigate to key sections of the platform.",
        el = ns("#quick_access")
      )$
        step(
        "Property Management",
        "Access and manage your property details here.",
        el = ns("#property_management")
      )$
        step(
        "Lease Administration",
        "Handle all lease-related activities from this section.",
        el = ns("#leasing_admin")
      )$
        step(
        "Market Survey",
        "Conduct market surveys and analyze competitor data.",
        el = ns("#market_survey")
      )

      # explore button
      shiny::observeEvent(input$explore, {
        guide$init()$start()
      })

      # navigation handlers
      shiny::observeEvent(input$nav_properties, {
        navigate_func("properties")
      })

      shiny::observeEvent(input$nav_pre_lease, {
        navigate_func("pre_lease")
      })

      shiny::observeEvent(input$nav_survey, {
        navigate_func("survey_admin")
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

#' @rdname mod_home
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_home_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Home",
    window_title = "Demo: Home",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Home",
      value = "home",
      icon = bsicons::bs_icon("house"),
      mod_home_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_home_server("demo")
  }

  shiny::shinyApp(ui, server)
}
