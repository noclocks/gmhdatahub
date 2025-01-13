
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
      class = "mb-4",
      height = "250px",
      background = "linear-gradient(135deg, #1e4d92 0%, #2c3e50 100%)",
      htmltools::tags$div(
        class = "d-flex flex-column justify-content-center h-100 p-5",
        htmltools::tags$div(
          class = "mb-2",
          htmltools::tags$span(
            bsicons::bs_icon("building-check", size = "2rem"),
            style = "opacity: 0.9;"
          )
        ),
        htmltools::tags$h1(
          "Welcome to GMH DataHub",
          class = "display-4 fw-bold mb-3",
          style = "letter-spacing: -0.02em;"
        ),
        htmltools::tags$p(
          class = "lead mb-4",
          style = "font-size: 1.3rem; max-width: 800px; opacity: 0.9;",
          "Your centralized platform for leasing data management and analytics. ",
          "Transform property insights into strategic decisions with our comprehensive dashboard suite."
        ),
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

    # Quick access features
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Property Management"),
        bslib::card_body(
          htmltools::tags$p("Access property details, maintenance records, and tenant information."),
          shiny::actionButton(ns("nav_properties"), "View Properties", class = "btn-primary")
        )
      ),
      bslib::card(
        bslib::card_header("Leasing Analysis"),
        bslib::card_body(
          htmltools::tags$p("Perform in-depth analysis of leasing, occupancy, pre-leasing rates, and more."),
          shiny::actionButton(ns("nav_pre_lease"), "View Leasing Reports", class = "btn-primary")
        )
      ),
      bslib::card(
        bslib::card_header("Market Survey"),
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
  global_filters = NULL,
  navigate_func = NULL
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
      cli::cat_rule("[Module]: mod_home_server()")

      # get db data

      # navigation handlers
      shiny::observeEvent(input$explore, {

      })

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
    theme = app_theme(),
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

# utilities ---------------------------------------------------------------

