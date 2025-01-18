#  ------------------------------------------------------------------------
#
# Title : Shiny App Sidebar Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------



# topic -------------------------------------------------------------------

#' App Sidebar Module
#'
#' @name app_sidebar
#'
#' @description
#' This module creates the global application sidebar containing filters, options,
#' user profile, and high-level application metadata information.
#'
#' It includes the following functions:
#'
#' - `app_sidebar_ui()`: UI function for the sidebar module.
#' - `app_sidebar_server()`: Server function for the sidebar module.
#'
#' and the following sub-modules:
#'
#' - `mod_sidebar_user_profile_ui()`: UI function for the user profile section.
#' - `mod_sidebar_user_profile_server()`: Server function for the user profile section.
#'
#' - `mod_sidebar_filters_ui()`: UI function for the filters section.
#' - `mod_sidebar_filters_server()`: Server function for the filters section.
#'
#' @param id The module's namespace ID.
#'
#' @seealso
#' [app_ui()]
#'
#' @returns
#' - `app_sidebar_ui()`: UI function for the sidebar module.
#' - `app_sidebar_server()`: Server function for the sidebar module.
NULL


# UI ----------------------------------------------------------------------

#' @rdname app_sidebar
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib sidebar accordion accordion_panel input_task_button
#' @importFrom htmltools tags
#' @importFrom shiny NS
app_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::sidebar(
    id = ns("sidebar"),
    title = "GMH Data Hub",
    open = FALSE,

    # mod_sidebar_user_profile_ui(ns("user")),
    mod_sidebar_filters_ui(ns("filters")),
    bslib::input_task_button(
      id = ns("refresh"),
      label = "Refresh Data",
      icon = bsicons::bs_icon("arrow-clockwise"),
      type = "success",
      auto_reset = TRUE
    )
  )
}

app_sidebar_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: app_sidear_server()")

      # user_profile <- mod_sidebar_user_profile_server("user")
      filters <- mod_sidebar_filters_server("filters")

      return(list(global_filters = filters))
    }
  )
}



mod_sidebar_filters_ui <- function(id) {
  ns <- shiny::NS(id)

  # get the default choices/values
  default_leasing_week_period <- get_leasing_week()
  default_leasing_week_minmax <- c(
    get_pre_lease_season_start_date() - lubridate::years(1),
    Sys.Date()
  )
  default_portfolios <- get_default_app_choices("portfolios")
  default_properties <- get_default_app_choices("properties")

  bslib::accordion(
    bslib::accordion_panel(
      "Filters",
      icon = bsicons::bs_icon("funnel"),
      shiny::selectizeInput(
        ns("portfolios"),
        label = "Investment Portfolio:",
        choices = default_portfolios,
        selected = default_portfolios,
        multiple = TRUE,
        options = list(
          plugins = "remove_button",
          closeAfterSelect = TRUE
        )
      ),
      shiny::selectInput(
        ns("properties"),
        label = "Properties:",
        choices = default_properties,
        selected = default_properties[1],
        multiple = TRUE
      ),
      shiny::dateRangeInput(
        ns("leasing_week"),
        label = "Leasing Week:",
        start = default_leasing_week_period$start,
        end = default_leasing_week_period$end,
        min = default_leasing_week_minmax[1],
        max = default_leasing_week_minmax[2],
        format = "yyyy-mm-dd",
        startview = "month",
        weekstart = 1,
        separator = " - "
      )
    )
  )
}

mod_sidebar_filters_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      filts <- shiny::reactive({
        shiny::req(
          input$portfolios,
          input$properties,
          input$leasing_week
        )

        leasing_week_start <- lubridate::as_date(input$leasing_week[1])
        leasing_week_end <- lubridate::as_date(input$leasing_week[2])

        list(
          portfolios = input$portfolios,
          properties = input$properties,
          leasing_week = input$leasing_week
        )
      })
    }
  )
}

mod_sidebar_user_profile_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header("User Profile"),
    bslib::card_body(
      style = "text-align: center; padding: 10px;",
      bsicons::bs_icon("person-circle", size = "2rem"),
      htmltools::tags$h5(shiny::textOutput(ns("user_name"))),
      htmltools::tags$p(shiny::textOutput(ns("user_role"))),
      htmltools::tags$p(shiny::textOutput(ns("user_email")))
    )
  )
}

mod_sidebar_user_profile_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$user_name <- shiny::renderText({
        "Jimmy Briggs"
      })
      output$user_role <- shiny::renderText({
        "Software Engineer"
      })
      output$user_email <- shiny::renderText({
        "jimmy.briggs@noclocks.dev"
      })
    }
  )
}
