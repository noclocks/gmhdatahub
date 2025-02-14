#  ------------------------------------------------------------------------
#
# Title : Shiny App Server
#    By : Jimmy Briggs
#  Date : 2025-01-17
#
#  ------------------------------------------------------------------------

#' Shiny App Server
#'
#' @family **Shiny App**
#'
#' @description
#' This is the main server function for the Shiny application.
#'
#' @param input,output,session The input, output, and session functions.
#'
#' @returns
#' The server for the Shiny application.
#'
#' @export
#'
#' @importFrom noClocksAuthR sign_out_from_shiny
#' @importFrom waiter waiter_hide
#' @importFrom shiny reactive
app_server <- function(input, output, session) {
  # sever::sever()
  # sever::rupture(ms = 10000)

  output$signed_in_as <- shiny::renderText({
    session$userData$user()$email
  })

  shiny::observeEvent(input$auth_logout, {
    noClocksAuthR::sign_out_from_shiny(session)
    session$reload()
  })

  # initialize database connection pool
  pool <- db_connect()
  session$userData$pool <- pool

  # waiter
  waiter::waiter_hide()

  # global filters
  global_filters <- shiny::reactive({
    list(
      portfolios = NULL,
      properties = NULL,
      leasing_week = NULL
    )
  })

  # define navigate function using this parent session
  navigate <- function(target) {
    bslib::nav_select(id = "nav", selected = target, session = session)
  }

  guide <- app_guide()

  # main modules
  mod_home_data <- mod_home_server("home", pool = pool, navigate_func = navigate, guide = guide)
  mod_dashboard_data <- mod_dashboard_server("dashboard", pool = pool)

  # data modules
  mod_properties_data <- mod_properties_server("properties", pool = pool, global_filters = global_filters)
  mod_property_units_data <- mod_property_units_server("property_units", pool = pool, global_filters = global_filters)
  mod_leases_data <- mod_leases_server("leases", pool = pool, global_filters = global_filters)
  mod_floorplans_data <- mod_floorplans_server("floorplans", pool = pool, global_filters = global_filters)
  mod_residents_data <- mod_residents_server("residents", pool = pool, global_filters = global_filters)

  # report modules
  mod_pre_lease_data <- mod_pre_lease_server("pre_lease", pool = pool)
  mod_box_score_data <- mod_box_score_server("box_score", pool = pool, global_filters = global_filters)
  mod_performance_data <- mod_performance_server("performance", pool = pool, global_filters = global_filters)

  # survey modules
  mod_survey_admin_data <- mod_survey_admin_server("survey_admin", pool = pool, global_filters = global_filters, navigate = navigate)
  mod_survey_forms_data <- mod_survey_forms_server("survey_forms", pool = pool)
  mod_survey_insights_data <- mod_survey_insights_server("survey_insights", pool = pool)
}
