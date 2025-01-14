
app_server <- function(input, output, session) {

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

  # modules
  mod_home_data <- mod_home_server("home", pool = pool, global_filters = global_filters, navigate_func = navigate)
  mod_dashboard_data <- mod_dashboard_server("dashboard", pool = pool, global_filters = global_filters)
  mod_properties_data <- mod_properties_server("properties", pool = pool, global_filters = global_filters)
  mod_property_units_data <- mod_property_units_server("property_units", pool = pool, global_filters = global_filters)
  mod_leases_data <- mod_leases_server("leases", pool = pool, global_filters = global_filters)
  mod_floorplans_data <- mod_floorplans_server("floorplans", pool = pool, global_filters = global_filters)
  mod_residents_data <- mod_residents_server("residents", pool = pool, global_filters = global_filters)

  mod_survey_admin_data <- mod_survey_admin_server("survey_admin", pool = pool, global_filters = global_filters)
  mod_survey_forms_data <- mod_survey_forms_server("survey_forms", pool = pool, global_filters = global_filters)
  mod_survey_insights_data <- mod_survey_insights_server("survey_insights", pool = pool, global_filters = global_filters)



}
