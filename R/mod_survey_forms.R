#  ------------------------------------------------------------------------
#
# Title : Survey Forms Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Forms Shiny Module
#'
#' @name mod_survey_forms
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Forms page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_forms_ui()`: User Interface (UI) for the module.
#' - `mod_survey_forms_server()`: Server logic for the module.
#' - `mod_survey_forms_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_forms_ui()`: UI output
#' - `mod_survey_forms_server()`: List of reactive expressions.
#' - `mod_survey_forms_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_forms_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_forms
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid navset_card_underline sidebar nav_panel nav_spacer nav_item
#' @importFrom htmltools tagList
#' @importFrom shiny NS selectizeInput dateInput actionButton icon
mod_survey_forms_ui <- function(id) {
  ns <- shiny::NS(id)

  current_week_start <- get_leasing_week_start_date()

  htmltools::tagList(
    bslib::page_fluid(
      bslib::navset_card_underline(
        id = ns("survey_tabs"),
        title = "GMH Communities - Market Survey Sections",
        sidebar = bslib::sidebar(
          title = icon_text("filter", "Filters"),
          width = 300,
          shiny::selectizeInput(
            ns("property"),
            label = icon_text("building", "Property"),
            choices = get_default_app_choices("properties"),
            selected = get_default_app_choices("properties")[["1047 Commonwealth Avenue"]]
          ),
          shiny::selectizeInput(
            ns("competitor"),
            label = icon_text("building", "Competitor"),
            choices = NULL,
            selected = NULL,
            options = list(placeholder = "Select a Competitor")
          ),
          shiny::dateInput(
            ns("leasing_week"),
            label = icon_text("calendar", "Leasing Week"),
            value = current_week_start,
            weekstart = 1,
            daysofweekdisabled = c(0, 2:6)
          )
        ),
        bslib::nav_panel(
          title = "Property Summary",
          value = "nav_property_summary",
          icon = bsicons::bs_icon("house"),
          mod_survey_property_summary_ui(ns("property_summary"))
        ),
        bslib::nav_panel(
          title = "Leasing Summary",
          value = "nav_leasing_summary",
          icon = bsicons::bs_icon("clipboard"),
          mod_survey_leasing_summary_ui(ns("leasing_summary"))
        ),
        bslib::nav_panel(
          title = "Short Term Leases",
          value = "nav_short_term_leases",
          icon = bsicons::bs_icon("calendar"),
          mod_survey_short_term_leases_ui(ns("short_term_leases"))
        ),
        bslib::nav_panel(
          title = "Fees",
          value = "nav_fees",
          icon = bsicons::bs_icon("credit-card"),
          mod_survey_fees_ui(ns("fees"))
        ),
        bslib::nav_panel(
          title = "Property Amenities",
          value = "nav_property_amenities",
          icon = bsicons::bs_icon("house"),
          mod_survey_property_amenities_ui(ns("property_amenities"))
        ),
        bslib::nav_panel(
          title = "Unit Amenities",
          value = "nav_unit_amenities",
          icon = bsicons::bs_icon("door-open"),
          mod_survey_unit_amenities_ui(ns("unit_amenities"))
        ),
        bslib::nav_panel(
          title = "Parking",
          value = "nav_parking",
          icon = bsicons::bs_icon("car-front"),
          mod_survey_parking_ui(ns("parking"))
        ),
        bslib::nav_panel(
          title = "Utilities",
          value = "nav_utilities",
          icon = bsicons::bs_icon("plug"),
          mod_survey_utilities_ui(ns("utilities"))
        ),
        bslib::nav_panel(
          title = "Hours",
          value = "nav_hours",
          icon = bsicons::bs_icon("clock"),
          mod_survey_hours_ui(ns("hours"))
        ),
        bslib::nav_panel(
          title = "Rents",
          value = "nav_rents",
          icon = bsicons::bs_icon("currency-dollar"),
          mod_survey_rents_ui(ns("rents"))
        ),
        bslib::nav_panel(
          title = "Notes",
          value = "nav_notes",
          icon = bsicons::bs_icon("sticky"),
          mod_survey_notes_ui(ns("notes"))
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          shiny::actionButton(
            ns("edit_survey_section"),
            "Edit Section Data",
            icon = shiny::icon("edit"),
            class = "btn-primary"
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_forms
#' @export
#' @importFrom cli cat_rule cli_alert_info cli_alert_success
#' @importFrom shiny moduleServer reactiveValues reactiveVal observeEvent updateSelectizeInput req observe reactive
#' @importFrom shiny bindEvent withProgress incProgress setProgress showNotification
mod_survey_forms_server <- function(
    id,
    pool = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # setup -------------------------------------------------------------------
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_forms_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # filters -----------------------------------------------------------------
      selected_filters <- shiny::reactiveValues(
        property_id = NULL,
        property_name = NULL,
        competitor_id = NULL,
        competitor_name = NULL,
        leasing_week_id = NULL,
        leasing_week_date = NULL,
        survey_id = NULL,
        user_id = NULL,
        user_email = NULL
      )

      # filter observers --------------------------------------------------------
      session$userData$selected_survey_property <- shiny::reactiveVal(NULL)

      # property ID & name
      shiny::observeEvent(input$property, {
        prop_id <- as.integer(input$property)
        prop_name <- get_property_name_by_id(input$property)
        selected_filters$property_id <- prop_id
        selected_filters$property_name <- prop_name
        session$userData$selected_survey_property(prop_id)
        cli::cli_alert_info(
          c(
            "Selected Property: {.field {prop_name}} (ID: {.field {prop_id}})"
          )
        )
        db_trigger()
      })

      shiny::observeEvent(input$property, {
        competitors <- db_read_survey_competitors(
          pool,
          property_id = input$property
        )

        if (nrow(competitors) == 0) {
          shiny::updateSelectizeInput(
            session,
            "competitor",
            choices = character(0),
            selected = character(0),
            options = list(placeholder = "No Competitors Available")
          )
        } else {
          comp_choices <- competitors$competitor_id |>
            as.list() |>
            setNames(competitors$competitor_name)
          shiny::updateSelectizeInput(
            session,
            "competitor",
            choices = comp_choices,
            selected = character(0)
          )
        }
      })

      # competitor ID & name
      shiny::observeEvent(input$competitor, {
        shiny::req(input$competitor)
        comp_id <- input$competitor
        comp_name <- get_competitor_name_by_id(input$competitor)
        selected_filters$competitor_id <- comp_id
        selected_filters$competitor_name <- comp_name
        msg <- if (is.null(comp_id)) {
          "Selected Competitor: {.field None}"
        } else {
          c(
            "Selected Competitor: {.field {comp_name}} (ID: {.field {comp_id}})"
          )
        }
        cli::cli_alert_info(msg)
        db_trigger()
      })

      # leasing week ID & date
      shiny::observeEvent(input$leasing_week, {
        shiny::req(input$leasing_week)
        leasing_week_date <- get_leasing_week_start_date(input$leasing_week)
        leasing_week_id <- get_leasing_week_id_by_date(pool, leasing_week_date)
        selected_filters$leasing_week_id <- leasing_week_id
        selected_filters$leasing_week_date <- leasing_week_date
        cli::cli_alert_info(
          c(
            "Selected Leasing Week: {.field {format(leasing_week_date, '%Y-%m-%d')}}",
            " (ID: {.field {leasing_week_id}})"
          )
        )
      })

      # survey ID -----------------------------------------------------------
      shiny::observe({
        if (is.null(selected_filters$survey_id)) {
          default_survey_id <- db_read_survey_id(
            pool,
            property_id = selected_filters$property_id,
            competitor_id = selected_filters$competitor_id,
            leasing_week_id = selected_filters$leasing_week_id
          )
          selected_filters$survey_id <- default_survey_id
          cli::cli_alert_info(
            c(
              "Cached Survey ID: Survey {.field {default_survey_id}}"
            )
          )
        }
      })

      # user --------------------------------------------------------------------
      shiny::observe({
        if (is.null(session$userData$user)) {
          user_email <- "default_user@example.com"
          user_id <- get_user_id_by_email(pool, user_email)
        } else {
          user_id <- session$userData$user()$user_uid
          user_email <- session$userData$user()$email
        }
        selected_filters$user_id <- user_id
        selected_filters$user_email <- user_email
        cli::cli_alert_info("Current User: {.field {user_email}} (ID: {.field {user_id}})")
      })

      # navigation --------------------------------------------------------------
      session$userData$selected_survey_tab <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$survey_tabs, {
        cli::cli_alert_info("Selected Survey Tab: {.field {input$survey_tabs}}")
        session$userData$selected_survey_tab(input$survey_tabs)
      })

      # survey data -------------------------------------------------------------
      survey_data <- shiny::reactiveValues(
        property_summary = NULL,
        leasing_summary = NULL,
        short_term_leases = NULL,
        fees = NULL,
        fee_structures = NULL,
        property_amenities = NULL,
        unit_amenities = NULL,
        unit_amenities_rates_premiums = NULL,
        parking = NULL,
        utilities = NULL,
        hours = NULL,
        notes = NULL,
        rents_by_floorplan = NULL
      )

      # map data -------------------------------------------------------
      map_data <- shiny::reactive({
        shiny::req(selected_filters$property_id)
        db_read_gmh_map_data(pool, property_id = selected_filters$property_id)
      })

      # database data -----------------------------------------------------------

      # setup db_refresh_trigger reactive value for session
      session$userData$db_refresh_trigger <- shiny::reactiveVal(0)

      # create db_trigger function to increment db_refresh_trigger and to pass to section modules
      db_trigger <- function() {
        session$userData$db_refresh_trigger(
          session$userData$db_refresh_trigger() + 1
        )
      }

      # observe db_refresh_trigger to trigger db_read_survey_data for refreshing database data across sections
      shiny::observe({
        session$userData$db_refresh_trigger()
        prop_id <- selected_filters$property_id
        comp_id <- selected_filters$competitor_id
        week_id <- selected_filters$leasing_week_id

        cli::cli_alert_info("Retrieving Survey Data from Database...")
        shiny::withProgress(
          message = "Retrieving Survey Data...",
          detail = "This may take a few moments.",
          value = 0,
          {
            shiny::incProgress(1 / 13, detail = "Retrieving Property Summary Data...")
            survey_data$property_summary <- db_read_survey_property_summary(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Leasing Summary Data...")
            survey_data$leasing_summary <- db_read_survey_leasing_summary(
              pool,
              property_id = prop_id,
              competitor_id = comp_id,
              leasing_week_id = week_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Short Term Leases Data...")
            survey_data$short_term_leases <- db_read_survey_short_term_leases(
              pool,
              property_id = prop_id,
              competitor_id = comp_id,
              leasing_week_id = week_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Fees Data...")
            survey_data$fees <- db_read_survey_fees(
              pool,
              property_id = prop_id,
              competitor_id = comp_id,
              leasing_week_id = week_id
            )
            survey_data$fee_structures <- db_read_survey_fee_structures(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Property Amenities Data...")
            survey_data$property_amenities <- db_read_survey_property_amenities(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Unit Amenities Data...")
            survey_data$unit_amenities <- db_read_survey_unit_amenities(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Unit Amenities Rates & Premiums Data...")
            survey_data$unit_amenities_rates_premiums <- db_read_survey_unit_amenities_rates_premiums(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Parking Data...")
            survey_data$parking <- db_read_survey_parking(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Utilities Data...")
            survey_data$utilities <- db_read_survey_utilities(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Hours Data...")
            survey_data$hours <- db_read_survey_hours(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Notes Data...")
            survey_data$notes <- db_read_survey_notes(
              pool,
              property_id = prop_id,
              competitor_id = comp_id
            )
            shiny::incProgress(1 / 13, detail = "Retrieving Rents By Floorplan Data...")
            survey_data$rents <- db_read_survey_rents_by_floorplan(
              pool,
              property_id = prop_id,
              competitor_id = comp_id,
              leasing_week_id = week_id
            )
            shiny::setProgress(1)
          }
        )
        cli::cli_alert_success("Survey Data Retrieved Successfully")
        shiny::showNotification("Survey Data Retrieved Successfully!")
      }) |>
        shiny::bindEvent(
          input$property,
          input$competitor,
          input$leasing_week,
          session$userData$db_refresh_trigger()
        )

      # survey modules ----------------------------------------------------------

      # property summary
      mod_survey_property_summary_data <- mod_survey_property_summary_server(
        id = "property_summary",
        pool = pool,
        survey_data = survey_data,
        map_data = map_data,
        selected_filters = selected_filters,
        db_trigger_func = db_trigger,
        edit_survey_section = shiny::reactive({
          input$edit_survey_section
        })
      )

      # leasing summary
      mod_survey_leasing_summary_data <- mod_survey_leasing_summary_server(
        id = "leasing_summary",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        db_trigger_func = db_trigger,
        edit_survey_section = shiny::reactive({
          input$edit_survey_section
        })
      )

      # fees
      mod_fees_data <- mod_survey_fees_server(
        id = "fees",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        db_trigger_func = db_trigger,
        edit_survey_section = shiny::reactive({
          input$edit_survey_section
        })
      )

      # property amenities
      mod_survey_property_amenities_data <- mod_survey_property_amenities_server(
        id = "property_amenities",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        db_trigger_func = db_trigger,
        edit_survey_section = shiny::reactive({
          input$edit_survey_section
        })
      )

      # unit amenities
      mod_survey_unit_amenities_data <- mod_survey_unit_amenities_server(
        id = "unit_amenities",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        db_trigger_func = db_trigger,
        edit_survey_section = shiny::reactive({
          input$edit_survey_section
        })
      )

      # short term leases
      mod_short_term_leases_data <- mod_survey_short_term_leases_server(
        id = "short_term_leases",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        edit_survey_section = shiny::reactive({ input$edit_survey_section })
      )

      # parking
      mod_parking_data <- mod_survey_parking_server(
        id = "parking",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        edit_survey_section = shiny::reactive({ input$edit_survey_section })
      )

      # utilities
      mod_survey_utilities_data <- mod_survey_utilities_server(
        "utilities",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        db_trigger_func = db_trigger,
        edit_survey_section = shiny::reactive({ input$edit_survey_section })
      )

      # notes
      mod_notes_data <- mod_survey_notes_server(
        "notes",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        edit_survey_section = shiny::reactive({ input$edit_survey_section })
      )

      # hours
      mod_hours_data <- mod_survey_hours_server(
        "hours",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        edit_survey_section = shiny::reactive({ input$edit_survey_section })
      )

      # rents
      mod_rents_data <- mod_survey_rents_server(
        "rents",
        pool = pool,
        survey_data = survey_data,
        selected_filters = selected_filters,
        edit_survey_section = shiny::reactive({ input$edit_survey_section })
      )

      # module data -------------------------------------------------------------
      survey_mods_data <- shiny::reactive({
        list(
          property_summary = mod_survey_property_summary_data(),
          leasing_summary = mod_survey_leasing_summary_data(),
          short_term_leases = mod_short_term_leases_data(),
          fees = mod_fees_data(),
          property_amenities = mod_survey_property_amenities_data(),
          unit_amenities = mod_survey_unit_amenities_data(),
          parking = mod_parking_data(),
          utilities = mod_survey_utilities_data(),
          notes = mod_notes_data(),
          rents = mod_rents_data()
        )
      })

      # return ------------------------------------------------------------------
      return(
        list(
          selected_filters = selected_filters,
          survey_data = survey_data,
          map_data = map_data,
          survey_mods_data = survey_mods_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_forms
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar nav_spacer nav_panel
#' @importFrom pkgload load_all
#' @importFrom shiny shinyApp
mod_survey_forms_demo <- function(pool = NULL) {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Forms",
    window_title = "Demo: Survey Forms",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Forms",
      value = "survey_forms",
      icon = bsicons::bs_icon("house"),
      mod_survey_forms_ui("demo")
    )
  )

  server <- function(input, output, session) {
    if (is.null(pool)) pool <- db_connect()
    default_property <- get_default_app_choices("properties")[["1047 Commonwealth Avenue"]]
    default_user <- get_user_id_by_email(pool, "default_user@example.com")
    mod_survey_forms_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)
}
