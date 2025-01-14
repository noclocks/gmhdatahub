
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
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_forms_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(

    # bslib::layout_columns(
    #   bslib::value_box(
    #     id = ns("properties_value_box"),
    #     title = "Properties",
    #     value = shiny::textOutput(ns("properties_count")),
    #     showcase = bsicons::bs_icon("building")
    #   ),
    #   bslib::value_box(
    #     id = ns("competitors_value_box"),
    #     title = "Competitors",
    #     value = shiny::textOutput("competitor_count"),
    #     showcase = bsicons::bs_icon("graph-up")
    #   ),
    #   bslib::value_box(
    #     id = ns("surveys_value_box"),
    #     title = "Survey Responses",
    #     value = shiny::textOutput("response_count"),
    #     showcase = bsicons::bs_icon("clipboard-data")
    #   )
    # ),

    # progress ----------------------------------------------------------------
    bslib::card(
      bslib::card_header(icon_text("percent", "Progress")),
      bslib::card_body(
        shinyWidgets::progressBar(
          ns("total_progress"),
          value = 0,
          total = 100,
          display_pct = TRUE,
          striped = TRUE,
          title = "Total Progress"
        )
      )
    ),
    bslib::navset_card_tab(
      id = ns("survey_tabs"),
      title = "GMH Communities - Leasing Market Survey Sections",
      sidebar = bslib::sidebar(
        title = icon_text("filter", "Filters"),
        width = 300,
        shiny::selectizeInput(
          ns("property"),
          label = icon_text("building", "Select Property"),
          choices = app_choices_lst$properties,
          selected = app_choices_lst$properties[["1047 Commonwealth Avenue"]]
        ),
        shiny::selectizeInput(
          ns("competitor"),
          label = icon_text("building", "Select Competitor"),
          choices = app_choices_lst$competitors[[1]]
        ),
        shiny::dateRangeInput(
          ns("leasing_week"),
          label = icon_text("calendar", "Leasing Week"),
          start = get_leasing_week_start_date(),
          end = get_leasing_week_end_date(),
          weekstart = 1,
          format = "yyyy-mm-dd"
        )
      ),
      bslib::nav_panel(
        title = "Property Summary",
        value = ns("property_summary"),
        mod_survey_property_summary_ui(ns("property_summary"))
      ),
      bslib::nav_panel(
        title = "Leasing Summary",
        value = ns("leasing_summary"),
        mod_survey_leasing_summary_ui(ns("leasing_summary"))
      ),
      bslib::nav_panel(
        title = "Short Term Leases",
        value = ns("short_term_leases"),
        mod_survey_short_term_leases_ui(ns("short_term_leases"))
      ),
      bslib::nav_panel(
        title = "Fees",
        value = ns("fees"),
        mod_survey_fees_ui(ns("fees"))
      ),
      bslib::nav_panel(
        title = "Amenities",
        value = ns("amenities"),
        mod_survey_amenities_ui(ns("amenities"))
      ),
      bslib::nav_panel(
        title = "Parking",
        value = ns("parking"),
        mod_survey_parking_ui(ns("parking"))
      ),
      bslib::nav_panel(
        title = "Utilities",
        value = ns("utilities"),
        mod_survey_utilities_ui(ns("utilities"))
      ),
      bslib::nav_panel(
        title = "Notes",
        value = ns("notes"),
        mod_survey_notes_ui(ns("notes"))
      ),
      bslib::nav_panel(
        title = "Rents",
        value = ns("rents"),
        mod_survey_rents_ui(ns("rents"))
      )
    )
  )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_forms
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_forms_server <- function(
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
  } else {
    global_filters <- shiny::reactiveValues(
      properties = "739085",
      leasing_week = get_leasing_week()
    )
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_forms_server()")

      # reactive values ---------------------------------------------------------
      db_metrics <- shiny::reactive({ db_read_survey_metrics(pool) })

      # value boxes -------------------------------------------------------------
      # output$properties_count <- shiny::renderText({
      #   shiny::req(db_metrics())
      #   db_metrics()$total_properties
      # })
      #
      # output$competitor_count <- shiny::renderText({
      #   shiny::req(db_metrics())
      #   db_metrics()$total_competitors
      # })
      #
      # output$response_count <- shiny::renderText({
      #   shiny::req(db_metrics())
      #   db_metrics()$total_responses
      # })

      # progress bars ----------------------------------------------------------
      output$total_progress <- shinyWidgets::updateProgressBar(
        session,
        ns("total_progress"),
        value = 80
      )


      # sub-modules -------------------------------------------------------------

      property_summary_data <- mod_survey_property_summary_server(
        ns("property_summary"),
        pool = pool
      )

      leasing_summary_data <- mod_survey_leasing_summary_server(
        ns("leasing_summary"),
        pool = pool,
        selected_property_id = NULL
      )

      short_term_leases_data <- mod_survey_short_term_leases_server(
        ns("short_term_leases"),
        pool = pool,
        global_filters = global_filters
      )

      fees_data <- mod_survey_fees_server(
        ns("fees"),
        pool = pool,
        global_filters = global_filters
      )

      amenities_data <- mod_survey_amenities_server(
        ns("amenities"),
        pool = pool,
        global_filters = global_filters
      )

      parking_data <- mod_survey_parking_server(
        ns("parking"),
        pool = pool,
        global_filters = global_filters
      )

      utilities_data <- mod_survey_utilities_server(
        ns("utilities"),
        pool = pool,
        global_filters = global_filters
      )

      notes_data <- mod_survey_notes_server(
        ns("notes"),
        pool = pool,
        global_filters = global_filters
      )

      rents_data <- mod_survey_rents_server(
        ns("rents"),
        pool = pool,
        global_filters = global_filters
      )

      return(
        list(
          # reactive values
          db_metrics = db_metrics,
          # sub-modules
          property_summary_data = property_summary_data,
          leasing_summary_data = leasing_summary_data,
          short_term_leases_data = short_term_leases_data,
          fees_data = fees_data,
          amenities_data = amenities_data,
          parking_data = parking_data,
          utilities_data = utilities_data,
          notes_data = notes_data,
          rents_data = rents_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_forms
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_forms_demo <- function() {

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
    mod_survey_forms_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------

