
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
          choices = c("None" = "none")
        ) |>
          shinyjs::disabled()
      ),
      bslib::nav_panel(
        title = "Property Summary",
        value = ns("nav_property_summary"),
        mod_survey_property_summary_ui(ns("property_summary"))
      ),
      bslib::nav_panel(
        title = "Leasing Summary",
        value = ns("nav_leasing_summary"),
        mod_survey_leasing_summary_ui(ns("leasing_summary"))
      ),
      bslib::nav_panel(
        title = "Short Term Leases",
        value = ns("nav_short_term_leases"),
        mod_survey_short_term_leases_ui(ns("short_term_leases"))
      ),
      bslib::nav_panel(
        title = "Fees",
        value = ns("nav_fees"),
        mod_survey_fees_ui(ns("fees"))
      ),
      bslib::nav_panel(
        title = "Amenities",
        value = ns("nav_amenities"),
        mod_survey_amenities_ui(ns("amenities"))
      ),
      bslib::nav_panel(
        title = "Parking",
        value = ns("nav_parking"),
        mod_survey_parking_ui(ns("parking"))
      ),
      bslib::nav_panel(
        title = "Utilities",
        value = ns("nav_utilities"),
        mod_survey_utilities_ui(ns("utilities"))
      ),
      bslib::nav_panel(
        title = "Notes",
        value = ns("nav_notes"),
        mod_survey_notes_ui(ns("notes"))
      ),
      bslib::nav_panel(
        title = "Rents",
        value = ns("nav_rents"),
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
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_forms_server()")

      # selected tab ------------------------------------------------------------
      selected_tab <- shiny::reactive({
        shiny::req(input$survey_tabs)
        input$survey_tabs
      })

      shiny::observeEvent(selected_tab(), {

      })

      # selected property / competitor ------------------------------------------
      selected_property_id <- shiny::reactiveVal(NULL)

      # competitors -------------------------------------------------------------
      properties_with_competitors <- db_read_tbl(pool, "mkt.property_competitors") |>
        dplyr::pull("property_id") |>
        unique()

      shiny::observeEvent(input$property, {
        if (input$property %in% properties_with_competitors) {
          shinyjs::enable("competitor")
          shiny::updateSelectizeInput(
            session,
            "competitor",
            choices = c("None" = "none", unlist(app_choices_lst$competitors[[input$property]])),
            selected = "none"
          )
        } else {
          shiny::updateSelectizeInput(
            session,
            "competitor",
            choices = c("None" = "none"),
            selected = "none"
          )
          shinyjs::disable("competitor")
        }
      })

      shiny::observe({
        shiny::req(input$property, input$competitor)
        if (input$competitor != "none") {
          selected_property_id(input$competitor)
        } else {
          selected_property_id(input$property)
        }
      })

      # reactive values ---------------------------------------------------------
      db_metrics <- shiny::reactive({ db_read_survey_metrics(pool) })

      # progress bars ----------------------------------------------------------
      output$total_progress <- shinyWidgets::updateProgressBar(
        session,
        ns("total_progress"),
        value = 80 # TODO
      )

      # sub-modules -------------------------------------------------------------
      property_summary_data <- mod_survey_property_summary_server(
        "property_summary",
        pool = pool,
        selected_property_id = selected_property_id
      )

      leasing_summary_data <- mod_survey_leasing_summary_server(
        "leasing_summary",
        pool = pool,
        selected_property_id = selected_property_id
      )

      short_term_leases_data <- mod_survey_short_term_leases_server(
        "short_term_leases",
        pool = pool,
        global_filters = global_filters
      )

      fees_data <- mod_survey_fees_server(
        "fees",
        pool = pool,
        global_filters = global_filters
      )

      amenities_data <- mod_survey_amenities_server(
        "amenities",
        pool = pool,
        global_filters = global_filters
      )

      parking_data <- mod_survey_parking_server(
        "parking",
        pool = pool,
        global_filters = global_filters
      )

      utilities_data <- mod_survey_utilities_server(
        "utilities",
        pool = pool,
        global_filters = global_filters
      )

      notes_data <- mod_survey_notes_server(
        "notes",
        pool = pool,
        global_filters = global_filters
      )

      rents_data <- mod_survey_rents_server(
        "rents",
        pool = pool,
        global_filters = global_filters
      )

      return(
        list(
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

