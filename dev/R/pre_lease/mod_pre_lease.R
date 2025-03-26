
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
#' A Shiny Module for the GMH Communities Pre-Lease Report Summary & Details.
#'
#' The following functions are implemented for the core, top-level, pre-lease module:
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
#' @importFrom bslib page_fluid navset_card_underline nav_panel nav_spacer
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS
#' @importFrom shinyjs useShinyjs
mod_pre_lease_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(shinyjs::useShinyjs()),
    bslib::page_fluid(

      # value boxes
      mod_pre_lease_value_boxes_ui(ns("value_boxes")),

      # nav
      bslib::navset_card_underline(
        id = ns("nav"),
        # sidebar
        sidebar = mod_pre_lease_sidebar_ui(ns("sidebar")),
        # summary panel
        bslib::nav_panel(
          title = "Summary",
          value = ns("summary"),
          icon = bsicons::bs_icon("clipboard-data"),
          mod_pre_lease_summary_ui(ns("summary"))
        ),
        # details panel
        bslib::nav_panel(
          title = "Details",
          value = ns("details"),
          icon = bsicons::bs_icon("info-circle"),
          mod_pre_lease_details_ui(ns("details"))
        ),
        # charts panel
        bslib::nav_panel(
          title = "Charts",
          value = ns("charts"),
          icon = bsicons::bs_icon("graph-up-arrow"),
          mod_pre_lease_charts_ui(ns("charts"))
        ),
        bslib::nav_spacer(),
        # actions buttons
        mod_pre_lease_actions_ui(ns("actions"))
      )
    )
  )

}


# server ----------------------------------------------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom cli cat_rule
#' @importFrom shiny moduleServer reactiveVal observe
mod_pre_lease_server <- function(id, pool = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_server()")

      if (is.null(pool)) pool <- session$userData$db_pool %||% db_connect()
      check_db_conn(pool)

      # database trigger
      db_trigger <- shiny::reactiveVal(0L)

      # summary data
      mod_data <- mod_pre_lease_data_server(id = "data", pool = pool, db_trigger = db_trigger)

      # value boxes
      mod_value_boxes_data <- mod_pre_lease_value_boxes_server(id = "value_boxes", summary_data = mod_data$pre_lease_summary_data)

      # sidebar
      mod_sidebar_data <- mod_pre_lease_sidebar_server(id = "sidebar", summary_data = mod_data$pre_lease_summary_data, db_trigger = db_trigger)

      # summary
      mod_summary_data <- mod_pre_lease_summary_server(id = "summary", summary_data = mod_data$pre_lease_summary_data)

      # details
      mod_details_data <- mod_pre_lease_details_server(id = "details", summary_data = mod_data$pre_lease_summary_data)

      # charts
      mod_charts_data <- mod_pre_lease_charts_server(id = "charts", summary_data = mod_data$pre_lease_summary_data)

      # actions
      mod_actions_data <- mod_pre_lease_actions_server(id = "actions", db_trigger = db_trigger)

      # toggle observer for value boxes
      shiny::observe({
        mod_value_boxes_data$toggle_value_boxes(mod_sidebar_data$toggle_val_boxes())
      })

      # return
      return(
        list(
          pre_lease_data = mod_data$pre_lease_summary_data,
          filters = mod_sidebar_data,
          selected_property = mod_summary_data$selected_property,
          selected_unit = mod_details_data$selected_unit
        )
      )

    }
  )

}
