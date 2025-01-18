#  ------------------------------------------------------------------------
#
# Title : Pre Lease Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Pre Lease Shiny Module
#'
#' @name mod_pre_lease
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Pre Lease page.
#'
#' Includes the following functions:
#'
#' - `mod_pre_lease_ui()`: User Interface (UI) for the module.
#' - `mod_pre_lease_server()`: Server logic for the module.
#' - `mod_pre_lease_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_pre_lease_ui()`: UI output
#' - `mod_pre_lease_server()`: List of reactive expressions.
#' - `mod_pre_lease_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_pre_lease_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_pre_lease_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card()
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_pre_lease_server <- function(
    id,
    pool = NULL,
    global_filters = NULL) {
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
      cli::cat_rule("[Module]: mod_pre_lease_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_pre_lease_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Pre Lease",
    window_title = "Demo: Pre Lease",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Pre Lease",
      value = "pre_lease",
      icon = bsicons::bs_icon("house"),
      mod_pre_lease_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_pre_lease_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
