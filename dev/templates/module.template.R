
#  ------------------------------------------------------------------------
#
# Title : {{title}} Shiny Module
#    By : {{author}}
#  Date : {{date}}
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' {{title}} Shiny Module
#'
#' @name mod_{{name}}
#'
#' @description
#' A Shiny Module for the GMH Data Hub's {{title}} page.
#'
#' Includes the following functions:
#'
#' - `mod_{{name}}_ui()`: User Interface (UI) for the module.
#' - `mod_{{name}}_server()`: Server logic for the module.
#' - `mod_{{name}}_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_{{name}}_ui()`: UI output
#' - `mod_{{name}}_server()`: List of reactive expressions.
#' - `mod_{{name}}_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_{{name}}_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_{{name}}_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(

    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_{{name}}_server <- function(
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
      cli::cat_rule("[Module]: mod_{{name}}_server()")

      return(
        list(
          # reactive values
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_{{name}}
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_{{name}}_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: {{title}}",
    window_title = "Demo: {{title}}",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "{{title}}",
      value = "{{name}}",
      icon = bsicons::bs_icon("house"),
      mod_{{name}}_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_{{name}}_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------


