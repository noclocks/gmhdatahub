
#  ------------------------------------------------------------------------
#
# Title : App Title Module
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

mod_title_ui <- function(
  id,
  app_info = .app_info
) {

  ns <- shiny::NS(id)

  htmltools::tags$div(
    id = ns("title"),
    class = "navbar-brand",
    htmltools::tags$a(
      href = "#",
      htmltools::tags$img(
        src = .app_info$logo,
        alt = .app_info$name,
        height = 50,
        width = "auto"
      )
    )
  )

}

mod_title_server <- function(id, ...) {
  shiny::server <- function(input, output, session) {
    # No server logic needed
  }
}
