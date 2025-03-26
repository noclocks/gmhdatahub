#  ------------------------------------------------------------------------
#
# Title : App Assets
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------

# add external resources --------------------------------------------------

.add_resource_path <- function() {
  shiny::addResourcePath("www", pkg_sys("www"))
}

rlang::on_load({ .add_resource_path() })

#' Add External Resources
#'
#' @description
#' Add external resources to the shiny app.
#'
#' @returns
#' Adds external resources to the shiny app.
#'
#' @export
#'
#' @importFrom conductor use_conductor
#' @importFrom fontawesome fa_html_dependency
#' @importFrom htmltools tags
#' @importFrom rintrojs introjsUI
#' @importFrom sever useSever
#' @importFrom shiny addResourcePath
#' @importFrom shinybrowser detect
#' @importFrom shinybusy busy_start_up spin_epic
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter
add_external_resources <- function() {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = pkg_sys("www")
  )

  htmltools::tags$head(
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    sever::useSever(),
    shinybusy::busy_start_up(
      loader = shinybusy::spin_epic("orbit", color = "#FFF"),
      text = "Initializing Data Hub...",
      timeout = 1500,
      color = "#FFF",
      background = gmh_colors("primary")
    ),
    shiny.emptystate::use_empty_state(),
    shinybrowser::detect(),
    fontawesome::fa_html_dependency(),
    rintrojs::introjsUI(),
    conductor::use_conductor(),
    htmltools::tags$link(href = "www/styles/css/custom-styles.css", rel = "stylesheet"),
    htmltools::tags$link(href = "www/styles/css/handsontable.css", rel = "stylesheet"),
    htmltools::tags$link(href = "www/styles/css/reactable.css", rel = "stylesheet"),
    app_favicon()
  )
}


# app_preloader_ui <- function() {
#   htmltools::tagList(
#     waiter::waiter_show_on_load(
#       html = shiny::tags$div(
#         style = "display: flex; flex-direction: column; align-items: center; justify-content: space-evenly; height: 50vh;",
#         shiny::tags$img(
#           src = "www/logo.svg",
#           height = "150px",
#           style = "background: #FFFFFF; border: 3px solid black; border-radius: 6px; padding: 0.5%;"
#         ),
#         waiter::spin_puzzle(),
#         shiny::tags$h3("Initializing Data Hub...")
#       ),
#       color = "#0E2B4C"
#     )
#   )
# }

app_logo <- function() {
  htmltools::tags$img(
    src = "www/img/gmh-logo.svg",
    alt = "GMH Communities Logo",
    height = 50,
    style = "margin-top: -10px;"
  )
}

app_favicon <- function(path = "www/favicon.ico") {
  favicon_mime_type <- mime::guess_type(
    path,
    mime_extra = c("ico" = "image/x-icon")
  )

  htmltools::tags$head(
    htmltools::tags$link(
      rel = "icon",
      type = favicon_mime_type,
      href = path
    )
  )
}
