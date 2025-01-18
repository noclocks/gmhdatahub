#  ------------------------------------------------------------------------
#
# Title : App Assets
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------

app_preloader_ui <- function() {
  htmltools::tagList(
    waiter::waiter_show_on_load(
      html = shiny::tags$div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: space-evenly; height: 50vh;",
        shiny::tags$img(
          src = "www/logo.svg",
          height = "150px",
          style = "background: #FFFFFF; border: 3px solid black; border-radius: 6px; padding: 0.5%;"
        ),
        waiter::spin_puzzle(),
        shiny::tags$h3("Initializing Data Hub...")
      ),
      color = "#0E2B4C"
    )
  )
}

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

add_external_resources <- function() {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = pkg_sys("www")
  )

  htmltools::tags$head(
    # golem::bundle_resources(
    #   path = pkg_sys("www"),
    #   app_title = app_info("name")
    # ),

    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    fontawesome::fa_html_dependency(),
    rintrojs::introjsUI(),
    app_favicon(),
    app_preloader_ui()
  )
}
