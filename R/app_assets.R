
#  ------------------------------------------------------------------------
#
# Title : App Assets
#    By : Jimmy Briggs
#  Date : 2025-01-10
#
#  ------------------------------------------------------------------------

app_favicon <- function(path = "www/favicon.ico") {

  htmltools::tags$head(
    htmltools::tags$link(
      rel = "shortcut icon",
      type = "image/x-icon",
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
    app_favicon(),
    # app_styles(),

    golem::bundle_resources(
      path = pkg_sys("www"),
      app_title = app_info("name")
    ),

    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    fontawesome::fa_html_dependency(),
    rintrojs::introjsUI()
  )

}


