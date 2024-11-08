
#  ------------------------------------------------------------------------
#
# Title : Shiny App UI
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------


# internal ----------------------------------------------------------------

add_external_resources <- function() {
  shiny::addResourcePath(
    "www",
    system.file("www", package = "gmhdatahub")
  )

  htmltools::tags$head(
    htmltools::tags$link(
      rel = "shortcut icon",
      type = "image/x-icon",
      href = "www/favicon.ico"
    ),
    golem::bundle_resources(
      path = system.file("www", package = "gmhdatahub"),
      app_title = "gmhdatahub"
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter()
  )
}

# UI ----------------------------------------------------------------------

app_ui <- function(req) {

  force(req)

  htmltools::tagList(
    add_external_resources(),
    bslib::page_navbar(
      id = "nav",
      lang = "en",
      window_title = "GMH DataHub",
      theme = bslib::bs_theme(version = 5),
      title = mod_title_ui("app"),
      footer = mod_footer_ui("app"),
      sidebar = mod_sidebar_ui("app"),
      bslib::nav_panel(
        title = "Home",
        value = "home",
        icon = shiny::icon("home")#,
        # mod_home_ui("app")
      ),
      bslib::nav_panel(
        title = "Data",
        value = "data",
        icon = shiny::icon("database")#,
        # mod_data_ui("app")
      ),
      bslib::nav_panel(
        title = "Analysis",
        value = "analysis",
        icon = shiny::icon("chart-line")#,
        # mod_analysis_ui("app")
      ),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "Links",
        align = "right",
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("github"), "GitHub",
            href = .app_info$repo_url,
            target = "_blank"
          )
        ),
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("book"), "Documentation",
            href = .app_info$docs_url,
            target = "_blank"
          )
        ),
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("envelope"), "Contact",
            href = "#",
            target = "_blank"
          )
        )
      ),
      bslib::nav_menu(
        title = "Logout",
        align = "right",
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("sign-out-alt"), "Logout",
            href = "#",
            target = "_blank"
          )
        )
      )
    )
  )

}


