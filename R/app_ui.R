#  ------------------------------------------------------------------------
#
# Title : Shiny App UI
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

#' Shiny App UI
#'
#' @family **Shiny App**
#'
#' @description
#' This function generates the core user interface for the GMH DataHub Shiny app.
#'
#' @param req (Internal) The initial HTTP request object.
#'
#' @returns
#' The UI for the Shiny application.
#'
#' @export
#'
#' @seealso
#' - [app_server()]: Server logic for the Shiny app that complements this UI.
#' - [app_theme_ui()]: Customized [bslib::bs_theme()] for the Shiny app,
#'   built leveraging the GMH Communities brand assets.
#' - [app_assets()]: Functions for managing and including various static assets in a Shiny application.
#' - [run_app()]: Run the GMH DataHub Shiny app.
#' - [bslib::page_navbar()]: Create a Bootstrap navbar.
#'
#' @importFrom bslib page_navbar nav_spacer nav_panel nav_menu nav_item input_dark_mode
#' @importFrom shiny actionLink
#' @importFrom htmltools tagList tags
#' @importFrom bsicons bs_icon
#'
#' @examplesIf interactive()
#' shiny::shinyApp(ui = app_ui, server = app_server)
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar nav_spacer nav_panel nav_menu nav_item input_dark_mode navbar_options
#' @importFrom htmltools tagList tags
#' @importFrom shiny icon textOutput actionLink
app_ui <- function(req = NULL) {

  force(req)

  if (!is.null(req)) {
    http_method <- req$REQUEST_METHOD
    path_info <- req$PATH_INFO
    if (http_method == "GET" && path_info == "/health") {
      return(app_healthcheck(req))
    }
  }

  htmltools::tagList(
    add_external_resources(),
    bslib::page_navbar(
      id = "nav",
      lang = "en",
      window_title = "GMH DataHub",
      navbar_options = bslib::navbar_options(
        class = "bg-light",
        position = "static-top",
        theme = "light",
        collapsible = TRUE,
        underline = TRUE
      ),
      selected = "survey_forms",
      theme = app_theme_ui(),
      title = app_title_ui(),
      footer = app_footer_ui(),
      # header = app_header_ui(),
      # sidebar = app_sidebar_ui("sidebar"),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "Home",
        value = "home",
        icon = bsicons::bs_icon("house"),
        mod_home_ui("home")
      ),
      bslib::nav_panel(
        title = "Dashboard",
        value = "dashboard",
        icon = bsicons::bs_icon("speedometer2"),
        mod_dashboard_ui("dashboard")
      ),
      bslib::nav_menu(
        title = "Data",
        value = "data",
        icon = bsicons::bs_icon("database"),
        bslib::nav_panel(
          title = "Properties",
          value = "properties",
          icon = bsicons::bs_icon("buildings"),
          mod_properties_ui("properties")
        ),
        bslib::nav_panel(
          title = "Property Units",
          value = "property_units",
          icon = bsicons::bs_icon("door-open"),
          mod_property_units_ui("property_units")
        ),
        bslib::nav_panel(
          title = "Leases",
          value = "leases",
          icon = bsicons::bs_icon("file-earmark-text"),
          mod_leases_ui("leases")
        ),
        bslib::nav_panel(
          title = "Floorplans",
          value = "floorplans",
          icon = bsicons::bs_icon("file-earmark-image"),
          mod_floorplans_ui("floorplans")
        ),
        bslib::nav_panel(
          title = "Residents",
          value = "residents",
          icon = bsicons::bs_icon("people"),
          mod_residents_ui("residents")
        )
      ),
      bslib::nav_menu(
        title = "Reports",
        value = "reports",
        icon = bsicons::bs_icon("file-earmark-text"),
        bslib::nav_panel(
          title = "Pre Lease",
          value = "pre_lease",
          icon = bsicons::bs_icon("file-check"),
          mod_pre_lease_ui("pre_lease")
        ),
        bslib::nav_panel(
          title = "Box Score",
          value = "box_score",
          icon = bsicons::bs_icon("file-earmark-bar-graph"),
          mod_box_score_ui("box_score")
        ),
        bslib::nav_panel(
          title = "Performance",
          value = "performance",
          icon = bsicons::bs_icon("graph-up-arrow"),
          mod_performance_ui("performance")
        )
      ),
      bslib::nav_menu(
        title = "Market Survey",
        value = "market_survey",
        icon = bsicons::bs_icon("clipboard-data"),
        bslib::nav_panel(
          title = "Survey Admin",
          value = "survey_admin",
          icon = bsicons::bs_icon("person-gear"),
          mod_survey_admin_ui("survey_admin")
        ),
        bslib::nav_panel(
          title = "Survey Forms",
          value = "survey_forms",
          icon = bsicons::bs_icon("ui-checks"),
          mod_survey_forms_ui("survey_forms")
        ),
        bslib::nav_panel(
          title = "Survey Insights",
          value = "survey_insights",
          icon = bsicons::bs_icon("lightbulb"),
          mod_survey_insights_ui("survey_insights")
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(bslib::input_dark_mode(id = "dark_mode", mode = "light")),
      bslib::nav_menu(
        title = "Links",
        align = "right",
        icon = bsicons::bs_icon("link-45deg"),
        bslib::nav_item(
          tags$a(
            icon("book"),
            "Documentation",
            href = "#", # pkg_sys("docs/index.html"),
            target = "_blank"
          )
        ),
        bslib::nav_item(
          tags$a(
            icon("github"), "GitHub",
            href = "https://docs.noclocks.dev/gmhdatahub/",
            target = "_blank"
          )
        )
      ),
      bslib::nav_menu(
        title = "Contact",
        align = "right",
        icon = bsicons::bs_icon("envelope"),
        bslib::nav_item(
          tags$a(
            icon("envelope"),
            "Email Support",
            href = "mailto:support@noclocks.dev",
            target = "_blank"
          )
        )
      ),
      bslib::nav_menu(
        title = "User",
        align = "right",
        icon = bsicons::bs_icon("person-circle"),
        bslib::nav_item(
          htmltools::tags$a(
            shiny::icon("user"),
            shiny::textOutput("signed_in_as"),
            href = "#",
            style = "display: inline-flex; align-items: center; padding: 2.5px 10px; width: 16rem; justify-content: space-between;"
          )
        ),
        bslib::nav_item(
          shiny::actionLink(
            inputId = "auth_logout",
            label = "Logout",
            icon = shiny::icon("sign-out-alt"),
            style = "display: inline-flex; align-items: center; padding: 2.5px 50px; width: -webkit-fill-available;"
          )
        )
      )
    )
  )
}

app_header_ui <- function(title = "GMH Data Hub Platform") {
  htmltools::tags$div(
    class = "px-4 py-3 app-header",
    htmltools::tags$div(
      class = "d-flex align-items-center",
      bsicons::bs_icon("building-fill", size = "1.5rem", class = "me-2"),
      htmltools::h3(title, class = "m-0")
    ),
    htmltools::p(
      class = "m-0 mt-1",
      "Your centralized platform for student housing portfolio analytics and insights."
    )
  )
}

app_title_ui <- function() {
  htmltools::tags$span(
    class = "navbar-brand",
    htmltools::tags$img(
      src = "www/logo.svg",
      alt = "GMH Communities Logo Dark",
      width = "auto",
      height = 50,
      class = "logo-light"
    )
  )
}

app_footer_ui <- function() {
  tags$footer(
    class = "footer py-3", # Removed mt-auto and bg-light classes
    tags$div(
      class = "container",
      tags$span(
        "© 2024 GMH DataHub" # Removed text-muted class
      )
    )
  )
}

app_links_menu_ui <- function() {
  bslib::nav_menu(
    title = "Links",
    align = "right",
    icon = bsicons::bs_icon("link-45deg"),
    bslib::nav_item(
      htmltools::tags$a(
        shiny::icon("book"),
        "Documentation",
        href = app_info("docs_url"),
        target = "_blank"
      )
    ),
    bslib::nav_item(
      htmltools::tags$a(
        shiny::icon("github"),
        "Source Code",
        href = app_info("repo_url"),
        target = "_blank"
      )
    )
  )
}

app_contact_menu_ui <- function() {
  bslib::nav_menu(
    title = "Contact",
    align = "right",
    icon = bsicons::bs_icon("envelope"),
    bslib::nav_item(
      htmltools::tags$a(
        shiny::icon("envelope"),
        "Email Support",
        href = "mailto:support@noclocks.dev",
        target = "_blank"
      )
    )
  )
}

app_user_menu_ui <- function() {
  bslib::nav_menu(
    title = "User",
    align = "right",
    icon = bsicons::bs_icon("person-circle"),
    bslib::nav_item(
      htmltools::tags$a(
        shiny::icon("user"),
        "User",
        href = "#"
      )
    ),
    bslib::nav_item(
      shiny::actionLink(
        inputId = "auth_logout",
        label = "Logout",
        icon = shiny::icon("sign-out-alt"),
        style = "display: inline-flex; align-items: center; padding: 2.5px 50px; width: -webkit-fill-available;"
      )
    )
  )
}

app_not_found_ui <- function() {
  shiny::httpResponse(
    status = 404L,
    contentType = "text/plain",
    content = "Not Found"
  )
}
