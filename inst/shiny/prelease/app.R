library(gmhdatahub)
library(shiny)
require(config)

# ensure config file is present
Sys.setenv(R_CONFIG_FILE = "config.yml")
if (!file.exists(Sys.getenv("R_CONFIG_FILE"))) {
  cli::cli_abort(
    c(
      "Configuration file {.field config.yml} not found. ",
      "Please ensure the file exists."
    )
  )
}

auth_config <- config::get("auth")
app_config <- config::get("app")

noClocksAuthR:::set_api_url(api_url = auth_config$base_url)

ui <- function(req) {

  force(req)

  if (!is.null(req)) {
    http_method <- req$REQUEST_METHOD
    path_info <- req$PATH_INFO
    if (http_method == "GET" && path_info == "/health") {
      return(gmhdatahub:::app_healthcheck(req))
    }
  }

  htmltools::tagList(
    gmhdatahub:::add_external_resources(),
    bslib::page_navbar(
      id = "nav",
      lang = "en",
      window_title = "GMH DataHub",
      position = "static-top",
      theme = gmhdatahub:::app_theme_ui(),
      title = gmhdatahub:::app_title_ui(),
      footer = gmhdatahub:::app_footer_ui(),
      bslib::nav_spacer(),
      bslib::nav_panel(
        title = "Pre Lease",
        value = "pre_lease",
        icon = bsicons::bs_icon("house"),
        gmhdatahub:::mod_pre_lease_ui("prelease")
      ),
      bslib::nav_spacer(),
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

secure_ui <- noClocksAuthR::secure_ui(
  ui = ui,
  sign_in_page_ui = gmhdatahub:::custom_sign_in_ui,
  custom_admin_button_ui = noClocksAuthR::admin_button_ui(align = "left")
) |>
  gmhdatahub:::healthcheck_ui()


server <- function(input, output, session) {

  output$signed_in_as <- shiny::renderText({
    session$userData$user()$email
  })

  shiny::observeEvent(input$auth_logout, {
    noClocksAuthR::sign_out_from_shiny(session)
    session$reload()
  })

  # initialize database connection pool
  pool <- gmhdatahub:::db_connect()
  session$userData$pool <- pool

  # waiter
  waiter::waiter_hide()


  gmhdatahub:::mod_pre_lease_server(
    "prelease",
    pool = pool
  )
}


secure_server <- noClocksAuthR::secure_server(
  server = server,
  custom_sign_in_server = noClocksAuthR::sign_in_module_2
)

on_start <- function() {
  noClocksAuthR::noclocksauthr_config(
    app_name = app_config$id,
    api_key = auth_config$api_key,
    is_invite_required = ifelse(Sys.getenv("R_CONFIG_ACTIVE") == "production", TRUE, FALSE),
    is_email_verification_required = FALSE
  )
}

# run app
shiny::shinyApp(
  ui = secure_ui,
  server = secure_server,
  onStart = on_start,
  uiPattern = ".*",
  options = app_opts(host = "0.0.0.0", port = 8080)
)
