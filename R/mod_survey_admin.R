#  ------------------------------------------------------------------------
#
# Title : Survey Admin Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Admin Shiny Module
#'
#' @name mod_survey_admin
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Admin page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_admin_ui()`: User Interface (UI) for the module.
#' - `mod_survey_admin_server()`: Server logic for the module.
#' - `mod_survey_admin_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_admin_ui()`: UI output
#' - `mod_survey_admin_server()`: List of reactive expressions.
#' - `mod_survey_admin_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_admin_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_admin
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_admin_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::card(
        bslib::card_header(
          class = "d-flex justify-content-between align-items-center",
          htmltools::tags$h2("Survey Administration", class = "m-0"),
          htmltools::tags$div(
            class = "form-check form-switch",
            htmltools::tags$input(
              class = "form-check-input",
              type = "checkbox",
              id = ns("toggle_metrics"),
              checked = "checked",
              style = "cursor: pointer;"
            ),
            htmltools::tags$label(
              class = "form-check-label",
              `for` = ns("toggle_metrics"),
              htmltools::tags$span(bsicons::bs_icon("eye-fill"), "Show Metrics")
            )
          )
        ),
        htmltools::tags$div(
          id = ns("value_boxes"),
          class = "my-4",
          bslib::layout_column_wrap(
            width = 1/3,
            bslib::value_box(
              title = "Total Properties",
              value = shiny::textOutput(ns("val_properties")),
              showcase = bsicons::bs_icon("buildings"),
              theme = "primary",
              class = "shadow-sm"
            ),
            bslib::value_box(
              title = "Total Competitors",
              value = shiny::textOutput(ns("val_competitors")),
              showcase = bsicons::bs_icon("building"),
              theme = "primary",
              class = "shadow-sm"
            ),
            bslib::value_box(
              title = "Total Surveys",
              value = shiny::textOutput(ns("val_surveys")),
              showcase = bsicons::bs_icon("clipboard"),
              theme = "primary",
              class = "shadow-sm"
            )
          )
        )
      ),
      # actions section
      bslib::card(
        bslib::card_header("Actions"),
        bslib::card_body(
          htmltools::tags$span(
            class = "help-block",
            style = "align: center;",
            htmltools::tags$p(
              shiny::icon("info-circle"),
              "Use the buttons below to add new properties, competitors, or create a new survey."
            )
          ),
          bslib::layout_column_wrap(
            width = 1/3,
            shiny::actionButton(ns("add_property"), "Add New Property", icon = shiny::icon("plus"), class = "btn-primary"),
            shiny::actionButton(ns("add_competitor"), "Add New Competitor", icon = shiny::icon("plus"), class = "btn-primary"),
            shiny::actionButton(ns("create_survey"), "Create New Survey", icon = shiny::icon("plus"), class = "btn-success")
          )
        )
      ),

      # navset card
      bslib::navset_card_underline(
        id = ns("nav"),
        title = htmltools::tags$span(bsicons::bs_icon("clipboard"), "Survey Admin"),
        bslib::nav_panel(
          title = icon_text("dashboard", "Overview"),
          bslib::layout_columns(
            col_widths = c(4, 8),
            bslib::card(
              bslib::card_header("Survey Status"),
              bslib::card_body(
                reactable::reactableOutput(ns("survey_status_table"))
              )
            ),
            bslib::card(
              bslib::card_header("Property Map"),
              bslib::card_body(
                leaflet::leafletOutput(ns("property_map"))
              )
            )
          )
        ),
        bslib::nav_panel(
          title = icon_text("chart-line", "Survey Charts"),
          bslib::layout_column_wrap(
            width = 1/2,
            bslib::card(
              bslib::card_header("Survey Rate Adjustments"),
              bslib::card_body(
                # apexcharter::apexchartOutput(ns("survey_rate_adjustments_chart"))
              )
            ),
            bslib::card(
              bslib::card_header("Survey Market Velocity"),
              bslib::card_body(
                # apexcharter::apexchartOutput(ns("survey_market_velocity_chart"))
              )
            )
          )
        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_survey_admin
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_admin_server <- function(
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
      cli::cat_rule("[Module]: mod_survey_admin_server()")

      # check for logged in user
      user <- session$userData$user

      if (is.null(user)) {
        user <- list(
          user_id = 1,
          user_email = "jimmy.briggs@noclocks.dev",
          user_name = "Jimmy Briggs"
        )
      } else {
        if (shiny::is.reactive(user)) {
          user <- user()
        } else {
          user <- as.list(user)
        }
      }

      shiny::observeEvent(input$toggle_metrics, {
        if (!input$toggle_metrics) {
          shinyjs::hide("value_boxes", anim = TRUE)
        } else {
          shinyjs::show("value_boxes", anim = TRUE)
        }
      })

      # Reactive values and observers
      properties_data <- shiny::reactive({
        shiny::req(pool)
        db_read_tbl(pool, "mkt.properties")
      })

      survey_data <- shiny::reactive({
        shiny::req(pool)
        db_read_tbl(pool, "mkt.surveys")
      })

      universities_data <- shiny::reactive({
        shiny::req(pool)
        unis <- db_read_tbl(pool, "mkt.universities")
        locs <- db_read_tbl(pool, "mkt.university_locations")
        dplyr::left_join(
          unis,
          locs,
          by = c("university_id" = "university_id")
        ) |>
          dplyr::select(-coordinates)
      })

      map_data <- shiny::reactive({
        shiny::req(pool)
        properties <- db_read_tbl(pool, "mkt.locations") |>
          dplyr::filter(.data$is_competitor == FALSE)
        competitors <- db_read_tbl(pool, "mkt.locations") |>
          dplyr::filter(.data$is_competitor == TRUE)
        universities <- universities_data()

        list(
          properties = properties,
          competitors = competitors,
          universities = universities
        )
      })

      # Value box outputs
      output$val_properties <- shiny::renderText({
        properties_data()$property_id |>
          length() |>
          scales::comma()
      })

      output$val_competitors <- shiny::renderText({
        map_data()$competitors |>
          nrow() |>
          scales::comma()
      })

      output$val_surveys <- shiny::renderText({
        survey_data()$survey_id |>
          length() |>
          scales::comma()
      })

      # Action button observers
      shiny::observeEvent(input$add_property, {
        # show_add_property_modal()
      })
      shiny::observeEvent(input$add_competitor, {
        # show_add_competitor_modal()
      })

      shiny::observeEvent(input$create_survey, {

        shiny::showModal(
          shiny::modalDialog(
            title = "Create New Survey",
            easyClose = TRUE,
            htmltools::tagList(
              shiny::selectInput(
                ns("survey_property"),
                "Select a GMH Property:",
                choices = get_default_app_choices("properties"),
                selected = NULL
              ),
              shiny::checkboxInput(
                ns("use_current_leasing_week"),
                "Use Current Leasing Week?",
                value = TRUE
              ),
              shiny::dateInput(
                ns("survey_leasing_week"),
                "Survey Date:",
                value = get_leasing_week_start_date()
              ),
              shiny::textInput(
                ns("survey_user"),
                "Current User Email:",
                value = user$user_email
              )
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("confirm_survey_init"), "Initialize")
            )
          )
        )

        # initialize validator
        iv <- shinyvalidate::InputValidator$new()
        req_inputs <- c("survey_property", "survey_leasing_week", "survey_user")
        validation_msgs <- c(
          "Please select a property.",
          "Please select a leasing week.",
          "Please enter a user."
        )
        purrr::walk2(
          req_inputs,
          validation_msgs,
          function(input_id, validation_msg) {
            iv$add_rule(
              input_id,
              shinyvalidate::sv_required(message = validation_msg)
            )
          }
        )

        iv$add_rule(
          "survey_user",
          shinyvalidate::sv_email(message = "Please enter a valid email address.")
        )

        # observe leasing week check box to update date input
        shiny::observeEvent(input$use_current_leasing_week, {
          if (input$use_current_leasing_week) {
            shiny::updateDateInput(
              session,
              "survey_leasing_week",
              value = get_leasing_week_start_date()
            )
            shinyjs::disable("survey_leasing_week")
          } else {
            shinyjs::enable("survey_leasing_week")
          }
        })

        # survey data reactive
        survey_data <- shiny::reactive({
          shiny::req(
            input$survey_property,
            input$survey_leasing_week,
            input$survey_user
          )

          user_id <- get_user_id_by_email(pool = pool, email = input$survey_user)
          property_name <- get_property_name_by_id(property_id = input$survey_property)

          tibble::tibble(
            property_id = input$survey_property,
            leasing_week = input$survey_leasing_week,
            user_id = user_id,
            user_email = input$survey_user,
            property_name = property_name,
            survey_date = Sys.Date(),
            survey_status = "Initialized"
          )
        })

        close <- function() {
          shiny::removeModal()
          iv$disable()
        }

        # observe confirm/submit button to create new survey and remove the modal
        shiny::observeEvent(input$confirm_survey_init, {
          iv$enable()
          valid_inputs <- iv$is_valid()
          if (valid_inputs) {
            new_survey <- survey_data()
            browser()
            db_mkt_insert_tbl(pool, "mkt.surveys", new_survey)
            close()
            shiny::showNotification(
              "Survey initialized successfully.",
              type = "message"
            )
          } else {
            shiny::showNotification(
              "Please correct the errors before continuing.",
              type = "warning"
            )
          }
        })

        shiny::observeEvent(input$cancel_survey_init, {
          close()
          shiny::showNotification(
            "Survey initialization cancelled.",
            type = "message"
          )
        })
      })

      output$survey_status_table <- reactable::renderReactable({

        properties_data <- properties_data()
        survey_data <- survey_data()
        map_data <- map_data()

        combined_data <- properties_data |>
          dplyr::left_join(
            survey_data |>
              dplyr::group_by(property_id) |>
              dplyr::filter(survey_date == max(survey_date)) |>
              dplyr::ungroup(),
            by = "property_id"
          ) |>
          dplyr::left_join(
            map_data$properties |>
              dplyr::select(property_id, address) |>
              dplyr::bind_rows(
                map_data$competitors |>
                  dplyr::select(property_id, address)
              ),
            by = "property_id"
          ) |>
          dplyr::transmute(
            name = property_name,
            type = ifelse(is_competitor, "Competitor", "Property"),
            address = address,
            last_survey_date = survey_date,
            survey_status = case_when(
              is.na(survey_date) ~ "Pending",
              difftime(Sys.Date(), survey_date, units = "days") > 7 ~ "Overdue",
              TRUE ~ "Complete"
            ),
            occupancy = as.numeric(NA),
            avg_rent = as.numeric(NA)
          )

        tbl_survey_properties_competitors(combined_data)
      })

      output$property_map <- leaflet::renderLeaflet({

        properties <- map_data()$properties
        competitors <- map_data()$competitors
        universities <- map_data()$universities

        map_survey_locations(
          properties = properties,
          competitors = competitors,
          universities = universities
        )
      })

      return(
        list(
          properties_data = properties_data,
          survey_data = survey_data,
          universities_data = universities_data,
          map_data = map_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_admin
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_admin_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Admin",
    window_title = "Demo: Survey Admin",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Admin",
      value = "survey_admin",
      icon = bsicons::bs_icon("house"),
      shinyjs::useShinyjs(),
      mod_survey_admin_ui("demo")
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    mod_survey_admin_server("demo", pool)
  }

  shiny::shinyApp(ui, server)
}

# modals ------------------------------------------------------------------

mod_survey_init_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::selectInput(
      ns("survey_property"),
      "Select a GMH Property:",
      choices = get_default_app_choices("properties"),
      selected = NULL
    ),
    shiny::checkboxInput(
      ns("use_current_leasing_week"),
      "Use Current Leasing Week?",
      value = TRUE
    ),
    shiny::dateInput(
      ns("survey_leasing_week"),
      "Survey Date:",
      value = get_leasing_week_start_date()
    ),
    shiny::textInput(
      ns("survey_user"),
      "Current User Email:",
      value = NULL
    )
  )

}

mod_survey_init_server <- function(id, pool = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      if (is.null(pool)) pool <- session$userData$pool
      check_db_conn(pool)
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_admin_survey_init_modal_server()")

      # initialize validator
      iv <- shinyvalidate::InputValidator$new()
      req_inputs <- c("survey_property", "survey_leasing_week", "survey_user")
      validation_msgs <- c(
        "Please select a property.",
        "Please select a leasing week.",
        "Please enter a user."
      )
      purrr::walk2(
        req_inputs,
        validation_msgs,
        function(input_id, validation_msg) {
          iv$add_rule(
            input_id,
            shinyvalidate::sv_required(message = validation_msg)
          )
        }
      )

      iv$add_rule(
        "survey_user",
        shinyvalidate::sv_email(message = "Please enter a valid email address.")
      )

      # observe leasing week check box to update date input
      shiny::observeEvent(input$use_current_leasing_week, {
        if (input$use_current_leasing_week) {
          shiny::updateDateInput(
            session,
            "survey_leasing_week",
            value = get_leasing_week_start_date()
          )
          shinyjs::disable("survey_leasing_week")
        } else {
          shinyjs::enable("survey_leasing_week")
        }
      })

      # survey data reactive
      survey_data <- shiny::reactive({
        shiny::req(
          input$survey_property,
          input$survey_leasing_week,
          input$survey_user
        )

        user_id <- get_user_id_by_email(pool, input$survey_user)
        property_name <- get_property_name_by_id(pool, input$survey_property)

        tibble::tibble(
          property_id = input$survey_property,
          leasing_week = input$survey_leasing_week,
          user_id = user_id,
          user_email = input$survey_user,
          property_name = property_name,
          survey_date = Sys.Date(),
          survey_status = "Initialized"
        )
      })

      close <- function() {
        shiny::removeModal()
        iv$disable()
      }

      # observe confirm/submit button to create new survey and remove the modal
      shiny::observeEvent(input$confirm_survey_init, {
        iv$enable()
        valid_inputs <- iv$is_valid()
        if (valid_inputs) {
          new_survey <- survey_data()
          db_mkt_insert_tbl(pool, "mkt.surveys", new_survey)
          close()
          shiny::showNotification(
            "Survey initialized successfully.",
            type = "message"
          )
        } else {
          shiny::showNotification(
            "Please correct the errors before continuing.",
            type = "warning"
          )
        }
      })

      shiny::observeEvent(input$cancel_survey_init, {
        close()
      })

      return(list(survey_data = survey_data))

    }
  )
}

