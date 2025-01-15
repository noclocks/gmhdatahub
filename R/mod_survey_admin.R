
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
    bslib::card_header(
      bslib::card_header(
        # class = 'bg-dark text-white',
        style = 'display: contents;',
        bsicons::bs_icon('table'),
        'Surveys',
        shiny::actionButton(
          ns('add_survey'),
          'Add Survey',
          icon = shiny::icon('plus'),
          class = 'btn-sm btn-primary float-end'
        )
      )
    ),
    bslib::card_body(
      reactable::reactableOutput(ns('surveys_admin_table')) |>
        with_loader()
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
  global_filters = NULL,
  navigate = navigate
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

      # Surveys Admin table ####
      surveys <- shiny::reactive({
        # Query existing surveys
        # TODO: move to function
        db_read_tbl(pool, 'mkt_dump.surveys', collect = FALSE) |>
          # Query the most recent survey for each unique `property_name`
          dplyr::group_by(property_name, survey_week) |>
          dplyr::filter(created_at == max(created_at)) |>
          dplyr::ungroup() |>
          dplyr::select(
            survey_uid,
            property_id,
            property_name,
            survey_week,
            created_at,
            user_email
          ) |>
          dplyr::collect()
      })

      output$surveys_admin_table <- reactable::renderReactable({
        out <- surveys() |>
          dplyr::select(-survey_uid, -property_id)

        reactable::reactable(
          out,
          columns = list(
            # survey_uid = reactable::colDef(
            #   name = 'Survey ID',
            #   show = FALSE
            # ),
            # property_id = reactable::colDef(
            #   name = 'Property ID',
            #   show = FALSE,
            # ),
            property_name = reactable::colDef(
              name = 'Property Name'
            ),
            survey_week = reactable::colDef(
              name = 'Survey Week'
            ),
            created_at = reactable::colDef(
              name = 'Created At'
            ),
            user_email = reactable::colDef(
              name = 'User Email'
            )
          )
        )
      })

      # Modal - Add Survey ####
      # shiny::observeEvent(input$add_survey, {
      #   shiny::showModal(
      #     shiny::modalDialog(
      #       title = "Property Summary",
      #       size = "l",
      #       bslib::layout_columns(
      #         col_widths = c(6, 6),
      #         bslib::card(
      #           bslib::card_body(
      #             shiny::textInput(session$ns("property_name_input"), "Property Name"),
      #             shiny::textInput(session$ns("website_input"), "Website URL"),
      #             shiny::textInput(session$ns("address_input"), "Address"),
      #             shiny::textInput(session$ns("phone_input"), "Phone Number"),
      #             radioButtons(
      #               session$ns("image_input_type"),
      #               "Image Input Type",
      #               choices = c("URL" = "url", "File Upload" = "file"),
      #               selected = "url"
      #             ),
      #             conditionalPanel(
      #               condition = sprintf("input['%s'] == 'url'", session$ns("image_input_type")),
      #               textInput(session$ns("image_url_input"), "Image URL")
      #             ),
      #             conditionalPanel(
      #               condition = sprintf("input['%s'] == 'file'", session$ns("image_input_type")),
      #               shiny::fileInput(session$ns("image_file"), "Upload Image", accept = c('image/png', 'image/jpeg', 'image/gif'))
      #             )
      #           )
      #         ),
      #         bslib::card(
      #           bslib::card_body(
      #             shiny::selectInput(
      #               session$ns("type_input"),
      #               "Property Type",
      #               choices = c("Student", "Conventional", "Affordable", "Innovative")
      #             ),
      #             shiny::sliderInput(
      #               session$ns("rating_input"),
      #               "Rating",
      #               min = 0,
      #               max = 5,
      #               value = 0,
      #               step = 0.5
      #             ),
      #             selectInput(session$ns("status_input"), "Status",
      #                         choices = c("New Construction", "Operational", "Undergoing Renovation")
      #                         ),
      #             numericInput(session$ns("year_built_input"), "Year Built",
      #                          min = 1900, max = 2023, value = as.numeric(lubridate::year(Sys.Date()))),
      #             sliderInput(
      #               session$ns("distance_input"), "Distance (miles)",
      #                         min = 0, max = 10, step = 0.1,
      #               value = 0
      #               )
      #           )
      #         )
      #       ),
      #       footer = shiny::actionButton(
      #         ns('next_property_summary'),
      #         'Next',
      #         class = 'btn-primary'
      #       )
      #     ))
      # })
      #
      # shiny::observeEvent(input$next_property_summary, {
      #   shiny::showModal(
      #     shiny::modalDialog(
      #       title = 'Leasing Summary',
      #       size = 'l',
      #       htmltools::tagList(
      #         bslib::card(
      #           bslib::card_header(
      #             class = "d-flex justify-content-between align-items-center",
      #             htmltools::tags$h3(
      #               class = "m-0",
      #               shiny::textOutput(ns("property_name_title"))
      #             ),
      #             shiny::actionButton(
      #               ns("edit"),
      #               "Edit",
      #               icon = shiny::icon("edit"),
      #               class = "btn-sm btn-primary"
      #             )
      #           ),
      #           bslib::card_body(
      #             class = "p-0",
      #             bslib::layout_columns(
      #               col_widths = c(6, 6),
      #               gap = "1rem",
      #               bslib::card(
      #                 shiny::selectInput(
      #                   ns("reporting_cycle"),
      #                   label = htmltools::tags$span(
      #                     bsicons::bs_icon("calendar-week"),
      #                     "Reporting Cycle"
      #                   ),
      #                   choices = get_survey_choices(section = "leasing_summary", type = "reporting_cycle"),
      #                   selected = get_survey_choices(section = "leasing_summary", type = "reporting_cycle")[[3]]
      #                 ),
      #                 shiny::dateInput(
      #                   ns("lease_launch_date"),
      #                   label = htmltools::tags$span(
      #                     bsicons::bs_icon("calendar-check"),
      #                     "Lease Launch Date"
      #                   ),
      #                   value = NULL
      #                 ),
      #                 shiny::dateInput(
      #                   ns("renewal_launch_date"),
      #                   label = htmltools::tags$span(
      #                     bsicons::bs_icon("calendar-plus"),
      #                     "Renewal Launch Date"),
      #                   value = NULL
      #                 ),
      #                 shiny::numericInput(
      #                   ns("current_occupancy"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("percent"), "Current Occupancy (%)"),
      #                   value = 0,
      #                   min = 0,
      #                   max = 100,
      #                   step = 1
      #                 ),
      #                 shiny::numericInput(
      #                   ns("prior_year_occupancy"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("clock-history"), "Prior Year Occupancy (%)"),
      #                   value = 0,
      #                   min = 0,
      #                   max = 100,
      #                   step = 1
      #                 ),
      #                 shiny::numericInput(
      #                   ns("current_prelease"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("graph-up"), "Current Pre-Lease (%)"),
      #                   value = 38.8
      #                 ),
      #                 shiny::numericInput(
      #                   ns("last_year_prelease"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("clock-history"), "Prior Year Pre-Lease (%)"),
      #                   value = 0,
      #                   min = 0,
      #                   max = 100,
      #                   step = 1
      #                 )
      #               ),
      #               bslib::card(
      #                 shiny::numericInput(
      #                   ns("total_renewals"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("arrow-repeat"), "Total Renewals"),
      #                   value = 0,
      #                   min = 0,
      #                   step = 1
      #                 ),
      #                 shiny::numericInput(
      #                   ns("total_new_leases"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("file-earmark-plus"), "Total New Leases"),
      #                   value = 0,
      #                   min = 0,
      #                   step = 1
      #                 ),
      #                 shiny::numericInput(
      #                   ns("weekly_traffic"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("people"), "Total Weekly Traffic"),
      #                   value = NULL,
      #                   min = 0,
      #                   step = 1
      #                 ),
      #                 shiny::selectInput(
      #                   ns("current_incentive"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("gift"), "Current Incentive"),
      #                   choices = get_survey_choices("leasing_summary", "current_incentive"),
      #                   selected = get_survey_choices("leasing_summary", "current_incentive")[[1]]
      #                 ),
      #                 shiny::numericInput(
      #                   ns("incentive_amount"),
      #                   label = htmltools::tags$span(bsicons::bs_icon("cash-stack"), "Incentive Amount ($)"),
      #                   value = 0
      #                 )
      #               ),
      #               shiny::dateInput(
      #                 ns("data_last_updated"),
      #                 label = htmltools::tags$span(bsicons::bs_icon("clock"), "Data Last Updated"),
      #                 value = Sys.Date()
      #               )
      #             )
      #           )
      #         )
      #       ),
      #       footer = shiny::actionButton(
      #         'next_leasing_summary',
      #         'Next',
      #         class = 'btn-primary'
      #       )
      #     )
      #   )
      # })

      # Nav - Add Survey ####
      shiny::observeEvent(input$add_survey, {
        properties <- pool |>
          dplyr::tbl(I("gmh.properties")) |>
          dplyr::filter(is_disabled == FALSE) |>
          dplyr::select(entrata_property_id, property_name) |>
          dplyr::collect()

        property_choices <- properties |>
          dplyr::pull(property_name)

        property_choies <- setNames(property_choices, properties$entrata_property_id)

        shiny::showModal(
          shiny::modalDialog(
            size = 'm',
            title = 'Add a New Survey',
            footer = shiny::tagList(
              shiny::modalButton('Cancel'),
              shiny::actionButton(
                ns('submit_add_survey'),
                'Create',
                icon = shiny::icon('plus'),
                class = 'btn-primary'
              )
            ),
            shinyWidgets::radioGroupButtons(
              inputId = ns("property_competitor_radio"),
              label = NULL,
              choices = c("Property", "Competitor"),
              justified = TRUE
            ),
            shinyWidgets::pickerInput(
              ns('add_survey_properties'),
              'Competitor Property',
              choices = property_choices,
              multiple = FALSE,
              inline = FALSE,
              width = '100%',
              options = shinyWidgets::pickerOptions(
                actionsBox = TRUE,
                liveSearch = TRUE
              )
            ),
            shiny::textInput(
              ns('add_survey_competitor_name'),
              'Competitor Name',
              placeholder = 'Property Name'
            ) |> shinyjs::hidden()
          )
        )
      })

      # Show/Hide `Property` pickerInput in `Add Property` modal
      shiny::observeEvent(input$property_competitor_radio, {
        if (input$property_competitor_radio == 'Property') {
          shinyjs::hide('add_survey_competitor_name')
        } else {
          shinyjs::show('add_survey_competitor_name')
        }
      })

      session$userData$add_survey_trigger <- shiny::reactiveVal(NULL)

      # Save new `Property/Competitor`
      shiny::observeEvent(input$submit_add_survey, {
        # TODO: Show `New Survey` UI
        shiny::removeModal()

        new_survey_info <- list(
          property_id = input$add_survey_properties,
          competitor_name = input$add_survey_competitor_name
        )

        session$userData$add_survey_trigger(new_survey_info)

        on.exit({
          navigate('survey_forms')
        }, add = TRUE)
      })

      return(
        list(
          # reactive values
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
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Admin",
      value = "survey_admin",
      icon = bsicons::bs_icon("house"),
      mod_survey_admin_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_admin_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------

