#  ------------------------------------------------------------------------
#
# Title : Survey Leasing Summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Leasing Summary Shiny Module
#'
#' @name mod_survey_leasing_summary
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Leasing Summary page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_leasing_summary_ui()`: User Interface (UI) for the module.
#' - `mod_survey_leasing_summary_server()`: Server logic for the module.
#' - `mod_survey_leasing_summary_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_leasing_summary_ui()`: UI output
#' - `mod_survey_leasing_summary_server()`: List of reactive expressions.
#' - `mod_survey_leasing_summary_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_leasing_summary_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_leasing_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_leasing_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    # page ------------------------------------------------------------------------------------------------------------
    bslib::page_fluid(
      # card --------------------------------------------------------------------
      bslib::card(
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("clipboard"),
            htmltools::tags$span(
              "Leasing Summary - ",
              shiny::textOutput(ns("property_name_title"), inline = TRUE)
            ),
            shiny::actionButton(
              ns("refresh"),
              "Refresh Data",
              class = "btn-sm btn-outline-light float-end",
              style = "width: auto;"
            )
          )
        ),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6, 6, 6),
            # occupancy statistics -----------------------------------------------------------------------------
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::tags$h5(bsicons::bs_icon("calculator"), " Occupancy Statistics")
              ),
              bslib::card_body(
                bslib::value_box(
                  title = "Current Occupancy %",
                  value = shiny::textOutput(ns("current_occupancy"), inline = TRUE),
                  theme = "primary",
                  showcase = bsicons::bs_icon("people")
                ),
                bslib::value_box(
                  title = "Current Pre-Lease %",
                  value = shiny::textOutput(ns("current_pre_lease"), inline = TRUE),
                  theme = "primary",
                  showcase = bsicons::bs_icon("graph-up")
                ),
                bslib::value_box(
                  title = "Prior Year Occupancy %",
                  value = shiny::textOutput(ns("prior_occupancy"), inline = TRUE),
                  theme = "warning",
                  showcase = bsicons::bs_icon("clock-history")
                ),
                bslib::value_box(
                  title = "Prior Year Pre-Lease %",
                  value = shiny::textOutput(ns("prior_pre_lease"), inline = TRUE),
                  theme = "warning",
                  showcase = bsicons::bs_icon("clock-history")
                )
              )
            ),
            # leasing activity ---------------------------------------------------------------------------------
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::tags$h5(bsicons::bs_icon("calculator"), " Leasing Activity")
              ),
              bslib::card_body(
                bslib::value_box(
                  title = "Total Renewals",
                  value = shiny::textOutput(ns("total_renewals")),
                  theme = "success",
                  showcase = bsicons::bs_icon("arrow-repeat")
                ),
                bslib::value_box(
                  title = "Total New Leases",
                  value = shiny::textOutput(ns("total_new_leases")),
                  theme = "success",
                  showcase = bsicons::bs_icon("file-earmark-plus")
                ),
                bslib::value_box(
                  title = "Weekly Leases",
                  value = shiny::textOutput(ns("weekly_leases")),
                  theme = "success",
                  showcase = bsicons::bs_icon("calendar-week")
                ),
                bslib::value_box(
                  title = "Weekly Traffic",
                  value = shiny::textOutput(ns("weekly_traffic")),
                  theme = "success",
                  showcase = bsicons::bs_icon("people")
                )
              )
            ),
            # important dates ---------------------------------------------------------------------------------
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::tags$h5(bsicons::bs_icon("calendar"), " Important Dates")
              ),
              bslib::card_body(
                htmltools::tags$p(
                  htmltools::tags$strong(
                    "Lease Launch: "
                  ),
                  shiny::textOutput(ns("lease_launch"), inline = TRUE)
                ),
                htmltools::tags$p(
                  htmltools::tags$strong(
                    "Renewal Launch: "
                  ),
                  shiny::textOutput(ns("renewal_launch"), inline = TRUE)
                ),
                htmltools::tags$p(
                  htmltools::tags$strong(
                    "Reporting Cycle: "
                  ),
                  shiny::textOutput(ns("reporting_cycle"), inline = TRUE)
                )
              )
            ),
            # incentives ---------------------------------------------------------------------------------
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::tags$h5(bsicons::bs_icon("cash-coin"), " Incentives")
              ),
              bslib::card_body(
                htmltools::tags$p(
                  htmltools::tags$strong(
                    "Current Incentive: "
                  ),
                  shiny::textOutput(ns("current_incentive"), inline = TRUE)
                ),
                htmltools::tags$p(
                  htmltools::tags$strong(
                    "Incentive Amount: "
                  ),
                  shiny::textOutput(ns("incentive_amount"), inline = TRUE)
                )
              )
            )
          )
        ),
        bslib::card_footer(
          htmltools::tags$p(
            "Data last updated: ",
            shiny::textOutput(ns("last_updated"), inline = TRUE)
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_leasing_summary
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_leasing_summary_server <- function(
    id,
    pool = NULL,
    survey_data = NULL,
    selected_filters = NULL,
    db_trigger_func = NULL,
    edit_survey_section = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # setup ------------------------------------------------------------
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_leasing_summary_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- leasing_summary_validator()

      # filters
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------

      # initial data
      leasing_summary_data <- shiny::reactive({
        shiny::req(survey_data$leasing_summary)
        if (nrow(survey_data$leasing_summary) == 0) {
          cli::cli_alert_warning("Leasing Summary Data Empty.")
        }
        survey_data$leasing_summary
      })

      # inputs data
      inputs_data <- shiny::reactive({
        shiny::req(
          # selected_filters,
          input$reporting_cycle,
          input$lease_launch_date,
          input$renewal_launch_date,
          input$current_occupancy,
          input$prior_year_occupancy,
          input$current_pre_lease,
          input$prior_year_pre_lease,
          input$total_renewals,
          input$total_new_leases,
          input$weekly_leases,
          input$weekly_traffic,
          input$current_incentive,
          input$incentive_amount
        )
        tibble::tibble(
          # survey_id = selected_filters$survey_id,
          # property_id = selected_filters$property_id,
          # competitor_id = selected_filters$competitor_id,
          # leasing_week_id = selected_filters$leasing_week_id,
          # property_name = selected_filters$property_name,
          reporting_cycle = input$reporting_cycle,
          lease_launch_date = input$lease_launch_date,
          renewal_launch_date = input$renewal_launch_date,
          current_occupancy = input$current_occupancy,
          prior_year_occupancy = input$prior_year_occupancy,
          current_pre_lease = input$current_pre_lease,
          prior_year_pre_lease = input$prior_year_pre_lease,
          total_renewals = input$total_renewals,
          total_new_leases = input$total_new_leases,
          weekly_leases = input$weekly_leases,
          weekly_traffic = input$weekly_traffic,
          current_incentive = input$current_incentive,
          incentive_amount = input$incentive_amount
        )
      })

      # UI outputs  ---------------------------------------------------------------
      output$last_updated <- shiny::renderText({
        leasing_summary_data()$updated_at |>
          max(na.rm = TRUE) |>
          format("%Y-%m-%d %I:%M %p")
      })

      output$property_name_title <- shiny::renderText({
        shiny::req(leasing_summary_data())
        prop_id <- leasing_summary_data()$property_id
        comp_id <- leasing_summary_data()$competitor_id
        name <- leasing_summary_data()$property_name
        if (is.na(comp_id)) {
          paste0(name, " (", prop_id, ")")
        } else {
          paste0(name, " (Competitor #", comp_id, ")")
        }
      })

      output$current_occupancy <- shiny::renderText({
        leasing_summary_data()$current_occupancy |>
          scales::percent(accuracy = 0.01)
      })

      output$prior_occupancy <- shiny::renderText({
        leasing_summary_data()$prior_year_occupancy |>
          scales::percent(accuracy = 0.01)
      })

      output$current_pre_lease <- shiny::renderText({
        leasing_summary_data()$current_pre_lease |>
          scales::percent(accuracy = 0.01)
      })

      output$prior_pre_lease <- shiny::renderText({
        leasing_summary_data()$prior_year_pre_lease |>
          scales::percent(accuracy = 0.01)
      })

      output$total_renewals <- shiny::renderText({
        leasing_summary_data()$total_renewals |>
          scales::comma()
      })

      output$total_new_leases <- shiny::renderText({
        leasing_summary_data()$total_new_leases |>
          scales::comma()
      })

      output$weekly_leases <- shiny::renderText({
        leasing_summary_data()$weekly_leases |>
          scales::comma()
      })

      output$weekly_traffic <- shiny::renderText({
        leasing_summary_data()$weekly_traffic |>
          scales::comma()
      })

      output$lease_launch <- shiny::renderText({
        leasing_summary_data()$lease_launch_date |>
          format("%Y-%m-%d")
      })

      output$renewal_launch <- shiny::renderText({
        leasing_summary_data()$renewal_launch_date |>
          format("%Y-%m-%d")
      })

      output$reporting_cycle <- shiny::renderText({
        leasing_summary_data()$reporting_cycle
      })

      output$current_incentive <- shiny::renderText({
        leasing_summary_data()$current_incentive
      })

      output$incentive_amount <- shiny::renderText({
        leasing_summary_data()$incentive_amount |>
          scales::dollar(accuracy = 0.01)
      })

      # edit modal --------------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab())

        if (session$userData$selected_survey_tab() != "nav_leasing_summary") {
          return()
        }

        data <- leasing_summary_data()

        iv$initialize()
        iv$enable()

        shiny::showModal(
          shiny::modalDialog(
            title = "Leasing Summary",
            size = "xl",
            easyClose = TRUE,
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                shiny::selectInput(
                  ns("reporting_cycle"),
                  "Reporting Cycle",
                  choices = get_survey_choices(section = "leasing_summary", type = "reporting_cycle"),
                  selected = data$reporting_cycle
                ),
                shiny::dateInput(
                  ns("lease_launch_date"),
                  "Lease Launch Date",
                  value = ifelse(
                    length(data$lease_launch_date) == 0,
                    lubridate::today(),
                    data$lease_launch_date
                  )
                ),
                shiny::dateInput(
                  ns("renewal_launch_date"),
                  "Renewal Launch Date",
                  value = ifelse(
                    length(data$renewal_launch_date) == 0,
                    lubridate::today(),
                    data$renewal_launch_date
                  )
                ),
                shiny::sliderInput(
                  ns("current_occupancy"),
                  "Current Occupancy %",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  ticks = TRUE,
                  post = "%",
                  value = ifelse(
                    length(data$current_occupancy) == 0,
                    0,
                    round(data$current_occupancy, 2)
                  )
                ),
                shiny::sliderInput(
                  ns("prior_year_occupancy"),
                  "Prior Year Occupancy %",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  ticks = TRUE,
                  post = "%",
                  value = ifelse(
                    length(data$prior_year_occupancy) == 0,
                    0,
                    round(data$prior_year_occupancy, 2)
                  )
                ),
                shiny::sliderInput(
                  ns("current_pre_lease"),
                  "Current Pre-Lease %",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  ticks = TRUE,
                  post = "%",
                  value = ifelse(
                    length(data$current_pre_lease) == 0,
                    0,
                    round(data$current_pre_lease, 2)
                  )
                ),
                shiny::sliderInput(
                  ns("prior_year_pre_lease"),
                  "Prior Year Pre-Lease %",
                  min = 0,
                  max = 1,
                  step = 0.01,
                  ticks = TRUE,
                  post = "%",
                  value = ifelse(
                    length(data$prior_year_pre_lease) == 0,
                    0,
                    round(data$prior_year_pre_lease, 2)
                  )
                )
              ),
              bslib::card(
                shiny::numericInput(
                  ns("total_renewals"),
                  "Total Renewals",
                  min = 0,
                  step = 1,
                  value = ifelse(
                    length(data$total_renewals) == 0,
                    0,
                    data$total_renewals
                  )
                ),
                shiny::numericInput(
                  ns("total_new_leases"),
                  "Total New Leases",
                  min = 0,
                  step = 1,
                  value = ifelse(
                    length(data$total_new_leases) == 0,
                    0,
                    data$total_new_leases
                  )
                ),
                shiny::numericInput(
                  ns("weekly_leases"),
                  "Weekly Leases",
                  min = 0,
                  step = 1,
                  value = ifelse(
                    length(data$weekly_leases) == 0,
                    0,
                    data$weekly_leases
                  )
                ),
                shiny::numericInput(
                  ns("weekly_traffic"),
                  "Weekly Traffic",
                  min = 0,
                  step = 1,
                  value = ifelse(
                    length(data$weekly_traffic) == 0,
                    0,
                    data$weekly_traffic
                  )
                ),
                shiny::selectInput(
                  ns("current_incentive"),
                  "Current Incentive",
                  choices = get_survey_choices(section = "leasing_summary", type = "current_incentive"),
                  selected = data$current_incentive,
                ),
                shiny::numericInput(
                  ns("incentive_amount"),
                  "Incentive Amount",
                  min = 0,
                  step = 1,
                  value = ifelse(
                    length(data$incentive_amount) == 0,
                    0,
                    data$incentive_amount
                  )
                ),
                shiny::dateInput(
                  ns("data_last_updated"),
                  "Data Last Updated",
                  value = data$updated_at
                ) |> shinyjs::disabled()
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              bslib::card(
                bslib::card_header("Review Changes"),
                bslib::card_body(
                  shiny::uiOutput(ns("changes_preview"))
                )
              )
            ),
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      shiny::observeEvent(input$current_incentive, {
        if (input$current_incentive != "None") {
          shinyjs::enable("incentive_amount")
        } else {
          shinyjs::disable("incentive_amount")
        }
      })

      # changes -----------------------------------------------------------------
      changes <- shiny::reactive({
        shiny::req(leasing_summary_data(), inputs_data())

        original_data <- leasing_summary_data()
        new_data <- inputs_data()

        changes <- purrr::map(
          names(new_data),
          function(field) {
            if (!is.na(new_data[[field]]) && !isTRUE(all.equal(new_data[[field]], original_data[[field]]))) {
              old_value <- if (is.numeric(original_data[[field]])) {
                round(original_data[[field]], 2)
              } else {
                original_data[[field]]
              }

              new_value <- if (is.numeric(new_data[[field]])) {
                round(new_data[[field]], 2)
              } else {
                new_data[[field]]
              }

              if (!isTRUE(all.equal(old_value, new_value))) {
                list(
                  field = field,
                  old = old_value,
                  new = new_value
                )
              }
            }
          }
        ) |>
          rlang::set_names(names(new_data)) |>
          purrr::compact()

        changes
      })

      shiny::observe({
        shiny::req(changes())
        num_changes <- length(changes())
        if (num_changes > 0) {
          cli::cli_alert_info("{.field {num_changes}} changes detected in the Property Summary.")
          shinyjs::enable("save_changes")
          shiny::updateDateInput(session, "data_last_updated", value = lubridate::now())
        } else {
          shinyjs::disable("save_changes")
        }
      })

      output$changes_preview <- shiny::renderUI({
        shiny::req(changes())
        changes_data <- changes()
        if (length(changes_data) == 0) {
          return(htmltools::tags$p("No changes detected.", class = "text-muted"))
        }
        changes_ui <- lapply(names(changes_data), function(field) {
          htmltools::tags$div(
            htmltools::tags$p(
              htmltools::tags$strong(
                paste0(
                  tools::toTitleCase(
                    gsub("_", " ", field)
                  ),
                  ":"
                )
              ),
              htmltools::tags$span(
                paste(
                  "Current:",
                  changes_data[[field]]$old
                ),
                style = "color: #666;"
              ),
              htmltools::tags$span(
                "→",
                style = "margin: 0 10px;"
              ),
              htmltools::tags$span(
                paste(
                  "New:",
                  changes_data[[field]]$new
                ),
                style = "color: #007bff;"
              )
            )
          )
        })
        do.call(htmltools::tagList, changes_ui)
      })

      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {
        shiny::req(changes())

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NA_integer_
          comp_id <- as.integer(selected_filters$competitor_id)
          prop_name <- selected_filters$competitor_name
        } else {
          prop_id <- as.integer(selected_filters$property_id)
          comp_id <- NA_integer_
          prop_name <- selected_filters$property_name
        }

        leasing_week_id <- as.integer(selected_filters$leasing_week_id)
        survey_id <- as.integer(selected_filters$survey_id)
        user_id <- selected_filters$user_id

        new_values <- inputs_data() |>
          dplyr::mutate(
            property_id = prop_id,
            competitor_id = comp_id,
            property_name = prop_name,
            leasing_week_id = leasing_week_id,
            survey_id = survey_id,
            updated_by = user_id
          ) |>
          dplyr::select(
            survey_id,
            property_id,
            competitor_id,
            leasing_week_id,
            property_name,
            reporting_cycle,
            lease_launch_date,
            renewal_launch_date,
            current_occupancy,
            prior_year_occupancy,
            current_pre_lease,
            prior_year_pre_lease,
            total_renewals,
            total_new_leases,
            weekly_leases,
            weekly_traffic,
            current_incentive,
            incentive_amount,
            updated_by
          )

        db_update_survey_leasing_summary(pool, new_values)

        # Trigger a refresh of the property data
        db_refresh_trigger(db_refresh_trigger() + 1)
        db_trigger_func()
        shiny::removeModal()
      })

      # refresh -----------------------------------------------------------------
      shiny::observeEvent(
        list(
          input$refresh,
          db_refresh_trigger()
        ),
        {
          shiny::withProgress(
            message = "Refreshing Data...",
            detail = "Please wait...",
            value = 0,
            {
              shiny::incProgress(1 / 2, detail = "Refreshing Data...")
              survey_data$leasing_summary <- db_read_survey_leasing_summary(
                pool,
                property_id = selected_filters$property_id,
                competitor_id = selected_filters$competitor_id,
                leasing_week_id = selected_filters$leasing_week_id,
                survey_id = selected_filters$survey_id
              )
              shiny::incProgress(1 / 2, detail = "Data Refreshed")
            }
          )
          shiny::showNotification("Data Refreshed", type = "default")
        },
        ignoreInit = TRUE
      )

      # return ------------------------------------------------------------------
      return(
        list(
          leasing_summary_data = leasing_summary_data,
          inputs_data = inputs_data,
          changes = changes
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_leasing_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_leasing_summary_demo <- function(pool = NULL) {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Leasing Summary",
    window_title = "Demo: Survey Leasing Summary",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Leasing Summary",
      value = "survey_leasing_summary",
      icon = bsicons::bs_icon("house"),
      mod_survey_leasing_summary_ui("demo")
    )
  )

  server <- function(input, output, session) {
    if (is.null(pool)) pool <- db_connect()
    mod_survey_leasing_summary_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)
}
