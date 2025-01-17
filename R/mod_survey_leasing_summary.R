
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

  current_leasing_week <- get_leasing_week_start_date()
  min_leasing_week <- current_leasing_week - lubridate::years(1)
  max_leasing_week <- get_leasing_week_end_date()

  htmltools::tagList(
    bslib::page_fluid(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::dateInput(
            ns("leasing_week"),
            label = "Leasing Week",
            value = NULL,
            weekstart = 1,
            min = min_leasing_week,
            max = max_leasing_week
          )
        ),
        bslib::card(
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            htmltools::tags$h3(
              class = "m-0",
              "Leasing Summary"
            ),
            shiny::actionButton(
              ns("edit"),
              "Edit",
              icon = shiny::icon("edit"),
              class = "btn-sm btn-primary"
            )
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(6, 6, 6, 6),

              bslib::card(
                bslib::card_header(
                  "Occupancy Statistics"
                ),
                bslib::card_body(
                  bslib::value_box(
                    title = "Current Occupancy %",
                    value = shiny::textOutput(ns("current_occupancy")),
                    theme = "primary",
                    showcase = bsicons::bs_icon("people")
                  ),
                  bslib::value_box(
                    title = "Current Pre-Lease %",
                    value = shiny::textOutput(ns("current_pre_lease")),
                    theme = "primary",
                    showcase = bsicons::bs_icon("graph-up")
                  ),
                  bslib::value_box(
                    title = "Prior Year Occupancy %",
                    value = shiny::textOutput(ns("prior_occupancy")),
                    theme = "secondary",
                    showcase = bsicons::bs_icon("clock-history")
                  ),
                  bslib::value_box(
                    title = "Prior Year Pre-Lease %",
                    value = shiny::textOutput(ns("prior_pre_lease")),
                    theme = "secondary",
                    showcase = bsicons::bs_icon("clock-history")
                  )
                )
              ),

              bslib::card(
                bslib::card_header(
                  "Important Dates"
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
                  ),
                  htmltools::tags$p(
                    htmltools::tags$strong(
                      "Last Updated: ",
                      shiny::textOutput(ns("last_updated"), inline = TRUE)
                    )
                  )
                )
              ),

              bslib::card(
                bslib::card_header(
                  "Leasing Activity"
                ),
                bslib::card_body(
                  bslib::layout_columns(
                    col_widths = c(6, 6),
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
                    )
                  ),
                  htmltools::tags$p(
                    htmltools::tags$strong(
                      "Weekly Leases: "
                    ),
                    shiny::textOutput(ns("weekly_leases"), inline = TRUE)
                  ),
                  htmltools::tags$p(
                    htmltools::tags$strong(
                      "Weekly Traffic: "
                    ),
                    shiny::textOutput(ns("weekly_traffic"), inline = TRUE)
                  )
                )
              ),

              bslib::card(
                bslib::card_header(
                  "Incentives"
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
  selected_property_id = NULL
) {

  # validation of reactives
  if (!is.null(selected_property_id)) stopifnot(shiny::is.reactive(selected_property_id))

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_leasing_summary_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # handle selected property ID
      if (is.null(selected_property_id)) {
        property_id <- db_read_tbl(pool, "mkt.properties", collect = FALSE) |>
          dplyr::filter(.data$is_competitor == FALSE) |>
          dplyr::pull("property_id")
        selected_property_id <- shiny::reactive({ property_id })
      }

      db_refresh_trigger <- shiny::reactiveVal(0)

      iv <- leasing_summary_validator()

      selected_leasing_week <- shiny::reactive({
        shiny::req(input$leasing_week)
        input$leasing_week |> get_leasing_week_start_date()
      })

      leasing_summary_data <- shiny::reactive({
        shiny::req(pool, selected_property_id(), selected_leasing_week())
        db_read_mkt_leasing_summary(
          pool,
          property_id = selected_property_id(),
          leasing_week = selected_leasing_week()
        )
      }) |>
        shiny::bindEvent(
          selected_property_id(),
          selected_leasing_week(),
          db_refresh_trigger()
        )

      leasing_data_latest_leasing_week <- shiny::reactive({
        db_read_mkt_leasing_summary(
          pool,
          property_id = selected_property_id()
        ) |>
          dplyr::pull("leasing_week") |>
          max()
      })

      shiny::observeEvent(leasing_data_latest_leasing_week(), {
        if (leasing_data_latest_leasing_week() != selected_leasing_week()) {
          shiny::updateDateInput(
            session,
            "leasing_week",
            value = leasing_data_latest_leasing_week()
          )
          cli::cli_alert_warning(
            "Leasing week has been updated to match the selected property's data."
          )
          shiny::showNotification(
            "Leasing week has been updated to match the selected property's data.",
            duration = 5000,
            type = "warning"
          )
        }
      }, once = TRUE)

      inputs_data <- shiny::reactive({
        tibble::tibble(
          property_id = selected_property_id(),
          property_name = leasing_summary_data()$property_name,
          leasing_week = selected_leasing_week(),
          reporting_cycle = input$reporting_cycle,
          lease_launch_date = input$lease_launch_date,
          renewal_launch_date = input$renewal_launch_date,
          current_occupancy = input$current_occupancy,
          last_year_occupancy = input$prior_year_occupancy,
          current_pre_lease = input$current_pre_lease,
          last_year_pre_lease = input$last_year_pre_lease,
          total_renewals = input$total_renewals,
          total_new_leases = input$total_new_leases,
          traffic_weekly = input$weekly_traffic,
          current_incentive = input$current_incentive,
          incentive_amount = input$incentive_amount
        )
      })

      # output data
      output$last_updated <- shiny::renderText({
        leasing_summary_data()$updated_at |>
          format("%Y-%m-%d %I:%M %p")
      })

      output$current_occupancy <- shiny::renderText({
        leasing_summary_data()$current_occupancy |>
          scales::percent(accuracy = 0.01)
      })

      output$prior_occupancy <- shiny::renderText({
        leasing_summary_data()$last_year_occupancy |>
          scales::percent(accuracy = 0.01)
      })

      output$current_pre_lease <- shiny::renderText({
        leasing_summary_data()$current_pre_lease |>
          scales::percent(accuracy = 0.01)
      })

      output$prior_pre_lease <- shiny::renderText({
        leasing_summary_data()$last_year_pre_lease |>
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
        leasing_summary_data()$total_leases_weekly |>
          scales::comma()
      })

      output$weekly_traffic <- shiny::renderText({
        leasing_summary_data()$traffic_weekly |>
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

      # edit
      shiny::observeEvent(input$edit, {

        data <- leasing_summary_data()

        iv$initialize()
        iv$enable()

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Leasing Summary",
            size = "l",
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
                  value = data$lease_launch_date
                ),
                shiny::dateInput(
                  ns("renewal_launch_date"),
                  "Renewal Launch Date",
                  value = data$renewal_launch_date
                ),
                shiny::sliderInput(
                  ns("current_occupancy"),
                  "Current Occupancy %",
                  min = 0,
                  max = 1,
                  value = round(data$current_occupancy, 2),
                  step = 0.01,
                  ticks = TRUE,
                  post = "%"
                ),
                shiny::sliderInput(
                  ns("prior_year_occupancy"),
                  "Prior Year Occupancy %",
                  min = 0,
                  max = 1,
                  value = round(data$last_year_occupancy, 2),
                  step = 0.01,
                  ticks = TRUE,
                  post = "%"
                ),
                shiny::sliderInput(
                  ns("current_pre_lease"),
                  "Current Pre-Lease %",
                  min = 0,
                  max = 1,
                  value = round(data$current_pre_lease, 2),
                  step = 0.01,
                  ticks = TRUE,
                  post = "%"
                ),
                shiny::sliderInput(
                  ns("last_year_pre_lease"),
                  "Prior Year Pre-Lease %",
                  min = 0,
                  max = 1,
                  value = round(data$last_year_pre_lease, 2),
                  step = 0.01,
                  ticks = TRUE,
                  post = "%"
                )
              ),
              bslib::card(
                shiny::numericInput(
                  ns("total_renewals"),
                  "Total Renewals",
                  value = data$total_renewals,
                  min = 0,
                  step = 1
                ),
                shiny::numericInput(
                  ns("total_new_leases"),
                  "Total New Leases",
                  value = data$total_new_leases,
                  min = 0,
                  step = 1
                ),
                shiny::numericInput(
                  ns("weekly_leases"),
                  "Weekly Leases",
                  value = data$total_leases_weekly,
                  min = 0,
                  step = 1
                ),
                shiny::numericInput(
                  ns("weekly_traffic"),
                  "Weekly Traffic",
                  value = data$traffic_weekly,
                  min = 0,
                  step = 1
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
                  value = data$incentive_amount,
                  min = 0,
                  step = 1
                ) |> shinyjs::disabled(),
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
                ns("save_changes"),
                "Save Changes",
                class = "btn-primary"
              ) |>
                shinyjs::disabled(),
              shiny::modalButton("Cancel")
            ),
            easyClose = TRUE
          )
        )
      })

      shiny::observeEvent(input$current_incentive, {
        if (input$current_incentive != "None") {
          shinyjs::enable("incentive_amount")
        }
      })

      changes <- shiny::reactive({
        shiny::req(inputs_data())
        original_data <- leasing_summary_data()
        new_values <- inputs_data()
        changes <- list()
        for (field in names(new_values)) {
          if (!is.null(new_values[[field]]) && !isTRUE(all.equal(new_values[[field]], original_data[[field]]))) {
            # Format numeric values with 2 decimal places
            old_value <- if (is.numeric(original_data[[field]])) {
              round(original_data[[field]], 2)
            } else {
              original_data[[field]]
            }
            new_value <- if (is.numeric(new_values[[field]])) {
              round(new_values[[field]], 2)
            } else {
              new_values[[field]]
            }
            # Only add to changes if the rounded values are different
            if (!isTRUE(all.equal(old_value, new_value))) {
              changes[[field]] <- list(
                old = old_value,
                new = new_value
              )
            }
          }
        }
        changes
      })

      shiny::observe({
        shiny::req(changes())
        if (length(changes()) > 0) {
          shinyjs::enable("save_changes")
          shiny::updateDateInput(
            session,
            "data_last_updated",
            value = lubridate::now()
          )
        } else {
          shinyjs::disable("save_changes")
        }
      })

      output$changes_preview <- shiny::renderUI({
        shiny::req(changes())
        changes_data <- changes()
        if (length(changes_data) == 0) {
          return(
            htmltools::tags$p("No changes made")
          )
        }
        changes_ui <- lapply(names(changes_data), function(field) {
          htmltools::tags$div(
            htmltools::tags$p(
              htmltools::tags$strong(
                paste0(tools::toTitleCase(
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

      shiny::observeEvent(input$save_changes, {
        shiny::req(pool, inputs_data(), selected_property_id(), selected_leasing_week())

        new_values <- inputs_data()

        db_update_mkt_leasing_summary(
          pool,
          property_id = selected_property_id(),
          leasing_week = selected_leasing_week(),
          new_values = new_values
        )

        db_refresh_trigger(db_refresh_trigger() + 1)

        shiny::removeModal()
      })

      return(
        list(
          leasing_summary_data = leasing_summary_data,
          inputs_data = inputs_data
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
