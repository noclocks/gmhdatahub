#  ------------------------------------------------------------------------
#
# Title : Pre Lease Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Pre-Lease Shiny Module
#'
#' @name mod_pre_lease
#'
#' @description
#' A Shiny Module for the GMH Communities Pre-Lease Summary & Details reports
#' data visualization and review.
#'
#' The following functions are implemented:
#'
#' - `mod_pre_lease_ui()`: User interface (UI) definition
#' - `mod_pre_lease_server()`: Server logic
#' - `mod_pre_lease_demo()`: Demo application for the module
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#' @param selected_property Selected property ID.
#' @param report_date Report date.
#'
#' @returns
#' - `mod_pre_lease_ui()`: UI HTML Output.
#' - `mod_pre_lease_server()`: List of reactive values.
#' - `mod_pre_lease_demo()`: `NULL`, used for the side-effect of a demo app.
#'
#' @examplesIf interactive()
#' mod_pre_lease_demo()
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid layout_columns value_box card card_header card_body card_footer navset_card_underline nav_panel
#' @importFrom htmltools tags
#' @importFrom plotly plotlyOutput
#' @importFrom reactable reactableOutput
#' @importFrom shiny NS textOutput downloadButton
mod_pre_lease_ui <- function(id) {

  ns <- shiny::NS(id)

  # get the default choices/values
  # report_date_choices <- db_read_gmh_pre
  default_report_date <- Sys.Date() |> lubridate::ymd()
  default_leasing_date <- get_entrata_custom_pre_lease_date(default_report_date)
  default_partners <- get_default_app_choices("partners")
  default_properties <- get_default_app_choices("properties")

  htmltools::tagList(
    htmltools::tags$head(
      shinyjs::useShinyjs()
    ),
    bslib::page_fluid(
      # value boxes -------------------------------------------------------------
      htmltools::div(
        id = ns("value_boxes"),
        class = "mb-4",
        bslib::layout_column_wrap(
          width = 1/4,
          gap = "1rem",
          bslib::value_box(
            title = "Average Occupancy %",
            value = shiny::textOutput(ns("val_avg_occupancy"), inline = TRUE),
            showcase = bsicons::bs_icon("percent"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "New Leases",
            subtitle = "Total new leases signed this period",
            value = shiny::textOutput(ns("val_new_leases"), inline = TRUE),
            showcase = bsicons::bs_icon("file-earmark-plus"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Renewals",
            value = shiny::textOutput(ns("val_new_renewals"), inline = TRUE),
            showcase = bsicons::bs_icon("arrow-repeat"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Year-Over-Year Change",
            value = shiny::textOutput(ns("val_yoy_pct_change"), inline = TRUE),
            showcase = bsicons::bs_icon("graph-up-arrow"),
            theme = "primary"
          )
        )
      ) |>
        shinyjs::hidden(),
      # nav card ----------------------------------------------------------------
      bslib::navset_card_underline(
        id = ns("nav"),
        sidebar = bslib::sidebar(
          id = ns("sidebar"),
          title = htmltools::tags$p("Controls"),
          open = FALSE,
          bslib::accordion(
            id = ns("accordion"),
            open = FALSE,
            bslib::accordion_panel(
              title = "Filters",
              value = ns("filters"),
              icon = bsicons::bs_icon("funnel"),
              shiny::selectInput(
                ns("partners"),
                label = "Investment Partners",
                multiple = TRUE,
                choices = get_default_app_choices("partners") |> names()
              ) |>
                with_tooltip(
                  "Select Investment Partners associated with certain properties."
                ),
              shiny::selectInput(
                ns("properties"),
                label = "Properties",
                multiple = TRUE,
                choices = get_default_app_choices("properties") |> names()
              ) |>
                with_tooltip(
                  "Select the Properties to include in the display."
                ),
              shiny::dateInput(
                ns("report_date"),
                label = "Report Date",
                value = Sys.Date()
              ) |>
                with_tooltip(
                  "Select a report date to view historical data pulled from the Entrata API."
                ),
              shiny::dateInput(
                ns("leasing_date"),
                label = "Pre-Lease Date",
                value = get_entrata_custom_pre_lease_date()
              ) |>
                shinyjs::disabled() |>
                with_tooltip(
                  "This is the date used when performing the API request to Entrata API."
                )
            ),
            bslib::accordion_panel(
              title = "Metrics",
              value = ns("metrics"),
              icon = bsicons::bs_icon("graph-up-arrow"),
              shiny::selectInput(
                ns("metric"),
                "Metric",
                choices = c("Leases" = "leases", "Renewals" = "renewals", "Pre-Lease %" = "prelease"),
                selected = "leases"
              ),
              shiny::sliderInput(
                ns("occupancy_target"),
                "Occupancy Target %",
                min = 0,
                max = 1,
                value = 0.95,
                step = 0.05,
                ticks = TRUE,
                post = "%"
              ),
              shiny::radioButtons(
                ns("group_by"),
                "Group By",
                choices = c("Property" = "property", "Investment Partner" = "partner"),
                selected = "property"
              )
            ),
            bslib::accordion_panel(
              title = "Options",
              value = ns("options"),
              icon = bsicons::bs_icon("gear"),
              bslib::input_switch(
                id = ns("toggle_val_boxes"),
                label = "Display KPI Metrics?",
                value = FALSE
              )
            )
          )
        ),
        # summary -----------------------------------------------------------------
        bslib::nav_panel(
          title = "Summary",
          icon = bsicons::bs_icon("clipboard-data"),
          value = ns("summary"),
          # summary table -----------------------------------------------------------
          bslib::card(
            full_screen = TRUE,
            bslib::card_body(
              reactable::reactableOutput(ns("summary_table")) |> with_loader()
            ),
            bslib::card_footer(
              class = "text-muted",
              "Last updated: ",
              shiny::textOutput(ns("last_updated"), inline = TRUE)
            )
          )
        ),
        bslib::nav_panel(
          title = "Details",
          icon = bsicons::bs_icon("info-circle"),
          value = ns("details"),
          bslib::card(
            entrata_table_ui(ns("details_table"))
          )
        ),
        bslib::nav_panel(
          title = "Charts",
          icon = bsicons::bs_icon("bar-chart"),
          value = "charts",
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("current_vs_prior_chart"),
                height = "400px"
              )
            ),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("occupancy_chart"),
                height = "400px"
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("velocity_chart"),
                height = "400px"
              )
            ),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("pre_lease_rates_chart"),
                height = "400px"
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("partner_distribution_chart"),
                height = "400px"
              )
            ),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("portfolio_summary_chart"),
                height = "400px"
              )
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("weekly_activity_chart"),
                height = "400px"
              )
            ),
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("weekly_leasing_breakdown_chart"),
                height = "400px"
              )
            )
          ),
          bslib::layout_columns(
            bslib::card(
              full_screen = TRUE,
              apexcharter::apexchartOutput(
                ns("yoy_variance_chart"),
                height = "400px"
              )
            )
          )
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          shiny::downloadButton(
            ns("download"),
            "Export to Excel",
            class = "btn-sm btn-success",
            icon = shiny::icon("file-excel")
          )
        ),
        bslib::nav_item(
          shiny::actionButton(
            ns("refresh"),
            "Refresh Data",
            icon = shiny::icon("recycle"),
            class = "btn-sm btn-primary"
          )
        ),
        bslib::nav_item(
          shiny::actionButton(
            ns("entrata"),
            "Entrata Settings",
            icon = shiny::icon("gear"),
            class = "btn-sm btn-primary"
          )
        ),
        bslib::nav_item(
          shiny::actionButton(
            inputId = ns("help"),
            label = "Help",
            icon = shiny::icon("question-circle"),
            class = "btn-sm btn-primary",
            style = "margin-right: 10px;"
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_pre_lease_server <- function(
    id,
    pool = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      Sys.setenv(CLI_NO_COLOR = "TRUE")

      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_server()")

      # database
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      selected_row <- shiny::reactiveVal(NULL)
      db_trigger <- shiny::reactiveVal(0)

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("model_beds", rule = shinyvalidate::sv_gte(0))
      iv$add_rule(
        "investment_partner",
        rule = shinyvalidate::sv_in_set(
          db_read_tbl(pool, "gmh.partners") |>
            dplyr::pull("partner_name") |>
            unique()
        )
      )

      # initial data
      pre_lease_summary_data <- shiny::reactive({
        shiny::req(db_trigger())
        db_read_tbl(pool, "gmh.pre_lease_global_summary")
      })

      shiny::observe({
        report_date <- pre_lease_summary_data() |>
          dplyr::pull("report_date") |>
          lubridate::ymd() |>
          max(na.rm = TRUE)
        partner_choices <- pre_lease_summary_data() |>
          dplyr::pull("investment_partner") |>
          unique()
        property_choices <- pre_lease_summary_data() |>
          dplyr::pull("property_name") |>
          unique()
        shiny::updateDateInput(
          session,
          "report_date",
          value = report_date
        )
        shiny::updateSelectInput(
          session,
          "partners",
          choices = partner_choices,
          selected = partner_choices
        )
        shiny::updateSelectInput(
          session,
          "properties",
          choices = property_choices,
          selected = property_choices
        )
      })

      tbl_data <- shiny::reactive({
        shiny::req(pre_lease_summary_data(), input$partners, input$properties)
        pre_lease_summary_data() |>
          dplyr::filter(
            .data$investment_partner %in% input$partners,
            .data$property_name %in% input$properties
          )
      })

      # summary table
      output$summary_table <- reactable::renderReactable({
        shiny::req(tbl_data())
        tbl_pre_lease_summary(tbl_data(), ns = ns)
      })

      shiny::observeEvent(input$edit_row, {
        row <- input$edit_row
        selected_row(row)
        row_data <- pre_lease_summary_data() |>
          dplyr::filter(dplyr::row_number() == row)
        selected_property <- row_data |> dplyr::pull("property_name")
        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Data",
            size = "m",
            easyClose = TRUE,
            htmltools::tagList(
              htmltools::tags$p(
                "Editing data for: ",
                htmltools::strong(selected_property)
              ),
              shiny::numericInput(
                ns("model_beds"),
                label = "Model Beds",
                value = row_data |> dplyr::pull("model_beds"),
                min = 0L
              ),
              shiny::selectInput(
                ns("investment_partner"),
                label = "Investment Partner",
                choices = get_default_app_choices("partners"),
                selected = row_data |> dplyr::pull("investment_partner")
              )
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$save, {

        iv$enable()

        shiny::req(selected_row(), input$model_beds, input$investment_partner)

        new_value_model_beds <- input$model_beds
        new_value_investment_partner <- input$investment_partner

        shiny::req(iv$is_valid())

        conn <- pool::poolCheckout(pool = pool)

        tryCatch({

          prop_id <- pre_lease_summary_data() |>
            dplyr::filter(dplyr::row_number() == selected_row()) |>
            dplyr::pull("property_id") |>
            as.integer()

          model_beds_dat <- tibble::tibble(
            property_id = prop_id,
            model_bed_count = new_value_model_beds,
            notes = "Updated via GMH DataHub"
          )

          dbx::dbxUpdate(
            conn,
            table = DBI::SQL("gmh.model_beds"),
            records = model_beds_dat,
            where_cols = c("property_id"),
            transaction = TRUE
          )

        partner_id <- db_read_tbl(pool, "gmh.partners") |>
          dplyr::filter(dplyr::pull("partner_name") == new_value_investment_partner) |>
          dplyr::pull("partner_id") |>
          as.integer()

        dbx::dbxExecute(
          conn,
          statement = "UPDATE 'gmh'.'properties' SET partner_id = ? WHERE property_id = ?",
          params = list(partner_id, prop_id)
        )

        cli::cli_alert_info("Model beds/Investment Partner updated successfully!")
        shiny::showNotification("Database updated successfully!", type = "message")
        db_trigger(db_trigger() + 1)

        }, error = function(e) {
          cli::cli_alert_danger("Error updating database: {.error e}")
          shiny::showNotification("Error updating database!", type = "error")
        }, finally = {
          shiny::removeModal()
          pool::poolReturn(conn)
          iv$disable()
        })

      })

      # refresh
      shiny::observeEvent(input$refresh, {

        shiny::showModal(
          shiny::modalDialog(
            title = "Refresh Data",
            size = "m",
            easyClose = TRUE,
            htmltools::tagList(
              htmltools::tags$p(
                "Would you like to refresh the data from the Entrata API or just from the database?"
              ),
              shiny::radioButtons(
                ns("refresh_type"),
                label = "Refresh Type",
                choices = c(
                  "Entrata API" = "entrata",
                  "Database" = "database"
                ),
                selected = "database"
              )
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("refresh_confirm"),
                "Refresh",
                class = "btn-primary"
              )
            )
          )
        )
      })

      # confirm refresh
      shiny::observeEvent(input$refresh_confirm, {
        shiny::req(input$refresh_type)
        if (input$refresh_type == "entrata") {
          # emulate clicking the entrata button
          js <- glue::glue("$('#{ns('entrata')}').click();")
          shinyjs::runjs(js)
        } else {
          db_trigger(db_trigger() + 1)
          shiny::showNotification("Data refreshed successfully!", type = "message")
        }
        shiny::removeModal()
      })

      # toggle metrics (value boxes)
      shiny::observeEvent(input$toggle_val_boxes, {
        shinyjs::toggle(
          "value_boxes",
          anim = TRUE,
          condition = input$toggle_val_boxes
        )
      })

      # entrata settings
      shiny::observeEvent(input$entrata, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Configure Entrata API",
            size = "l",
            easyClose = TRUE,
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                shiny::dateInput(
                  ns("period_date"),
                  label = "Period Date",
                  value = get_entrata_custom_pre_lease_date()
                ),
                shiny::radioButtons(
                  ns("summarize_by"),
                  label = "Summarize By",
                  choices = c(
                    "Unit Type" = "unit_type",
                    "Do Not Summarize" = "do_not_summarize",
                    "Property" = "property",
                    "Floorplan Name" = "floorplan_name"
                  ),
                  selected = "unit_type"
                ),
                shiny::radioButtons(
                  ns("group_by"),
                  label = "Group By",
                  choices = c(
                    "Unit Type" = "unit_type",
                    "Property" = "property",
                    "Floorplan Name" = "floorplan_name",
                    "Lease Term" = "lease_term",
                    "Do Not Group" = "do_not_group"
                  ),
                  selected = "unit_type"
                ),
                shiny::radioButtons(
                  ns("consolidate_by"),
                  label = "Consolidate By",
                  choices = c(
                    "No Consolidation" = "no_consolidation",
                    "Consolidate All Properties" = "consolidate_all_properties",
                    "Consolidate by Property Groups" = "consolidate_by_property_groups"
                  ),
                  selected = "no_consolidation"
                )
              ),
              bslib::card(
                bslib::input_switch(
                  ns("space_options"),
                  label = "Space Options?",
                  value = FALSE
                ),
                bslib::input_switch(
                  ns("charge_code_detail"),
                  label = "Charge Code Detail?",
                  value = TRUE
                ),
                bslib::input_switch(
                  ns("additional_units_show"),
                  label = "Additional Units Shown?",
                  value = FALSE
                ),
                bslib::input_switch(
                  ns("combine_unit_spaces_with_same_lease"),
                  label = "Combine Unit Spaces with Same Lease?",
                  value = FALSE
                ),
                bslib::input_switch(
                  ns("arrange_by_property"),
                  label = "Arrange by Property?",
                  value = FALSE
                ),
                bslib::input_switch(
                  ns("yoy"),
                  label = "Year-Over-Year?",
                  value = TRUE
                ),
                shiny::checkboxGroupInput(
                  ns("subtotals"),
                  label = "Subtotals",
                  choices = c(
                    "Summary" = "summary",
                    "Details" = "details"
                  ),
                  selected = c("summary", "details")
                ),
                shiny::textInput(
                  ns("request_id"),
                  label = "Request ID",
                  value = "",
                  placeholder = "(Optional) Enter Request ID:"
                )
              )
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              bslib::input_task_button(
                id = ns("entrata_refresh"),
                label = "Refresh Entrata Data",
                icon = bsicons::bs_icon("arrow-clockwise"),
                class = "btn-primary"
              )
            )
          )
        )
      })

      # confirm entrata
      shiny::observeEvent(input$entrata_refresh, {

        cli::cli_alert_info("Refreshing Entrata API Pre-Lease Data...")

        steps <- 10
        incr <- 1 / steps

        shiny::withProgress(
          message = "Refreshing Entrata API Data...",
          detail = "This may take a few minutes...",
          value = 0,
          {
            shiny::incProgress(incr, detail = "Pre-Lease Report by Property")
            pre_lease_by_property <- entrata_pre_lease_report(summarize_by = "property")

            shiny::incProgress(incr, detail = "Pre-Lease Report by Unit")
            pre_lease_by_unit <- entrata_pre_lease_report(summarize_by = "unit_type")

            pre_lease_summary_by_property <- pre_lease_by_property$summary
            pre_lease_summary_by_unit <- pre_lease_by_unit$summary
            pre_lease_details <- pre_lease_by_property$details
            pre_lease_params_by_property <- pre_lease_by_property$parameters
            pre_lease_params_by_unit <- pre_lease_by_unit$parameters

            shiny::incProgress(incr, detail = "Weekly Leasing Data")
            weekly_leasing_data <- entrata_lease_execution_report()

            tryCatch({
              pool::poolWithTransaction(pool, {

                conn <- pool::poolCheckout(pool)

                shiny::incProgress(
                  incr,
                  message = "Updating Database",
                  detail = "Weekly Leasing Data"
                )

                dbx::dbxUpsert(
                  conn,
                  table = DBI::SQL("entrata.pre_lease_weekly"),
                  records = weekly_leasing_data |>
                    dplyr::select(-property_name, -weekly_total),
                  where_cols = c("report_date", "property_id"),
                  skip_existing = FALSE
                )

                shiny::incProgress(incr, detail = "Pre-Lease Report by Property")

                dbx::dbxUpsert(
                  conn,
                  table = DBI::SQL("entrata.pre_lease_summary_by_property"),
                  records = pre_lease_summary_by_property,
                  where_cols = c("report_date", "property_id"),
                  skip_existing = FALSE
                )

                shiny::incProgress(incr, detail = "Pre-Lease Report by Unit")

                dbx::dbxUpsert(
                  conn,
                  table = DBI::SQL("entrata.pre_lease_summary_by_unit"),
                  records = pre_lease_summary_by_unit,
                  where_cols = c("report_date", "property_id"),
                  skip_existing = FALSE
                )

                shiny::incProgress(incr, detail = "Pre-Lease Report Details")

                browser()

                pool::dbAppendTable(
                  pool,
                  DBI::SQL("entrata.pre_lease_details"),
                  value = pre_lease_details,
                  append = TRUE
                )

                cli::cli_alert_info("Data refreshed successfully!")
                shiny::incProgress(incr, "Refreshing Views and Pulling New Data from Database")
                shiny::setProgress(100, detail = "Data refreshed successfully!")
              })
            }, error = function(e) {
              cli::cli_alert_danger("Error updating database: {.error {e}}")
              shiny::showNotification("Error updating database!", type = "error")
            }, finally = {
              pool::poolReturn(conn)
              db_trigger(db_trigger() + 1)
              shiny::removeModal()
            })
          }
        )
      })

      shiny::observeEvent(input$help, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Pre-Lease Summary Help",
            size = "l",
            easyClose = TRUE,
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("view_docs"),
                "View Technical Documentation",
                icon = shiny::icon("book"),
                class = "btn-primary"
              ),
              shiny::modalButton("Close")
            ),
            bslib::card(
              class = "mb-4",
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::div(
                  bsicons::bs_icon("info-circle", class = "me-2"),
                  "About"
                )
              ),
              bslib::card_body(
                htmltools::tags$p(
                  bsicons::bs_icon("clipboard-data", class = "me-2"),
                  "This pre-lease summary dashboard provides a comprehensive overview of property leasing performance across multiple locations.
              The data includes detailed metrics on current occupancy, lease types, and velocity targets.",
                  class = "mb-3"
                ),
                htmltools::tags$ul(
                  class = "list-unstyled",
                  htmltools::tags$li(bsicons::bs_icon("house-check", class = "me-2"), "Current Occupancy: Shows the current percentage of occupied beds", class = "mb-2"),
                  htmltools::tags$li(bsicons::bs_icon("file-earmark-text", class = "me-2"), "Lease Types: Breakdown between new leases and renewals", class = "mb-2"),
                  htmltools::tags$li(bsicons::bs_icon("graph-up", class = "me-2"), "YOY Performance: Year-over-year comparison of leasing metrics", class = "mb-2"),
                  htmltools::tags$li(bsicons::bs_icon("speedometer", class = "me-2"), "Velocity Targets: Progress towards 90%, 95%, and 100% occupancy goals")
                )
              )
            )
          )
        )
      })

      shiny::observeEvent(input$view_docs, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Technical Documentation",
            size = "l",
            easyClose = TRUE,
            footer = htmltools::tagList(
              shiny::modalButton("Close")
            ),
            bslib::card(
              class = "mb-4",
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::div(
                  bsicons::bs_icon("book", class = "me-2"),
                  "Technical Documentation"
                )
              ),
              bslib::card_body(
                htmltools::tags$p(
                  bsicons::bs_icon("info-circle", class = "me-2"),
                  "This section provides detailed technical documentation for the pre-lease summary dashboard.
              The documentation includes information on the data sources, metrics, and calculations used in the dashboard."
                ),
              )
            ),
            htmltools::tags$hr(),
            bslib::card(
              class = "mb-4",
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::div(
                  bsicons::bs_icon("database", class = "me-2"),
                  "Data Pipeline"
                )
              ),
              bslib::card_body(
                htmltools::tags$iframe(
                  src = "www/content/pre_lease/pre_lease_summary_tbl.html",
                  width = "100%",
                  height = "600px",
                  scrolling = "auto",
                  frameborder = 0
                )
              )
            )
          )
        )
      })

      # value boxes
      output$val_avg_occupancy <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("current_occupancy") |>
          mean(na.rm = TRUE) |>
          scales::percent(accuracy = 0.1)
      })

      output$val_new_leases <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("current_total_new") |>
          sum(na.rm = TRUE) |>
          scales::comma()
      })

      output$val_new_renewals <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("current_total_renewals") |>
          sum(na.rm = TRUE) |>
          scales::comma()
      })

      output$val_yoy_pct_change <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("yoy_variance_percent") |>
          mean(na.rm = TRUE) |>
          scales::percent(accuracy = 0.1)
      })

      # last updated
      output$last_updated <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("report_date") |>
          lubridate::ymd() |>
          max(na.rm = TRUE) |>
          format("%B %d, %Y")
      })

      # download
      output$download <- shiny::downloadHandler(
        filename = function() {
          get_xl_report_file_name(report_name = "GMH Pre-Lease Summary",
                                  report_date = Sys.Date())
        },
        content = function(file) {
          shiny::withProgress(
            message = "Generating Excel Pre-Lease Report Export...",
            value = 0,
            {
              shiny::setProgress(10, message = "Collecting Data and Information...")

              report_date <- pre_lease_summary_data() |>
                dplyr::pull("report_date") |>
                max(na.rm = TRUE)

              leasing_week <- get_leasing_week_number(report_date)

              weeks_left_to_lease <- get_weeks_left_to_lease(report_date)

              leasing_season_ending <- get_pre_lease_season_start_date(report_date)

              xl_data <- pre_lease_summary_data() |>
                dplyr::select(
                  property_name,
                  investment_partner,
                  total_beds,
                  model_beds,
                  current_occupied,
                  current_occupancy,
                  current_total_new,
                  current_total_renewals,
                  current_total_leases,
                  current_preleased_percent,
                  prior_total_new,
                  prior_total_renewals,
                  prior_total_leases,
                  prior_preleased_percent,
                  yoy_variance_count,
                  yoy_variance_percent,
                  weekly_new,
                  weekly_renewal,
                  weekly_total,
                  weekly_percent_gained,
                  beds_left,
                  vel_90,
                  vel_95,
                  vel_100
                ) |>
                dplyr::mutate(
                  dplyr::across(
                    tidyselect::where(is.numeric),
                    dplyr::coalesce,
                    0
                  )
                )

              shiny::setProgress(20, message = "Formatting Data for Excel...")

              comma_cols <- c(
                "total_beds",
                "model_beds",
                "current_occupied",
                "current_total_new",
                "current_total_renewals",
                "current_total_leases",
                "prior_total_new",
                "prior_total_renewals",
                "prior_total_leases",
                "yoy_variance_count",
                "weekly_new",
                "weekly_renewal",
                "weekly_total",
                "beds_left",
                "vel_90",
                "vel_95",
                "vel_100"
              )

              pct_cols <- c(
                "current_occupancy",
                "current_preleased_percent",
                "prior_preleased_percent",
                "yoy_variance_percent",
                "weekly_percent_gained"
              )

              for (col in comma_cols) {
                class(xl_data[[col]]) <- c("comma", class(xl_data[[col]]))
              }

              for (col in pct_cols) {
                class(xl_data[[col]]) <- c("percentage", class(xl_data[[col]]))
              }

              shiny::setProgress(30, message = "Initializing Excel Template...")

              wb_template <- openxlsx2::wb_load(
                pkg_sys("templates/excel/pre-lease-summary-template.xlsx"),
                sheet = "Pre-Lease Summary"
              )

              shiny::setProgress(40, message = "Writing Report Date to Excel...")

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = report_date,
                start_row = 2,
                start_col = 2,
                col_names = FALSE
              )

              setProgress(50, message = "Writing Leasing Season Ending Date to Excel...")

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = leasing_season_ending,
                start_row = 2,
                start_col = 24,
                col_names = FALSE
              )

              setProgress(60, message = "Writing Leasing Week to Excel...")

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = leasing_week,
                start_row = 3,
                start_col = 2,
                col_names = FALSE
              )

              setProgress(70, message = "Writing Weeks Left to Lease to Excel...")

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = weeks_left_to_lease,
                start_row = 3,
                start_col = 24,
                col_names = FALSE
              )

              setProgress(80, message = "Writing Data to Excel...")

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = xl_data |>
                  dplyr::select(property_name, investment_partner),
                start_row = 6,
                start_col = 1,
                col_names = FALSE
              )

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = xl_data |>
                  dplyr::select(
                    total_beds:current_total_renewals
                  ),
                start_row = 6,
                start_col = 3,
                col_names = FALSE
              )

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = xl_data |>
                  dplyr::select(
                    prior_total_new:prior_total_renewals
                  ),
                start_row = 6,
                start_col = 11,
                col_names = FALSE
              )

              wb_template$add_data(
                sheet = "Pre-Lease Summary",
                x = xl_data |>
                  dplyr::select(
                    weekly_new:weekly_renewal
                  ),
                start_row = 6,
                start_col = 17,
                col_names = FALSE
              )

              setProgress(90, message = "Finalizing Excel File...")

              wb_template$save(file, overwrite = TRUE)

              setProgress(100, message = "Excel File Generated Successfully!")

            }
          )

        }
      )

      # current vs prior
      output$current_vs_prior_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_current_vs_prior(data = pre_lease_summary_data(), metric = input$metric)
      })

      # occupancy
      output$occupancy_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data(), input$occupancy_target, input$group_by)
        chart_occupancy(
          data = pre_lease_summary_data(),
          target = input$occupancy_target,
          by = input$group_by
        )
      })

      # velocity
      output$velocity_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_velocity_comparison(data = pre_lease_summary_data())
      })

      # pre-lease rates
      output$pre_lease_rates_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_pre_lease_rates(data = pre_lease_summary_data())
      })

      # partner distribution
      output$partner_distribution_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_partner_distribution(data = pre_lease_summary_data())
      })

      # portfolio summary
      output$portfolio_summary_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_portfolio_summary(data = pre_lease_summary_data())
      })

      # weekly activity
      output$weekly_activity_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_weekly_activity(data = pre_lease_summary_data())
      })

      # weekly leasing breakdown
      output$weekly_leasing_breakdown_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data())
        chart_weekly_leasing_breakdown(data = pre_lease_summary_data())
      })

      # yoy variance
      output$yoy_variance_chart <- apexcharter::renderApexchart({
        shiny::req(pre_lease_summary_data(), input$group_by)
        chart_yoy_variance(data = pre_lease_summary_data(), by = input$group_by)
      })

      entrata_pre_lease_summary_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_summary") |>
          dplyr::filter(
            .data$report_date == max(.data$report_date, na.rm = TRUE),
            .data$property_name %in% input$properties
          )
      })

      entrata_pre_lease_details_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_details") |>
          dplyr::filter(
            .data$report_date == max(.data$report_date, na.rm = TRUE),
            .data$property_name %in% input$properties
          )
      })

      entrata_pre_lease_details_by_property_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_details_by_property") |>
          dplyr::filter(
            .data$report_date == max(.data$report_date, na.rm = TRUE),
            .data$property_name %in% input$properties
          )
      })

      # details table
      entrata_table_server(
        id = "details_table",
        summary_data = entrata_pre_lease_details_by_property_data,
        details_data = entrata_pre_lease_details_data
      )

      return(
        list(
          pre_lease_summary_data = pre_lease_summary_data,
          entrata_pre_lease_summary_data = entrata_pre_lease_summary_data,
          entrata_pre_lease_details_data = entrata_pre_lease_details_data,
          entrata_pre_lease_details_by_property_data = entrata_pre_lease_details_by_property_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_pre_lease
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_pre_lease_demo <- function() {
  pkgload::load_all()
  shiny::addResourcePath("www", pkg_sys("www"))

  ui <- bslib::page_navbar(
    title = "Demo: Pre Lease",
    window_title = "Demo: Pre Lease",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Pre Lease",
      value = "pre_lease",
      icon = bsicons::bs_icon("house"),
      mod_pre_lease_ui("demo")
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    mod_pre_lease_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)
}


entrata_table_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    reactable::reactableOutput(ns("table")) |> with_loader()
  )

}

entrata_table_server <- function(id, summary_data, details_data) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: entrata_table_server()")

      output$table <- reactable::renderReactable({
        tbl_entrata_pre_lease(summary_data(), details_data())
      })

      return(list(
        # summary_data = summary_data,
        # details_data = details_data
      ))
    })

}



