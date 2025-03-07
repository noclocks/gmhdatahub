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
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shinyscreenshot screenshotButton
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
          bslib::accordion(
            open = TRUE,
            id = ns("accordion"),
            bslib::accordion_panel(
              title = "Property Level Summary",
              value = ns("property_level"),
              icon = bsicons::bs_icon("building-fill"),
              bslib::card(
                full_screen = TRUE,
                reactable::reactableOutput(ns("property_table")) |> with_loader()
              )
            ),
            bslib::accordion_panel(
              title = "Unit Level Summary",
              value = ns("unit_level"),
              icon = bsicons::bs_icon("house-door"),
              bslib::card(
                full_screen = TRUE,
                shiny::uiOutput(ns("selected_property_info")),
                shiny::conditionalPanel(
                  condition = paste0("!output['", ns("has_property_selection"), "']"),
                  htmltools::tags$div(
                    class = "alert alert-info m-3",
                    htmltools::tags$div(
                      class = "d-flex align-items-center gap-2",
                      bsicons::bs_icon("arrow-up-circle"),
                      "Select a property from the table above to view its unit details."
                    )
                  )
                ),
                shiny::conditionalPanel(
                  condition = paste0("output['", ns("has_property_selection"), "'] == true"),
                  reactable::reactableOutput(ns("unit_table")) |> with_loader()
                )
              )
            ),
            bslib::accordion_panel(
              title = "Unit Level Details",
              value = ns("details_level"),
              icon = bsicons::bs_icon("info-circle"),
              bslib::card(
                full_screen = TRUE,
                shiny::uiOutput(ns("selected_unit_info")),
                shiny::conditionalPanel(
                  condition = paste0("!output['", ns("has_unit_selection"), "']"),
                  htmltools::tags$div(
                    class = "alert alert-info m-3",
                    "Please select a unit from the table above to view lease details."
                  )
                ),
                reactable::reactableOutput(ns("detail_table")) |> with_loader()
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Charts",
          icon = bsicons::bs_icon("bar-chart"),
          value = "charts",
          htmltools::tags$div(
            id = ns("charts-container"),
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
          )
        ),
        bslib::nav_spacer(),
        bslib::nav_item(
          shiny::actionButton(
            ns("excel"),
            "Export to Excel",
            class = "btn-sm btn-success",
            icon = shiny::icon("file-excel")
          )
        ),
        bslib::nav_item(
          shinyscreenshot::screenshotButton(
            id = ns("charts-container"),
            filename = paste0(
              format(Sys.Date(), "%Y-%m-%d"),
              "_GMH_Pre_Lease_Charts"
            ),
            ns = ns,
            label = "Export Charts",
            class = "btn-sm btn-warning"
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

      Sys.setenv(CLI_NO_COLOR = TRUE)

      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_server()")

      # database
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      selected_row <- shiny::reactiveVal(NULL)
      db_trigger <- shiny::reactiveVal(0)

      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("model_beds", rule = shinyvalidate::sv_gte(0))
      # iv$add_rule(
      #   "investment_partner",
      #   rule = shinyvalidate::sv_in_set(
      #     db_read_tbl(pool, "gmh.partners") |>
      #       dplyr::pull("partner_name") |>
      #       unique()
      #   )
      # )

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
                choices = get_default_app_choices("partners") |> names(),
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

        new_value_model_beds <- as.integer(input$model_beds)
        new_value_investment_partner <- input$investment_partner

        shiny::req(iv$is_valid())

        tryCatch({

          prop_id <- pre_lease_summary_data() |>
            dplyr::filter(dplyr::row_number() == selected_row()) |>
            dplyr::pull("property_id") |>
            as.integer()

          orig_model_beds <- pre_lease_summary_data() |>
            dplyr::filter(dplyr::row_number() == selected_row()) |>
            dplyr::pull("model_beds") |>
            as.integer()

          if (new_value_model_beds != orig_model_beds) {
            db_update_gmh_model_beds(pool, prop_id, new_value_model_beds)
          } else {
            cli::cli_alert_info("Model beds did not change.")
          }

          partner_id <- db_read_tbl(pool, "gmh.partners") |>
            dplyr::filter(.data$partner_name == new_value_investment_partner) |>
            dplyr::pull("partner_id") |>
            as.integer()

          orig_partner_id <- pre_lease_summary_data() |>
            dplyr::filter(dplyr::row_number() == selected_row()) |>
            dplyr::pull("investment_partner") |>
            get_partner_id_by_name() |>
            as.integer()

          if (partner_id != orig_partner_id) {
            db_update_gmh_property_partner(pool, prop_id, partner_id)
          } else {
            cli::cli_alert_info("Investment partner did not change.")
          }

          shiny::showNotification("Database updated successfully!", type = "message")
          db_trigger(db_trigger() + 1)

        }, error = function(e) {
          cli::cli_alert_danger("Error updating database: {.error {e}}")
          shiny::showNotification("Error updating database!", type = "error")
        }, finally = {
          shiny::removeModal()
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
            bslib::card(
              height = "auto",
              bslib::card_header(
                class = "bg-warning",
                htmltools::div(
                  bsicons::bs_icon("exclamation-triangle", class = "me-2"),
                  "Warning"
                )
              ),
              bslib::card_body(
                htmltools::tags$p(
                  "Refreshing the Entrata API data may take up to 5 minutes. Please be patient."
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

            pool::poolWithTransaction(pool, function(conn) {

              shiny::incProgress(
                incr,
                message = "Updating Database",
                detail = "Weekly Leasing Data"
              )

              tryCatch({
                dbx::dbxUpsert(
                  conn,
                  table = DBI::SQL("entrata.pre_lease_weekly"),
                  records = weekly_leasing_data |>
                    dplyr::select(-property_name, -weekly_total),
                  where_cols = c("report_date", "property_id"),
                  skip_existing = FALSE
                )
                cli::cli_alert_success("Weekly Leasing Data updated successfully!")
              }, error = function(e) {
                cli::cli_alert_danger("Error updating Weekly Leasing Data: {.error {e}}")
                shiny::showNotification("Error updating Weekly Leasing Data!", type = "error")
              })

              shiny::incProgress(incr, detail = "Pre-Lease Report by Property")

              tryCatch({
                dbx::dbxUpsert(
                  conn,
                  table = DBI::SQL("entrata.pre_lease_summary_by_property"),
                  records = pre_lease_summary_by_property,
                  where_cols = c("report_date", "property_id"),
                  skip_existing = FALSE
                )
                cli::cli_alert_success("Pre-Lease Report by Property updated successfully!")
              }, error = function(e) {
                cli::cli_alert_danger("Error updating Pre-Lease Report by Property: {.error {e}}")
                shiny::showNotification("Error updating Pre-Lease Report by Property!", type = "error")
              })

              shiny::incProgress(incr, detail = "Pre-Lease Report by Unit")

              tryCatch({
                dbx::dbxUpsert(
                  conn,
                  table = DBI::SQL("entrata.pre_lease_summary_by_unit"),
                  records = pre_lease_summary_by_unit,
                  where_cols = c("report_date", "property_id", "unit_type"),
                  skip_existing = FALSE
                )
                cli::cli_alert_success("Pre-Lease Report by Unit updated successfully!")
              }, error = function(e) {
                cli::cli_alert_danger("Error updating Pre-Lease Report by Unit: {.error {e}}")
                shiny::showNotification("Error updating Pre-Lease Report by Unit!", type = "error")
              })

              shiny::incProgress(incr, detail = "Pre-Lease Report Details")

              tryCatch({
                pool::dbWriteTable(
                  pool,
                  DBI::SQL("entrata.pre_lease_report_details"),
                  value = pre_lease_details,
                  overwrite = TRUE
                )
                cli::cli_alert_success("Pre-Lease Report Details updated successfully!")
              }, error = function(e) {
                cli::cli_alert_danger("Error updating Pre-Lease Report Details: {.error {e}}")
                shiny::showNotification("Error updating Pre-Lease Report Details!", type = "error")
              })

              shiny::incProgress(incr, detail = "Refreshing Views and Pulling New Data from Database")
              cli::cli_alert_info("Data refreshed successfully!")
              shiny::setProgress(100, detail = "Data refreshed successfully!")
              shiny::showNotification("Data refreshed successfully!", type = "message")

            })

            db_trigger(db_trigger() + 1)
            shiny::removeModal()

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

      # excel
      shiny::observeEvent(input$excel, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Excel Report Options",
            size = "l",
            easyClose = TRUE,
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::downloadButton(
                ns("download"),
                "Export to Excel",
                class = "btn-success",
                icon = shiny::icon("file-excel")
              )
            ),
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::div(
                  bsicons::bs_icon("file-excel", class = "me-2"),
                  "Report Name"
                )
              ),
              bslib::layout_columns(
                col_widths = c(6, 6),
                shiny::textInput(
                  ns("report_name"),
                  label = "Report Name",
                  value = "GMH Pre-Lease Summary"
                ),
                bslib::input_switch(
                  ns("include_current_date"),
                  label = "Include Current Date in File Name?",
                  value = TRUE
                )
              ),
              bslib::card_footer(
                htmltools::tags$p(
                  "Derived Report File Name: ",
                  htmltools::tags$strong(
                    shiny::textOutput(
                      ns("report_file_name"),
                      inline = TRUE
                    )
                  )
                )
              )
            ),
            shiny::radioButtons(
              ns("export_data"),
              label = "Export Data",
              choices = c(
                "All Data" = "all",
                "Filtered Data" = "filtered"
              ),
              selected = "filtered",
              width = "100%"
            )
          )
        )
      })

      output$report_file_name <- shiny::renderText({
        get_xl_report_file_name(report_name = input$report_name,
                                report_date = Sys.Date())
      })

      # download
      output$download <- shiny::downloadHandler(
        filename = function() {
          get_xl_report_file_name(report_name = input$report_name,
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

              if (input$export_data == "filtered") {
                xl_data <- xl_data |>
                  dplyr::filter(
                    .data$investment_partner %in% input$partners,
                    .data$property_name %in% input$properties
                  )
              }

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

              # wb_template <- openxlsx2::wb_load(
              #   pkg_sys("templates/excel/pre-lease-summary-template.xlsx"),
              #   sheet = "Pre-Lease Summary"
              # )

              wb_template <- openxlsx2::wb_load(
                pkg_sys("templates/excel/template.xlsx"),
                sheet = "Summary"
              )

              wb_init <- wb_template

              shiny::setProgress(40, message = "Writing Report Date to Excel...")

              wb_init$add_data(
                sheet = "Summary",
                x = report_date,
                start_row = 2,
                start_col = 2,
                col_names = FALSE,
                name = "report_date"
              )

              wb_init$add_named_region(
                sheet = "Summary",
                dims = "B2",
                name = "report_date",
                overwrite = TRUE
              )

              setProgress(50, message = "Writing Leasing Season Ending Date to Excel...")

              wb_init$add_data(
                sheet = "Summary",
                x = leasing_season_ending,
                start_row = 2,
                start_col = 24,
                col_names = FALSE,
                name = "leasing_season_ending"
              )

              wb_init$add_named_region(
                sheet = "Summary",
                dims = "X2",
                name = "leasing_season_ending",
                overwrite = TRUE
              )

              setProgress(60, message = "Writing Leasing Week to Excel...")

              wb_init$add_data(
                sheet = "Summary",
                x = leasing_week,
                start_row = 3,
                start_col = 2,
                col_names = FALSE,
                name = "leasing_week"
              )

              wb_init$add_named_region(
                sheet = "Summary",
                dims = "B3",
                name = "leasing_week",
                overwrite = TRUE
              )

              setProgress(70, message = "Writing Weeks Left to Lease to Excel...")

              wb_init$add_data(
                sheet = "Summary",
                x = weeks_left_to_lease,
                start_row = 3,
                start_col = 24,
                col_names = FALSE,
                name = "weeks_left_to_lease"
              )

              wb_init$add_named_region(
                sheet = "Summary",
                dims = "X3",
                name = "weeks_left_to_lease",
                overwrite = TRUE
              )

              setProgress(80, message = "Writing Data to Excel...")

              wb_init$add_data(
                sheet = "Summary",
                x = xl_data |>
                  dplyr::select(property_name, investment_partner),
                start_row = 6,
                start_col = 1,
                col_names = FALSE
              )

              wb_init$add_data(
                sheet = "Summary",
                x = xl_data |>
                  dplyr::select(
                    total_beds:current_total_renewals
                  ),
                start_row = 6,
                start_col = 3,
                col_names = FALSE
              )

              wb_init$add_data(
                sheet = "Summary",
                x = xl_data |>
                  dplyr::select(
                    prior_total_new:prior_total_renewals
                  ),
                start_row = 6,
                start_col = 11,
                col_names = FALSE
              )

              wb_init$add_data(
                sheet = "Summary",
                x = xl_data |>
                  dplyr::select(
                    weekly_new:weekly_renewal
                  ),
                start_row = 6,
                start_col = 17,
                col_names = FALSE
              )

              setProgress(90, message = "Writing Formulas to Excel...")

              wb_init$add_formula(
                sheet = "Summary",
                x = "SUM(G6:H6)",
                dims = paste0("I6:I", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "I6 / $C6",
                dims = paste0("J6:J", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "SUM(K6:L6)",
                dims = paste0("M6:M", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "M6 / $C6",
                dims = paste0("N6:N", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "I6 - M6",
                dims = paste0("O6:O", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "J6 - N6",
                dims = paste0("P6:P", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "SUM(Q6:R6)",
                dims = paste0("S6:S", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "S6 / $U6",
                dims = paste0("T6:T", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "$C6 - $I6",
                dims = paste0("U6:U", nrow(xl_data) + 5),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = "($U6 * V$5) / weeks_left_to_lease",
                dims = paste0("V6:X", nrow(xl_data) + 5),
                shared = TRUE
              )

              setProgress(90, message = "Styling Excel Data...")

              tbl_row_sty <- wb_init$get_cell_style(sheet = "Summary", dims = "A6:X6")

              wb_init$set_cell_style(
                sheet = "Summary",
                dims = paste0("A7:X", nrow(xl_data) + 5),
                style = tbl_row_sty
              )

              tot_row_num <- nrow(xl_data) + 7

              tot_row_cell_sty <- wb_init$get_cell_style(sheet = "Summary", dims = "A5")

              setProgress(90, message = "Creating Totals...")

              wb_init$add_data(
                sheet = "Summary",
                x = "Total/Average",
                start_row = tot_row_num,
                start_col = 1,
                col_names = FALSE
              )

              wb_init$set_cell_style(
                sheet = "Summary",
                dims = paste0("A", tot_row_num, ":X", tot_row_num),
                style = tbl_row_sty
              )

              wb_init$set_cell_style(
                sheet = "Summary",
                dims = paste0("A", tot_row_num),
                style = tot_row_cell_sty
              )

              # wb_init$set_row_heights(
              #   sheet = "Summary",
              #   rows = tot_row_num,
              #   heights = 30
              # )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUM(C6:C", tot_row_num - 2, ")"),
                dims = paste0("C", tot_row_num, ":E", tot_row_num),
                shared = TRUE
              )

              # =SUMPRODUCT(F6:F26, $C$6:$C$26) / $C$28
              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUMPRODUCT(F6:F", tot_row_num - 2, ", $C$6:$C$", tot_row_num - 2, ") / $C$", tot_row_num),
                dims = paste0("F", tot_row_num)
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUM(G6:G", tot_row_num - 2, ")"),
                dims = paste0("G", tot_row_num, ":I", tot_row_num),
                shared = TRUE
              )

              # =SUMPRODUCT(J6:J26, $C$6:$C$26) / $C$28
              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUMPRODUCT(J6:J", tot_row_num - 2, ", $C$6:$C$", tot_row_num - 2, ") / $C$", tot_row_num),
                dims = paste0("J", tot_row_num)
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUM(K6:K", tot_row_num - 2, ")"),
                dims = paste0("K", tot_row_num, ":M", tot_row_num),
                shared = TRUE
              )

              # =SUMPRODUCT(N6:N26, $C$6:$C$26) / $C$28
              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUMPRODUCT(N6:N", tot_row_num - 2, ", $C$6:$C$", tot_row_num - 2, ") / $C$", tot_row_num),
                dims = paste0("N", tot_row_num)
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("I", tot_row_num, " - M", tot_row_num),
                dims = paste0("O", tot_row_num)
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("J", tot_row_num, " - N", tot_row_num),
                dims = paste0("P", tot_row_num)
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUM(Q6:Q", tot_row_num - 2, ")"),
                dims = paste0("Q", tot_row_num, ":S", tot_row_num),
                shared = TRUE
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("S", tot_row_num, " / $C$", tot_row_num),
                dims = paste0("T", tot_row_num)
              )

              wb_init$add_formula(
                sheet = "Summary",
                x = paste0("SUM(U6:U", tot_row_num - 2, ")"),
                dims = paste0("U", tot_row_num, ":X", tot_row_num),
                shared = TRUE
              )


              # # remove rows if filtered / nrows between template and data differ
              # nrow_template <- 21
              # nrow_data <- nrow(xl_data)
              #
              # if (nrow_data < nrow_template) {
              #   start_row <- 6L
              #   end_row <- start_row + nrow_data - 1L
              #   remove_start_row <- end_row + 1L
              #   remove_end_row <- nrow_template + 5L
              #
              #   remove_dims <- openxlsx2::wb_dims(
              #     rows = remove_start_row:remove_end_row,
              #     cols = 1:10000000
              #   )
              #
              #   wb_template$clean_sheet(
              #     sheet = "Pre-Lease Summary",
              #     dims = remove_dims
              #   )
              # }

              setProgress(95, message = "Finalizing Excel File...")

              wb_init$save(file, overwrite = TRUE)

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

      # Selected property/unit reactive values
      selected_property <- shiny::reactiveVal(NULL)
      selected_unit <- shiny::reactiveVal(NULL)

      # Property info panel
      output$selected_property_info <- shiny::renderUI({
        shiny::req(selected_property())
        prop_data <- pre_lease_summary_data() |>
          dplyr::filter(property_name == selected_property())

        htmltools::tags$div(
          class = "alert alert-info m-3",
          htmltools::tags$div(
            class = "d-flex justify-content-between align-items-center",
            htmltools::tags$h4(class = "mb-0", selected_property()),
            htmltools::tags$span(
              class = "badge bg-primary",
              sprintf("Occupancy: %.1f%%", prop_data$current_occupancy * 100)
            )
          )
        )
      })

      # Unit info panel
      output$selected_unit_info <- shiny::renderUI({
        shiny::req(selected_property(), selected_unit())
        unit_data <- entrata_pre_lease_details_by_property_data() |>
          dplyr::filter(
            property_name == selected_property(),
            unit_type == selected_unit()
          )

        htmltools::tags$div(
          class = "alert alert-info m-3",
          htmltools::tags$div(
            class = "d-flex justify-content-between align-items-center",
            htmltools::tags$h4(class = "mb-0", paste("Unit Type:", selected_unit())),
            htmltools::tags$span(
              class = "badge bg-primary",
              sprintf("Available Units: %d", unit_data$available_units)
            )
          )
        )
      })

      # Selection state outputs
      output$has_property_selection <- shiny::reactive({
        !is.null(selected_property()) && length(selected_property()) > 0
      })
      shiny::outputOptions(output, "has_property_selection", suspendWhenHidden = FALSE)

      output$has_unit_selection <- shiny::reactive({
        !is.null(selected_unit()) && length(selected_unit()) > 0
      })
      shiny::outputOptions(output, "has_unit_selection", suspendWhenHidden = FALSE)


      # Property table selection
      shiny::observe({
        hold <- reactable::getReactableState("property_table", "selected")
        if (is.null(hold) || length(hold) == 0) {
          selected_property(NULL)
        } else {
          selected_property(pre_lease_summary_data()$property_name[hold])
        }
      })

      # Unit table selection
      shiny::observe({
        hold <- reactable::getReactableState("unit_table", "selected")
        if (is.null(hold) || length(hold) == 0) {
          selected_unit(NULL)
        } else {
          selected_unit(unit_data()$unit_type[hold])
        }
      })

      shiny::observe({
        shiny::req(selected_property(), selected_unit())
        cli::cli_alert_info(
          c(
            "Selected Property: {.field {selected_property()}}; ",
            "Selected Unit: {.field {selected_unit()}}"
          )
        )
      })


      # entrata_pre_lease_summary_data <- shiny::reactive({
      #   shiny::req(pool, db_trigger())
      #   db_read_tbl(pool, "entrata.pre_lease_report_summary") |>
      #     dplyr::filter(
      #       .data$report_date == max(.data$report_date, na.rm = TRUE),
      #       .data$property_name %in% input$properties
      #     )
      # })

      entrata_pre_lease_details_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_details_w_charge_codes") |>
          dplyr::filter(
            .data$report_date == max(.data$report_date, na.rm = TRUE),
            .data$property_name %in% input$properties
          ) |>
          dplyr::select(
            -lease_started_on_date,
            -lease_partially_completed_on_date,
            -lease_approved_on_date,
            -ledger_name,
            -deposit_held,
            -advertised_rate,
            -scheduled_rent_total
          ) |>
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric),
                          function(x) {
                            as.numeric(x) |> dplyr::coalesce(0.00)
                          }
            ),
            dplyr::across(tidyselect::where(is.integer),
                          function(x) {
                            as.integer(x) |> dplyr::coalesce(0L)
                          }
            )
          )
      })

      entrata_pre_lease_details_by_property_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_summary_by_unit") |>
          dplyr::filter(
            .data$report_date == max(.data$report_date, na.rm = TRUE),
            .data$property_name %in% input$properties
          ) |>
          dplyr::transmute(
            report_date = .data$report_date,
            property_id = .data$property_id,
            property_name = .data$property_name,
            unit_type = .data$unit_type,
            total_units = .data$total_unit_count,
            excluded_units = .data$excluded_unit_count,
            rentable_units = .data$rentable_unit_count,
            available_units = .data$available_count,
            avg_scheduled_charges = .data$avg_scheduled_rent,
            current_occupied = .data$occupied_count,
            current_occupancy = .data$occupied_count / .data$available_count,
            current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
            current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
            current_total_leases = .data$current_total_new + .data$current_total_renewals,
            current_preleased_percent = .data$current_total_leases / .data$available_count,
            prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
            prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
            prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
            prior_preleased_percent = .data$prior_total_leases / .data$available_count,
            yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
            yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent,
          ) |>
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric),
                          function(x) {
                            as.numeric(x) |> dplyr::coalesce(0.00)
                          }
            ),
            dplyr::across(tidyselect::where(is.integer),
                          function(x) {
                            as.integer(x) |> dplyr::coalesce(0L)
                          }
            )
          )
      })

      unit_data <- shiny::reactive({
        shiny::req(selected_property())
        entrata_pre_lease_details_by_property_data() |>
          dplyr::filter(property_name == selected_property())

      })

      detail_data <- shiny::reactive({
        shiny::req(selected_property(), selected_unit())
        entrata_pre_lease_details_data() |>
          dplyr::filter(
            property_name == selected_property(),
            unit_type == selected_unit()
          )
      })

      # Property level table
      output$property_table <- reactable::renderReactable({

        tbl_data <- pre_lease_summary_data() |>
          dplyr::select(
            report_date,
            property_id,
            property_name,
            total_beds,
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
            beds_left,
            vel_90,
            vel_95,
            vel_100
          )

        reactable::reactable(
          tbl_data,
          selection = "single",
          onClick = "select",
          highlight = TRUE,
          striped = TRUE,
          bordered = TRUE,
          outlined = TRUE,
          compact = TRUE,
          sortable = TRUE,
          defaultSorted = c("property_name"),
          showSortIcon = TRUE,
          showSortable = TRUE,
          theme = reactable::reactableTheme(
            headerStyle = list(
              background = gmh_colors("primary"),
              color = gmh_colors("light"),
              "&:hover[aria-sort]" = list(background = gmh_colors("secondary")),
              "&[aria-sort]" = list(background = gmh_colors("secondary")),
              borderRight = paste0("1px solid ", gmh_colors("light"))
            ),
            rowSelectedStyle = list(
              backgroundColor = "#eee",
              boxShadow = paste0("inset 2px 0 0 0 ", gmh_colors("primary"))
            )
          ),
          defaultColDef = reactable::colDef(
            align = "center",
            headerVAlign = "center",
            vAlign = "center",
            format = reactable::colFormat(separators = TRUE),
            na = "-"
          ),
          columns = list(
            .selection = reactable::colDef(
              width = 50,
              sticky = "left",
              style = list(cursor = "pointer")
            ),
            report_date = reactable::colDef(show = FALSE),
            property_id = reactable::colDef(show = FALSE),
            property_name = reactable::colDef(
              name = "Property Name",
              width = 250,
              sticky = "left",
              cell = function(value, index) {
                property_id <- tbl_data$property_id[index]
                property_id <- if (!is.na(property_id)) property_id else "Unknown"
                htmltools::tags$div(
                  htmltools::tags$div(style = "font-weight: 600;", value),
                  htmltools::tags$div(style = "font-size: 0.75rem; color: #666;", property_id)
                )
              }
            ),
            total_beds = reactable::colDef(
              name = "Total Beds",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_occupied = reactable::colDef(
              name = "Occupied",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_occupancy = reactable::colDef(
              name = "Occupancy %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
              width = 150,
              align = "center",
              vAlign = "center"
            ),
            current_total_new = reactable::colDef(
              name = "New Leases",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_total_renewals = reactable::colDef(
              name = "Renewals",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_total_leases = reactable::colDef(
              name = "Total New",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_preleased_percent = reactable::colDef(
              name = "Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              width = 150,
              align = "center",
              vAlign = "center",
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            ),
            prior_total_new = reactable::colDef(
              name = "Prior New Leases",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            prior_total_renewals = reactable::colDef(
              name = "Prior Renewals",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            prior_total_leases = reactable::colDef(
              name = "Prior Total New",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            prior_preleased_percent = reactable::colDef(
              name = "Prior Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              width = 150,
              align = "center",
              vAlign = "center",
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            ),
            yoy_variance_count = reactable::colDef(
              name = "YoY Change",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            yoy_variance_percent = reactable::colDef(
              name = "Yoy % Change",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            ),
            beds_left = reactable::colDef(
              name = "Beds Left",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            vel_90 = reactable::colDef(
              name = "90% Velocity",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            vel_95 = reactable::colDef(
              name = "95% Velocity",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            vel_100 = reactable::colDef(
              name = "100% Velocity",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            )
          )
        )
      })

      # Unit level table
      output$unit_table <- reactable::renderReactable({
        req(unit_data())

        reactable::reactable(
          unit_data(),
          selection = "single",
          onClick = "select",
          highlight = TRUE,
          striped = TRUE,
          bordered = TRUE,
          resizable = TRUE,
          outlined = TRUE,
          compact = TRUE,
          theme = reactable::reactableTheme(
            headerStyle = list(
              background = gmh_colors("primary"),
              color = gmh_colors("light"),
              "&:hover[aria-sort]" = list(background = gmh_colors("secondary")),
              "&[aria-sort]" = list(background = gmh_colors("secondary")),
              borderRight = paste0("1px solid ", gmh_colors("light"))
            ),
            rowSelectedStyle = list(
              backgroundColor = "#eee",
              boxShadow = paste0("inset 2px 0 0 0 ", gmh_colors("primary"))
            )
          ),
          defaultColDef = reactable::colDef(
            align = "center",
            headerVAlign = "center",
            vAlign = "center",
            format = reactable::colFormat(separators = TRUE)
          ),
          columns = list(
            .selection = reactable::colDef(
              width = 50,
              sticky = "left",
              style = list(cursor = "pointer")
            ),
            report_date = reactable::colDef(show = FALSE),
            property_id = reactable::colDef(show = FALSE),
            property_name = reactable::colDef(show = FALSE),
            unit_type = reactable::colDef(
              name = "Unit Type",
              width = 150,
              sticky = "left"
            ),
            total_units = reactable::colDef(
              name = "Total Units",
              format = reactable::colFormat(digits = 0, separators = TRUE),
              aggregate = "sum"
            ),
            excluded_units = reactable::colDef(
              name = "Excluded Units",
              format = reactable::colFormat(digits = 0, separators = TRUE),
              aggregate = "sum"
            ),
            rentable_units = reactable::colDef(
              name = "Rentable Units",
              format = reactable::colFormat(digits = 0, separators = TRUE),
              aggregate = "sum"
            ),
            available_units = reactable::colDef(
              name = "Available Units",
              format = reactable::colFormat(digits = 0, separators = TRUE),
              aggregate = "sum"
            ),
            avg_scheduled_charges = reactable::colDef(
              name = "Avg Scheduled Charges",
              format = reactable::colFormat(currency = "USD", digits = 1),
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            ),
            current_occupied = reactable::colDef(
              name = "Current Occupied",
              format = reactable::colFormat(digits = 0, separators = TRUE),
              aggregate = "sum"
            ),
            current_occupancy = reactable::colDef(
              name = "Occupancy %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }"),
              width = 150,
              align = "center",
              vAlign = "center"
            ),
            current_total_new = reactable::colDef(
              name = "New Leases",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_total_renewals = reactable::colDef(
              name = "Renewals",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_total_leases = reactable::colDef(
              name = "Total New Leases",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            current_preleased_percent = reactable::colDef(
              name = "Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              width = 150,
              align = "center",
              vAlign = "center",
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            ),
            prior_total_new = reactable::colDef(
              name = "Prior New Leases",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            prior_total_renewals = reactable::colDef(
              name = "Prior Renewals",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            prior_total_leases = reactable::colDef(
              name = "Prior Total New",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            prior_preleased_percent = reactable::colDef(
              name = "Prior Pre-Lease %",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              width = 150,
              align = "center",
              vAlign = "center",
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            ),
            yoy_variance_count = reactable::colDef(
              name = "YoY Change",
              format = reactable::colFormat(separators = TRUE, digits = 0),
              aggregate = "sum"
            ),
            yoy_variance_percent = reactable::colDef(
              name = "YoY % Change",
              format = reactable::colFormat(percent = TRUE, digits = 1),
              aggregate = reactable::JS("function(values) { return values.reduce((a, b) => a + b, 0) / values.length }")
            )
          )
        )
      })

      # Detail level table
      output$detail_table <- reactable::renderReactable({
        shiny::req(detail_data())

        tbl_data <- detail_data() |>
          dplyr::arrange(
            .data$bldg_unit,
            .data$move_in_date
          ) |>
          dplyr::mutate(
            dplyr::across(tidyselect::where(is.numeric),
                          function(x) {
                            as.numeric(x) |> dplyr::coalesce(0.00)
                          }
            ),
            dplyr::across(tidyselect::where(is.integer),
                          function(x) {
                            as.integer(x) |> dplyr::coalesce(0L)
                          }
            )
          )

        reactable::reactable(
          tbl_data,
          highlight = TRUE,
          striped = TRUE,
          bordered = TRUE,
          outlined = TRUE,
          compact = TRUE,
          theme = reactable::reactableTheme(
            highlightColor = "#f0f0f0",
            borderColor = "#dfe2e5",
            stripedColor = "#f6f8fa",
            cellPadding = "8px 12px",
            headerStyle = list(
              background = gmh_colors("primary"),
              color = gmh_colors("light"),
              "&:hover[aria-sort]" = list(background = gmh_colors("secondary")),
              "&[aria-sort]" = list(background = gmh_colors("secondary")),
              borderRight = paste0("1px solid ", gmh_colors("light"))
            )
          ),
          defaultColDef = reactable::colDef(
            align = "center",
            headerVAlign = "center",
            vAlign = "center",
            format = reactable::colFormat(separators = TRUE)
          ),
          columns = list(
            report_date = reactable::colDef(show = FALSE),
            property_id = reactable::colDef(show = FALSE),
            property_name = reactable::colDef(show = FALSE),
            unit_type = reactable::colDef(show = FALSE),
            floorplan_name = reactable::colDef(show = FALSE),
            sqft = reactable::colDef(show = FALSE),
            resident_id = reactable::colDef(show = FALSE),
            resident_name = reactable::colDef(show = FALSE),
            resident_email = reactable::colDef(show = FALSE),
            resident_phone = reactable::colDef(show = FALSE),
            resident_gender = reactable::colDef(show = FALSE),
            leasing_agent = reactable::colDef(show = FALSE),
            lease_id = reactable::colDef(show = FALSE),
            lease_sub_status = reactable::colDef(show = FALSE),
            lease_occupancy_type = reactable::colDef(show = FALSE),
            lease_term_name = reactable::colDef(show = FALSE),
            lease_term_month = reactable::colDef(show = FALSE),
            space_option_preferred = reactable::colDef(show = FALSE),
            space_option = reactable::colDef(show = FALSE),
            move_in_date = reactable::colDef(show = FALSE),
            lease_start_date = reactable::colDef(show = FALSE),
            lease_end_date = reactable::colDef(show = FALSE),
            lease_completed_on_date = reactable::colDef(show = FALSE),
            bldg_unit = reactable::colDef(name = "Building", width = 100),
            unit_status = reactable::colDef(name = "Unit Status", width = 150),
            lease_status = reactable::colDef(name = "Lease Status"),
            charge_code = reactable::colDef(name = "Charge Code"),
            deposit_charged = reactable::colDef(name = "Deposit Charged", format = reactable::colFormat(currency = "USD", digits = 1)),
            market_rent = reactable::colDef(name = "Market Rent", format = reactable::colFormat(currency = "USD", digits = 1)),
            budgeted_rent = reactable::colDef(name = "Budgeted Rent", format = reactable::colFormat(currency = "USD", digits = 1)),
            scheduled_rent = reactable::colDef(name = "Scheduled Rent", format = reactable::colFormat(currency = "USD", digits = 1)),
            actual_charges = reactable::colDef(name = "Actual Charges", format = reactable::colFormat(currency = "USD", digits = 1))
          ),

          details = function(index) {
            detail <- tbl_data[index, ]
            htmltools::div(
              class = "p-3",
              htmltools::div(
                style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 1rem;",
                htmltools::tags$div(
                  class = "card",
                  htmltools::tags$div(
                    class = "card-header",
                    "Additional Information"
                  ),
                  htmltools::div(
                    class = "card-body",
                    htmltools::tags$table(
                      class = "table table-sm table-borderless",
                      htmltools::tags$tbody(
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Floor Plan:")),
                          htmltools::tags$td(detail$floorplan_name)
                        ),
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Square Footage:")),
                          htmltools::tags$td(detail$sqft)
                        )
                      )
                    )
                  )
                ),
                htmltools::div(
                  class = "card",
                  htmltools::div(
                    class = "card-header",
                    "Lease Information"
                  ),
                  htmltools::div(
                    class = "card-body",
                    htmltools::tags$table(
                      class = "table table-sm table-borderless",
                      htmltools::tags$tbody(
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Lease ID:")),
                          htmltools::tags$td(detail$lease_id)
                        ),
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Lease Sub Status:")),
                          htmltools::tags$td(detail$lease_sub_status)
                        ),
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Move In Date:")),
                          htmltools::tags$td(detail$move_in_date)
                        )
                      )
                    )
                  )
                ),
                htmltools::div(
                  class = "card",
                  htmltools::div(
                    class = "card-header",
                    "Resident Information"
                  ),
                  htmltools::div(
                    class = "card-body",
                    htmltools::tags$table(
                      class = "table table-sm table-borderless",
                      htmltools::tags$tbody(
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Resident ID:")),
                          htmltools::tags$td(detail$resident_id)
                        ),
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Resident Name:")),
                          htmltools::tags$td(detail$resident_name)
                        ),
                        htmltools::tags$tr(
                          htmltools::tags$td(htmltools::tags$strong("Resident Gender:")),
                          htmltools::tags$td(detail$resident_gender)
                        )
                      )
                    )
                  )
                )
              )
            )
          }
        )

      })

      shiny::observeEvent(selected_property(), {
        if (!is.null(selected_property()) && length(selected_property()) > 0) {
          shinyjs::runjs(
            sprintf(
              "setTimeout(function() {
                document.querySelector(\"[data-value=\'%s\']\").scrollIntoView({
                  behavior: \"smooth\",
                  block: \"start\"
                });
              }, 100);",
              ns("unit_level")
            )
          )
          bslib::accordion_panel_open(
            id = "accordion",
            values = ns("unit_level")
          )
          bslib::accordion_panel_close(
            id = "accordion",
            values = ns("property_level")
          )
        }
      })

      shiny::observeEvent(selected_unit(), {
        if (!is.null(selected_unit()) && length(selected_unit()) > 0) {
          shinyjs::runjs(
            sprintf(
              "setTimeout(function() {
                document.querySelector(\"[data-value=\'%s\']\").scrollIntoView({
                  behavior: \"smooth\",
                  block: \"start\"
                });
              }, 100);",
              ns("details_level")
            )
          )
          bslib::accordion_panel_open(
            id = "accordion",
            values = ns("details_level")
          )
          bslib::accordion_panel_close(
            id = "accordion",
            values = ns("unit_level")
          )
        }
      })

      return(
        list(
          pre_lease_summary_data = pre_lease_summary_data,
          entrata_pre_lease_details_data = entrata_pre_lease_details_data,
          entrata_pre_lease_details_by_property_data = entrata_pre_lease_details_by_property_data,
          unit_data = unit_data,
          detail_data = detail_data
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
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-primary text-white",
        htmltools::div(
          bsicons::bs_icon("table", class = "me-2"),
          "Entrata Pre-Lease Details"
        )
      ),
      bslib::card_body(
        bslib::card(
          height = "auto",
          bslib::card_header(
            class = "bg-info",
            htmltools::div(
              bsicons::bs_icon("info-circle", class = "me-2"),
              "Information"
            )
          ),
          bslib::card_body(
            htmltools::tags$p(
              "This page is still under development. Please check back later for updates."
            )
          )
        ),
        htmltools::tags$hr(),
        reactable::reactableOutput(ns("table")) |>
          with_loader()
      ),
      bslib::card_footer(
        class = "bg-light",
        htmltools::tags$small(
          "Last updated: ",
          shiny::textOutput(ns("last_updated"), inline = TRUE)
        )
      )
    )
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

      output$last_updated <- shiny::renderText({
        summary_data() |>
          dplyr::pull("report_date") |>
          lubridate::ymd() |>
          max(na.rm = TRUE) |>
          format("%B %d, %Y")
      })

      return(list(
        # summary_data = summary_data,
        # details_data = details_data
      ))
    })

}



