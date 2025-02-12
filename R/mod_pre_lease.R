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
  default_partners <- get_default_app_choices("partners") |> names()
  default_properties <- get_default_app_choices("properties")

  htmltools::tagList(
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
            showcase = bsicons::bs_icon("percent")
          ),
          bslib::value_box(
            title = "New Leases",
            subtitle = "Total new leases signed this period",
            value = shiny::textOutput(ns("val_new_leases"), inline = TRUE),
            showcase = bsicons::bs_icon("file-earmark-plus")
          ),
          bslib::value_box(
            title = "Renewals",
            value = shiny::textOutput(ns("val_new_renewals"), inline = TRUE),
            showcase = bsicons::bs_icon("arrow-repeat")
          ),
          bslib::value_box(
            title = "Year-Over-Year Change",
            value = shiny::textOutput(ns("val_yoy_pct_change"), inline = TRUE),
            showcase = bsicons::bs_icon("graph-up-arrow")
          )
        )
      ),
      # nav card ----------------------------------------------------------------
      bslib::navset_card_underline(
        id = ns("nav"),
        full_screen = TRUE,
        sidebar = bslib::sidebar(
          id = ns("sidebar"),
          title = "Controls",
          shiny::actionButton(
            inputId = ns("entrata_settings"),
            label = "Entrata Settings",
            icon = shiny::icon("gear"),
            class = "btn-primary"
          ),
          bslib::accordion(
            id = ns("accordion"),
            bslib::accordion_panel(
              title = "Filters",
              value = ns("filters"),
              icon = bsicons::bs_icon("funnel"),
              shiny::selectInput(
                ns("partners"),
                label = "Investment Partners",
                choices = default_partners,
                selectize = FALSE,
                selected = default_partners,
                multiple = TRUE
              ),
              shiny::selectInput(
                ns("properties"),
                label = "Properties",
                choices = default_properties,
                selectize = FALSE,
                selected = default_properties,
                multiple = TRUE
              ),
              shiny::dateInput(
                ns("report_date"),
                label = "Report Date",
                value = default_report_date
              ),
              shiny::dateInput(
                ns("leasing_date"),
                label = "Pre-Lease Date",
                value = default_leasing_date
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
            height = "100%",
            class = "mb-4",
            bslib::card_header(
              class = "bg-primary text-white d-flex justify-content-between align-items-center",
              htmltools::div(
                bsicons::bs_icon("table", class = "me-2"),
                "Pre-Lease Summary"
              ),
              bslib::input_task_button(
                id = ns("refresh"),
                icon = bsicons::bs_icon("arrow-clockwise"),
                label = "Refresh",
                class = "btn-info btn-sm",
                style = "margin-left: auto; display: inline-flex; align-items: center; padding: 2.5px 10px;"
              ),
              shiny::downloadButton(
                ns("download"),
                "Export to Excel",
                class = "btn-success btn-sm",
                style = "margin-left: 10px; display: inline-flex; align-items: center; padding: 2.5px 10px;"
              )
            ),
            bslib::card_body(
              class = "p-0",
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
            full_screen = TRUE,
            height = "100%",
            class = "mb-4",
            bslib::card_header(
              class = "bg-primary text-white",
              htmltools::div(
                bsicons::bs_icon("table", class = "me-2"),
                "Pre-Lease Details"
              )
            ),
            bslib::card_body(
              class = "p-0",
              entrata_table_ui(ns("details_table"))
            )
          )
        ),
        bslib::nav_panel(
          title = "Charts",
          icon = bsicons::bs_icon("bar-chart"),
          value = "charts",
          bslib::layout_columns(
            col_widths = c(6, 6),
            gap = "1rem",
            bslib::card(
              full_screen = TRUE,
              class = "h-100",
              bslib::card_header(
                class = "bg-dark text-white",
                htmltools::div(
                  bsicons::bs_icon("bar-chart", class = "me-2"),
                  "Occupancy vs. Target"
                )
              ),
              bslib::card_body(
                class = "p-3",
                plotly::plotlyOutput(ns("occupancy_chart"), height = "400px")
              )
            ),
            bslib::card(
              full_screen = TRUE,
              class = "h-100",
              bslib::card_header(
                class = "bg-dark text-white",
                htmltools::div(
                  bsicons::bs_icon("pie-chart", class = "me-2"),
                  "New vs. Renewal Distribution"
                )
              ),
              bslib::card_body(
                class = "p-3",
                plotly::plotlyOutput(ns("lease_type_chart"), height = "400px")
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "About",
          icon = bsicons::bs_icon("info-circle"),
          value = "about",
          bslib::card(
            class = "mb-4",
            bslib::card_header(
              class = "bg-dark text-white",
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

      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_server()")

      # database
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      selected_row <- shiny::reactiveVal(NULL)
      db_trigger <- shiny::reactiveVal(0)

      # reactive values for filters
      filters <- shiny::reactiveValues(
        partners = NULL,
        properties = NULL,
        report_date = NULL,
        leasing_date = NULL
      )

      # observe inputs and update reactives
      shiny::observe({
        filters$partners <- input$partners
        filters$properties <- input$properties
        filters$report_date <- input$report_date
        filters$leasing_date <- input$leasing_date
      }) |>
        shiny::bindEvent(
          input$partners,
          input$properties,
          input$report_date,
          input$leasing_date
        )

      # observe partners to update properties
      shiny::observeEvent(filters$partners, {

        partner_properties <- db_read_tbl(pool, "gmh.pre_lease_global_summary", collect = FALSE) |>
          dplyr::filter(
            investment_partner %in% filters$partners
          ) |>
          dplyr::select("property_id", "property_name") |>
          dplyr::collect() |>
          tibble::deframe()

        shiny::updateSelectInput(
          session,
          "properties",
          choices = partner_properties,
          selected = partner_properties
        )

        cli::cli_alert_info(
          c(
            "Investment Partners selected: {.field {input$partners}}",
            "Properties updated to: {.field {partner_properties}}"
          )
        )

      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # observe properties to update partners
      shiny::observeEvent(filters$properties, {

        prop_id <- as.integer(filters$properties)

        property_partners <- db_read_tbl(pool, "gmh.pre_lease_global_summary", collect = FALSE) |>
          dplyr::filter(
            .data$property_id %in% .env$prop_id
          ) |>
          dplyr::pull("investment_partner") |>
          unique()

        shiny::updateSelectInput(
          session,
          "partners",
          choices = property_partners,
          selected = property_partners
        )

        cli::cli_alert_info(
          c(
            "Properties selected: {.field {input$properties}}. ",
            "Investment Partners updated to: {.field {property_partner_names}}"
          )
        )

      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      # initial data
      pre_lease_summary_data <- shiny::reactive({
        shiny::req(db_trigger())
        db_read_tbl(pool, "gmh.pre_lease_global_summary")
      })

      # summary table
      output$summary_table <- reactable::renderReactable({
        shiny::req(pre_lease_summary_data())
        tbl_pre_lease_summary(pre_lease_summary_data(), ns = ns)
      })

      shiny::observeEvent(input$edit_row, {
        row <- input$edit_row
        selected_row(row)
        row_data <- pre_lease_summary_data() |>
          dplyr::filter(dplyr::row_number() == row)
        selected_property <- row_data |> dplyr::pull("property_name")
        tit <- paste0("Edit Data for ", selected_property)

        shiny::showModal(
          shiny::modalDialog(
            title = tit,
            size = "m",
            easyClose = TRUE,
            htmltools::tagList(
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
        shiny::req(selected_row(), input$model_beds, input$investment_partner)

        new_value <- input$model_beds

        rept_date <- pre_lease_summary_data() |>
          dplyr::pull("report_date") |>
          lubridate::ymd() |>
          max(na.rm = TRUE)

        prop_id <- pre_lease_summary_data() |>
          dplyr::filter(dplyr::row_number() == selected_row()) |>
          dplyr::pull("property_id") |>
          as.integer()

        dat <- pre_lease_summary_data() |>
          dplyr::filter(
            .data$property_id == .env$prop_id,
            .data$report_date == .env$rept_date
          ) |>
          dplyr::mutate(
            model_beds = new_value
          )

        conn <- pool::poolCheckout(pool = pool)

        tryCatch({
          dbx::dbxUpdate(
            conn,
            table = DBI::SQL("gmh.pre_lease_summary"),
            records = dat,
            where_cols = c("property_id", "report_date"),
            transaction = TRUE
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
        })

      })

      # refresh
      shiny::observeEvent(input$refresh, {
        db_trigger(db_trigger() + 1)
        shiny::showNotification("Data refreshed successfully!", type = "message")
      })

      # entrata settings
      shiny::observeEvent(input$entrata_settings, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Configure Entrata API",
            size = "l",
            easyClose = TRUE,
            htmltools::tagList(
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
                      "Do Not Summarize" = "do_not_summarize",
                      "Property" = "property",
                      "Unit Type" = "unit_type",
                      "Floorplan Name" = "floorplan_name"
                    ),
                    selected = "do_not_summarize"
                  ),
                  shiny::radioButtons(
                    ns("group_by"),
                    label = "Group By",
                    choices = c(
                      "Do Not Group" = "do_not_group",
                      "Property" = "property",
                      "Unit Type" = "unit_type",
                      "Floorplan Name" = "floorplan_name",
                      "Lease Term" = "lease_term"
                    ),
                    selected = "do_not_group"
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
                  )
                )
              )
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                ns("confirm"),
                "Confirm",
                class = "btn-primary"
              )
            )
          )
        )
      })

      # confirm entrata
      shiny::observeEvent(input$confirm, {
        shiny::req(input$properties, input$period_date, input$summarize_by, input$group_by, input$consolidate_by, input$space_options, input$charge_code_detail, input$additional_units_show, input$combine_unit_spaces_with_same_lease, input$arrange_by_property, input$yoy, input$subtotals)

        cli::cli_alert_info("Refreshing data...")
        data <- shiny::withProgress(
          message = "Refreshing data...",
          value = 0,
          {
            shiny::setProgress(10, message = "Refreshing data...")
            entrata_pre_lease_report(
              property_ids = input$properties
            )
            shiny::setProgress(100, message = "Data refreshed successfully!")
          }
        )
        cli::cli_alert_info("Data refreshed successfully!")
        shiny::showNotification("Data refreshed successfully!", type = "message")
        db_trigger(db_trigger() + 1)
        shiny::removeModal()
      })



      # value boxes
      output$val_avg_occupancy <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("current_occupancy") |>
          mean(na.rm = TRUE) |>
          scales::percent()
      })

      output$val_new_leases <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("current_total_new") |>
          sum(na.rm = TRUE)
      })

      output$val_new_renewals <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("current_total_renewals") |>
          sum(na.rm = TRUE)
      })

      output$val_yoy_pct_change <- shiny::renderText({
        shiny::req(pre_lease_summary_data())
        pre_lease_summary_data() |>
          dplyr::pull("yoy_variance_percent") |>
          mean(na.rm = TRUE) |>
          scales::percent()
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

      # occupancy chart
      output$occupancy_chart <- plotly::renderPlotly({
        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data()

        plot_height <- 400

        p <- plotly::plot_ly(
          data = df,
          height = plot_height
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_occupancy,
            type = "bar",
            name = "Current Occupancy %",
            marker = list(color = "#2C3E50")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~ rep(0.90, nrow(df)),
            type = "scatter",
            mode = "lines",
            name = "90% Target",
            line = list(color = "#E74C3C", dash = "dash")
          ) |>
          plotly::layout(
            autosize = TRUE,
            height = plot_height,
            margin = list(
              l = 50,
              r = 50,
              b = 100,
              t = 30,
              pad = 4
            ),
            bargap = 0.2,
            xaxis = list(
              tickangle = 45,
              automargin = TRUE,
              title = list(text = "Property")
            ),
            yaxis = list(
              tickformat = ".0%",
              automargin = TRUE,
              title = list(text = "Occupancy %"),
              range = c(0, 1)
            ),
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              y = -0.2
            ),
            barmode = "group"
          )

        p
      })

      # lease type chart
      output$lease_type_chart <- plotly::renderPlotly({
        shiny::req(pre_lease_summary_data())

        df <- pre_lease_summary_data()

        p <- plotly::plot_ly(
          data = df,
          height = 500 # Explicit height
        ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_total_new,
            type = "bar",
            name = "New Leases",
            marker = list(color = "#2C3E50")
          ) |>
          plotly::add_trace(
            x = ~property_name,
            y = ~current_total_renewals,
            type = "bar",
            name = "Renewals",
            marker = list(color = "#18BC9C")
          ) |>
          plotly::layout(
            autosize = TRUE,
            margin = list(
              l = 50,
              r = 50,
              b = 100,
              t = 50,
              pad = 4
            ),
            xaxis = list(
              tickangle = 45,
              automargin = TRUE
            ),
            yaxis = list(
              automargin = TRUE
            ),
            showlegend = TRUE,
            barmode = "stack"
          )

        p
      })

      entrata_pre_lease_summary_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_summary") |>
          dplyr::filter(
            report_date == Sys.Date()
          )
      })

      entrata_pre_lease_details_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_details") |>
          dplyr::filter(
            report_date == Sys.Date()
          )
      })

      entrata_pre_lease_details_by_property_data <- shiny::reactive({
        shiny::req(pool, db_trigger())
        db_read_tbl(pool, "entrata.pre_lease_report_details_by_property") |>
          dplyr::filter(
            report_date == Sys.Date()
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
          # summary_data = pre_lease_summary_data,
          # details_data = pre_lease_details_data
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

mod_prelease_sidebar_ui <- function(id) {

  ns <- shiny::NS(id)


}

mod_prelease_sidebar_filters_server <- function(id, pool = NULL) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_prelease_sidebar_filters_server()")

      # check database
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)



      return(filters)
    }
  )
}

# table -------------------------------------------------------------------

pre_lease_tbl_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$script(
      sprintf("window.handleEditClick_%s = function(index) {
            Shiny.setInputValue('%s', {row: index}, {priority: 'event'});
            }", ns('edit_row'), ns('edit_row'))),
    reactable::reactableOutput(ns("table")) |> with_loader()
  )

}

pre_lease_tbl_server <- function(id, summary_data) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: pre_lease_tbl_server()")

      summary_data <- shiny::reactiveValue({
        summary_data() |>
          dplyr::mutate(.actions = NA)
      })
      selected_row <- shiny::reactiveVal(NULL)

      output$table <- reactable::renderReactable({
        shiny::req(summary_data())
        tbl_pre_lease_summary(summary_data(), ns = ns)
      })

      shiny::observeEvent(input$edit_row, {
        row <- input$edit_row
        selected_row(row)

        row_data <- pre_lease_summary_data() |>
          dplyr::filter(dplyr::row_number() == row)

        selected_property <- row_data |> dplyr::pull("property_name")

        tit <- paste0("Edit Data for ", selected_property)

        shiny::showModal(
          shiny::modalDialog(
            title = tit,
            size = "m",
            easyClose = TRUE,
            htmltools::tagList(
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
        req(input$modal_investment_partner, input$modal_model_beds)

        data <- rv()
        row <- selected_row()

        if (!is.null(row) && row >= 1 && row <= nrow(data)) {
          new_beds <- as.numeric(input$modal_model_beds)
          new_partner <- trimws(input$modal_investment_partner)

          if (!is.na(new_beds) && new_beds >= 0 && nchar(new_partner) > 0) {
            data[row, "model_beds"] <- new_beds
            data[row, "investment_partner"] <- new_partner
            rv(data)
          }
        }
        removeModal()
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      return(rv)
    })

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

mod_entrata_settings_ui <- function(id) {

  ns <- shiny::NS(id)


}

mod_entrata_settings_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: modal_refresh_server()")

      # entrata
      shiny::observeEvent(input$entrata_settings, {
        shiny::showModal(
          mod_entrata_settings_ui(ns("entrata_settings"))
        )
      })

      shiny::observeEvent(input$confirm, {
        shiny::req(
          input$properties,
          input$period_date,
          input$summarize_by,
          input$group_by,
          input$consolidate_by,
          input$space_options,
          input$charge_code_detail,
          input$additional_units_show,
          input$combine_unit_spaces_with_same_lease,
          input$arrange_by_property,
          input$yoy,
          input$subtotals
        )

      })

    }
  )
}

parse_pre_lease_report_filter_params <- function(
    property_ids = NULL,
    period_date = NULL,
    summarize_by = "property",
    group_by = "do_not_group",
    consolidate_by = "no_consolidation",
    consider_pre_leased_on = "332",
    space_options = "do_not_show",
    charge_code_detail = 1,
    additional_units_shown = "available",
    combine_unit_spaces_with_same_lease = 0,
    arrange_by_property = 0,
    subtotals = list("summary", "details"),
    yoy = 1,
    ...
) {

  group_by <- rlang::arg_match0(group_by, c("do_not_group", "unit_type", "floorplan_name", "lease_term"))
  summarize_by <- rlang::arg_match0(summarize_by, c("property", "unit_type", "floorplan_name", "do_not_summarize"))
  charge_code_detail <- purrr::pluck(additional_params, "charge_code_detail") %||% 1
  space_options <- purrr::pluck(additional_params, "space_options") %||% "do_not_show"
  additional_units_shown <- purrr::pluck(additional_params, "additional_units_shown") %||% "available"
  combine_unit_spaces_with_same_lease <- purrr::pluck(additional_params, "combine_unit_spaces_with_same_lease") %||% 0
  consolidate_by <- purrr::pluck(additional_params, "consolidate_by") %||% "no_consolidation"
  arrange_by_property <- purrr::pluck(additional_params, "arrange_by_property") %||% 0
  subtotals <- purrr::pluck(additional_params, "subtotals") %||% list("summary", "details")
  yoy <- purrr::pluck(additional_params, "yoy") %||% 1

  period_date <- get_entrata_custom_pre_lease_date(period_date) |> entrata_date()
  period <- list(date = period_date, period_type = "date")

  list(
    property_ids = as.list(as.character(property_ids)),
    period = period,
    summarize_by = summarize_by,
    group_by = group_by,
    consider_pre_leased_on = as.character(consider_pre_leased_on),
    charge_code_detail = charge_code_detail,
    consolidate_by = consolidate_by,

    space_options = space_options,
    charge_code_detail = charge_code_detail,
    additional_units_shown = additional_units_shown,
    combine_unit_spaces_with_same_lease = combine_unit_spaces_with_same_lease,
    arrange_by_property = arrange_by_property,
    subtotals = subtotals,
    yoy = yoy
  )

}


