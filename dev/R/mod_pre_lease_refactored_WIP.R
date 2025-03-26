

# pre-lease module ------------------------------------------------------------------------------------------------







# value boxes module ----------------------------------------------------------------------------------------------

mod_pre_lease_value_boxes_ui <- function(id, summary_data = NULL) {

  ns <- shiny::NS(id)

  htmltools::tagList(
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
      shinyjs::hidden()
  )

}

mod_pre_lease_value_boxes_server <- function(id, summary_data) {

  stopifnot(shiny::is.reactive(summary_data))

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cli::cat_rule("[Module]: mod_pre_lease_value_boxes_server()")

    # average occupancy percent
    output$val_avg_occupancy <- shiny::renderText({
      shiny::req(summary_data())
      summary_data() |>
        dplyr::pull("current_occupancy") |>
        mean(na.rm = TRUE) |>
        scales::percent(accuracy = 0.1)
    })

    # new leases
    output$val_new_leases <- shiny::renderText({
      shiny::req(summary_data())
      summary_data() |>
        dplyr::pull("current_total_new") |>
        sum(na.rm = TRUE) |>
        scales::comma()
    })

    # renewals
    output$val_new_renewals <- shiny::renderText({
      shiny::req(summary_data())
      summary_data() |>
        dplyr::pull("current_total_renewals") |>
        sum(na.rm = TRUE) |>
        scales::comma()
    })

    # year-over-year change
    output$val_yoy_pct_change <- shiny::renderText({
      shiny::req(summary_data())
      summary_data() |>
        dplyr::pull("yoy_variance_percent") |>
        mean(na.rm = TRUE) |>
        scales::percent(accuracy = 0.1)
    })

    # create a function to return for toggling the value boxes
    toggle_value_boxes <- function(show) {
      shinyjs::toggle(
        id = "value_boxes",
        anim = TRUE,
        animType = "fade",
        time = 0.5,
        condition = show
      )
    }

    # return the toggle function
    return(
      list(
        toggle_value_boxes = toggle_value_boxes
      )
    )

  })

}


# sidebar module --------------------------------------------------------------------------------------------------

mod_pre_lease_sidebar_ui <- function(id) {

  ns <- shiny::NS(id)

  bslib::sidebar(
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
  )

}


mod_pre_lease_sidebar_server <- function(id, summary_data, db_trigger) {

  stopifnot(
    shiny::is.reactive(summary_data),
    shiny::is.reactive(db_trigger)
  )

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cli::cat_rule("[Module]: mod_pre_lease_sidebar_server()")

    # update inputs from summary data
    shiny::observeEvent(summary_data(), {

      report_date <- summary_data() |>
        dplyr::pull("report_date") |>
        lubridate::ymd() |>
        max(na.rm = TRUE)

      partner_choices <- summary_data() |>
        dplyr::pull("investment_partner") |>
        unique()

      property_choices <- summary_data() |>
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

    # return the input values
    return(
      list(
        partners = shiny::reactive({ input$partners }),
        properties = shiny::reactive({ input$properties }),
        report_date = shiny::reactive({ input$report_date }),
        leasing_date = shiny::reactive({ input$leasing_date }),
        metric = shiny::reactive({ input$metric }),
        occupancy_target = shiny::reactive({ input$occupancy_target }),
        group_by = shiny::reactive({ input$group_by }),
        toggle_val_boxes = shiny::reactive({ input$toggle_val_boxes })
      )
    )

  })

}


# summary ---------------------------------------------------------------------------------------------------------

mod_pre_lease_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagsList(
    htmltools::tags$div(
      class = "summary-container",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$span(
            bsicons::bs_icon("table"),
            " Pre-Lease Summary"
          )
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("summary_table")) |> with_loader()
        ),
        bslib::card_footer(
          class = "text-muted",
          "Last Updated: ",
          shiny::textOutput(ns("last_updated"), inline = TRUE)
        )
      )
    )
  )

}


mod_pre_lease_summary_server <- function(id, summary_data, db_trigger, pool = NULL) {

  stopifnot(shiny::is.reactive(summary_data), shiny::is.reactive(db_trigger))

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # setup
      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_summary_server()")

      # db connection
      if (is.null(pool)) pool <- session$userData$db_pool %||% db_connect()
      check_db_conn(pool)

      # selected property
      selected_property <- shiny::reactiveVal(NULL)

      # selected row
      selected_row <- shiny::reactiveVal(NULL)

      # validator
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("model_beds", rule = shinyvalidate::sv_gte(0))

      # render the summary table
      output$summary_table <- reactable::renderReactable({
        shiny::req(summary_data())
        tbl_pre_lease_summary(summary_data(), ns = ns)
      })

      # last updated
      output$last_updated <- shiny::renderText({
        shiny::req(summary_data())
        summary_data() |>
          dplyr::pull("report_date") |>
          lubridate::ymd() |>
          max(na.rm = TRUE) |>
          format("%B %d, %Y")
      })

      # edit modal
      shiny::observeEvent(input$edit_row, {
        row <- input$edit_row
        selected_row(row)
        row_data <- pre_lease_summary_data() |>
          dplyr::filter(dplyr::row_number() == row)
        selected_property_val <- row_data |> dplyr::pull("property_name")
        selected_property(selected_property_val)

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Data",
            size = "m",
            easyClose = TRUE,
            htmltools::tagList(
              htmltools::tags$p(
                "Editing data for: ",
                htmltools::strong(selected_property_val)
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

      # save
      shiny::observeEvent(input$save, {

        # enable validator
        iv$enable()

        shiny::req(selected_row(), input$model_beds, input$investment_partner)

        new_value_model_beds <- as.integer(input$model_beds)
        new_value_investment_partner <- input$investment_partner

        shiny::req(iv$is_valid())

        tryCatch({

          row_data <- summary_data() |>
            dplyr::filter(dplyr::row_number() == selected_row())

          prop_id <- row_data |> dplyr::pull("property_id") |> as.integer()
          orig_model_beds <- row_data |> dplyr::pull("model_beds") |> as.integer()

          if (new_value_model_beds != orig_model_beds) {
            db_update_gmh_pre_lease_model_beds(pool, prop_id, new_value_model_beds)
          } else {
            cli::cli_alert_info("No changes for model beds.")
          }

          partner_id <- db_read_tbl(pool, "gmh.partners") |>
            dplyr::filter(.data$partner_name == new_value_investment_partner) |>
            dplyr::pull("partner_id") |>
            as.integer()

          orig_parter_id <- row_data |> dplyr::pull("investment_partner") |> get_partner_id_by_name() |> as.integer()

          if (partner_id != orig_parter_id) {
            db_update_gmh_property_partner(pool, prop_id, partner_id)
          } else {
            cli::cli_alert_info("No changes for investment partner.")
          }

          shiny::showNotification("Database updated successfully!", type = "message")
          db_trigger(db_trigger() + 1)

        }, error = function(e) {
          cli::cli_alert_danger("Error updating database: {.error {e$message}}")
          shiny::showNotification("Error updating database!", type = "error")
        }, finally {
          shiny::removeModal()
          iv$disable()
        })
      })

      # return
      return(
        list(
          selected_property = selected_property,
          selected_row = selected_row
        )
      )
    }
  )

}

mod_pre_lease_details_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagsList(
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
  )

}

mod_pre_lease_details_server <- function(id, summary_data, details_data, details_by_property_data) {

  stopifnot(
    shiny::is.reactive(summary_data),
    shiny::is.reactive(details_data),
    shiny::is.reactive(details_by_property_data)
  )

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # setup
      ns <- session$ns
      cli::cat_rule("[Module]: mod_pre_lease_details_server()")

      # reactive values
      selected_property <- shiny::reactiveVal(NULL)
      selected_unit <- shiny::reactiveVal(NULL)

      # property level data
      property_data <- shiny::reactive({
        shiny::req(summary_data())
        summary_data()
      })

      # unit level data
      unit_data <- shiny::reactive({
        shiny::req(selected_property(), details_by_property_data())
        details_by_property_data() |>
          dplyr::filter(.data$property_name == .env$selected_property())
      })

      # detail level data
      detail_data <- shiny::reactive({
        shiny::req(selected_property(), selected_unit(), details_data())
        details_data() |>
          dplyr::filter(
            .data$property_name == .env$selected_property(),
            .data$unit_type == .env$selected_unit()
          )
      })

      # property info panel
      output$selected_property_info <- shiny::renderUI({
        shiny::req(selected_property(), property_data())

        prop_data <- property_data() |>
          dplyr::filter(.data$property_name == .env$selected_property())

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

      # unit info panel
      output$selected_unit_info <- shiny::renderUI({
        shiny::req(selected_property(), selected_unit(), unit_data())

        unit_info <- unit_data() |>
          dplyr::filter(.data$unit_type == .env$selected_unit())

        htmltools::tags$div(
          class = "alert alert-info m-3",
          htmltools::tags$div(
            class = "d-flex justify-content-between align-items-center",
            htmltools::tags$h4(class = "mb-0", paste("Unit Type:", selected_unit())),
            htmltools::tags$span(
              class = "badge bg-primary",
              sprintf("Available Units: %d", unit_info$available_units)
            )
          )
        )
      })

      # selection state outputs
      output$has_property_selection <- shiny::reactive({
        !is.null(selected_property()) && length(selected_property()) > 0
      })
      shiny::outputOptions(output, "has_property_selection", suspendWhenHidden = FALSE)

      output$has_unit_selection <- shiny::reactive({
        !is.null(selected_unit()) && length(selected_unit()) > 0
      })
      shiny::outputOptions(output, "has_unit_selection", suspendWhenHidden = FALSE)

      # property table
      output$property_table <- reactable::renderReactable({

        tbl_data <- property_data() |>
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

      # unit table
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

      # detail table
      output$detail_table <- reactable::renderReactable({
        shiny::req(detail_data())

        tbl_data <- detail_data() |>
          dplyr::arrange(
            .data$bldg_unit,
            .data$move_in_date
          ) |>
          dplyr::mutate(
            dplyr::across(
              tidyselect::where(is.numeric),
              function(x) {
                as.numeric(x) |> dplyr::coalesce(0.00)
              }
            ),
            dplyr::across(
              tidyselect::where(is.integer),
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

      # property table selection
      shiny::observe({
        hold <- reactable::getReactableState("property_table", "selected")
        if (is.null(hold) || length(hold) == 0) {
          selected_property(NULL)
        } else {
          selected_property(property_data()$property_name[hold])
        }
      })

      # unit table selection
      shiny::observe({
        hold <- reactable::getReactableState("unit_table", "selected")
        if (is.null(hold) || length(hold) == 0) {
          selected_unit(NULL)
        } else {
          selected_unit(unit_data()$unit_type[hold])
        }
      })

      # panel navigation
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

      # return
      return(
        list(
          selected_property = selected_property,
          selected_unit = selected_unit
        )
      )

    }
  )

}
