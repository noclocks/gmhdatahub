
#  ------------------------------------------------------------------------
#
# Title : Wizard Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-16
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Wizard Shiny Module
#'
#' @name mod_wizard
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Leasing Market Survey Wizard.
#'
#' Includes the following functions:
#'
#' - `mod_wizard_ui()`: User Interface (UI) for the module.
#' - `mod_wizard_server()`: Server logic for the module.
#' - `mod_wizard_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_wizard_ui()`: UI output
#' - `mod_wizard_server()`: List of reactive expressions.
#' - `mod_wizard_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_wizard_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_wizard
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_wizard_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$link(
        rel = "stylesheet",
        href = "www/modules/mod_wizard/styles.css"
      )
    ),
    htmltools::tags$div(
      id = ns("wizard"),
      bslib::card(
        bslib::card_header(
          htmltools::tags$div(
            class = "d-flex justify-content-between align-items-center position-relative mb-2",
            shiny::uiOutput(ns("progression"))
          ),
          htmltools::tags$div(
            shinyWidgets::progressBar(
              id = ns("wizard_progress"),
              value = 0,
              total = 100,
              title = "Progress",
              display_pct = TRUE
            )
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("header")),
          htmltools::tags$hr(),
          shiny::uiOutput(ns("body"))
        ),

        bslib::card_footer(
          htmltools::tags$div(
            class = "d-flex justify-content-between",
            shiny::actionButton(ns("prev_btn"), "Previous", class = "btn-secondary"),
            shiny::actionButton(ns("cancel_btn"), "Cancel", class = "btn-danger"),
            shiny::actionButton(ns("submit_btn"), "Submit", class = "btn-success") |> shinyjs::hidden(),
            shiny::actionButton(ns("next_btn"), "Next", class = "btn-primary")
          )
        )
      )
    )
  )
}

#' @rdname mod_wizard
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_wizard_server <- function(
  id,
  pool = NULL,
  selected_property_id = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool
      check_db_conn(pool)

      ns <- session$ns
      cli::cat_rule("[Module]: mod_wizard_server()")

      # initialize input validation
      iv <- shinyvalidate::InputValidator$new()
      iv <- set_validation_rules(iv, 1)

      # setup reactive values
      current_section <- shiny::reactiveVal(1)
      total_sections <- 12
      sections <- wizard_sections()
      wizard_data <- shiny::reactiveVal(list())

      # update wizard data function
      update_wizard_data <- function(new_data) {
        current_data <- wizard_data()
        updated_data <- modifyList(current_data, new_data)
        wizard_data(updated_data)
      }

      # progression timeline
      output$progression <- shiny::renderUI({
        render_progression(current_section(), sections, ns)
      })

      # header
      output$header <- shiny::renderUI({
        htmltools::tags$h5(names(sections)[current_section()])
      })

      # body
      output$body <- shiny::renderUI({
        render_wizard_body(current_section(), ns)
      })

      # progress bar
      shiny::observeEvent(current_section(), {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "wizard_progress",
          value = (current_section() / total_sections) * 100
        )
        iv <- set_validation_rules(iv, current_section())
      })

      # navigation logic
      shiny::observeEvent(input$next_btn, {
        iv$enable()
        if (iv$is_valid()) {
          section_data <- switch(
            current_section(),
            "1" = get_intro_data(input),
            "2" = get_property_summary_data(input),
            "3" = get_leasing_summary_data(input),
            "4" = get_short_term_leases_data(input)
          )
          update_wizard_data(section_data)
          current_section(current_section() + 1)
          iv$disable()
        } else {
          shiny::showNotification("Please fix the highlighted errors before proceeding.", type = "error")
        }
      })

      shiny::observeEvent(input$prev_btn, {
        if (current_section() > 1) {
          current_section(current_section() - 1)
        }
      })

      shiny::observeEvent(input$cancel_btn, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Cancel Survey",
            "Are you sure you want to cancel? All progress will be lost.",
            footer = shiny::tagList(
              shiny::modalButton("No"),
              shiny::actionButton(ns("confirm_cancel"), "Yes, Cancel", class = "btn-danger")
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_cancel, {
        shiny::removeModal()
        shiny::showNotification("Survey canceled.", type = "message")
      })

      shiny::observe({
        if (current_section() == 1) shinyjs::disable("prev_btn") else shinyjs::enable("prev_btn")
        if (current_section() == total_sections) {
          shinyjs::hide("next_btn")
          shinyjs::show("submit_btn")
        } else {
          shinyjs::show("next_btn")
          shinyjs::hide("submit_btn")
        }
      })

      shiny::observeEvent(input$submit_btn, {
        tryCatch({
          shiny::showModal(render_submission_modal(ns))
        }, error = function(e) {
          shiny::showNotification("Error during submission. Please try again.", type = "error")
        })
      })

      shiny::observeEvent(input$confirm_submit, {
        shiny::removeModal()
        # TODO: save data to database
        shiny::showNotification("Survey submitted successfully!", type = "message")
      })

      output$summary <- shiny::renderUI({
        survey <- survey_data()
        htmltools::tagList(
          shiny::h3("Summary of Responses"),
          htmltools::tags$ul(
            lapply(names(survey), function(section) {
              htmltools::tags$li(
                htmltools::tags$b(section), ": ",
                htmltools::tags$pre(jsonlite::toJSON(survey[[section]], pretty = TRUE))
              )
            })
          )
        )
      })

      survey_data <- shiny::reactive({
        list(
          intro = get_intro_data(input),
          property_summary = get_property_summary_data(input),
          leasing_summary = get_leasing_summary_data(input),
          short_term_leases = get_short_term_leases_data(input)
        )
      })

      return(list(survey_data = survey_data))
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_wizard
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_wizard_demo <- function() {

  pkgload::load_all()

  shiny::addResourcePath(
    "www",
    pkg_sys("www")
  )

  ui <- bslib::page_navbar(
    title = "Demo: Wizard",
    window_title = "Demo: Wizard",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Wizard",
      value = "wizard",
      icon = bsicons::bs_icon("house"),
      shinyjs::useShinyjs(),
      mod_wizard_ui("demo")
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    selected_property_id <- shiny::reactive({
      get_property_id_by_name("1047 Commonwealth Avenue")
    })
    mod_wizard_server("demo", pool = pool, selected_property_id = selected_property_id)
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------

wizard_sections <- function() {
  list(
    "Introduction" = 1,
    "Property Summary" = 2,
    "Leasing Summary" = 3,
    "Short Term Leases" = 4,
    "Fees" = 5,
    "Amenities" = 6,
    "Parking" = 7,
    "Utilities" = 8,
    "Office Hours" = 9,
    "Notes" = 10,
    "Rent Tables" = 11,
    "Finish" = 12
  )
}

render_wizard_body <- function(current_section, ns) {
  switch(
    current_section,
    "Introduction" = intro_ui(ns),
    "Property Summary" = property_summary_ui(ns),
    "Leasing Summary" = leasing_summary_ui(ns),
    "Short Term Leases" = short_term_leases_ui(ns)
  )
}

render_progression <- function(current_step, steps, ns) {
  timeline_items <- lapply(seq_along(steps), function(i) {
    name <- names(steps)[i]
    step_num <- steps[[i]]
    is_active <- step_num <= current_step
    is_current <- step_num == current_step

    # Step indicator (circle with number or icon)
    step_circle <- div(
      class = paste(
        "step-circle d-flex align-items-center justify-content-center",
        if (is_current) "current-step" else if (is_active) "active-step" else "inactive-step"
      ),
      step_num
    )

    # Label below the step
    step_label <- div(
      class = "step-label text-center",
      name
    )

    # Combine step and label
    div(
      class = "step-item",
      step_circle,
      step_label
    )
  })

  # Progress bar
  progress_percentage <- round((current_step / length(steps)) * 100, 2)
  progress_bar <- div(
    class = "progress-container",
    div(
      class = "progress-bar-active",
      style = paste0("width: ", progress_percentage, "%;")
    )
  )

  # Render timeline
  div(
    class = "timeline-container",
    progress_bar,
    div(
      class = "timeline-steps d-flex justify-content-between",
      timeline_items
    )
  )
}

render_submision_modal <- function(ns) {
  shiny::modalDialog(
    title = "Confirm Submission",
    "Are you sure you want to submit the survey?",
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns("confirm_submit"), "Submit", class = "btn-success")
    )
  )
}

set_validation_rules <- function(iv, section) {

  section <- as.character(section)

  switch(
    section,
    "1" = {
      iv_1 <- shinyvalidate::InputValidator$new()
      iv_1$add_rule("survey_property", shinyvalidate::sv_required(message = "Property is required."))
      iv_1$add_rule("survey_leasing_week", shinyvalidate::sv_required(message = "Leasing week is required."))
      iv_1$add_rule("survey_user", shinyvalidate::sv_required(message = "User email is required."))
      iv_1$add_rule("survey_user", shinyvalidate::sv_email(message = "Enter a valid email address."))
      iv$add_validator(iv_1)
      return(iv)
    },
    "2" = {
      input_ids <- property_summary_inputs_tbl$id
      required_inputs <- property_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(id)
      validation_msgs <- property_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(name) |>
        purrr::map_chr(~ paste0(.x, " is required."))
      iv_2 <- shinyvalidate::InputValidator$new()
      purrr::walk2(
        required_inputs,
        validation_msgs,
        function(input_id, validation_msg) {
          iv_2$add_rule(
            input_id,
            shinyvalidate::sv_required(message = validation_msg)
          )
        }
      )
      # add regex rules for phone and website
      phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"
      iv_2$add_rule("property_website", shinyvalidate::sv_url(message = "Please enter a valid URL for the property website."))
      iv_2$add_rule("property_phone_number", shinyvalidate::sv_regex(phone_regex, "Enter a valid phone number."))
      # add rules for distance to campus and property rating
      iv_2$add_rule("distance_to_campus", shinyvalidate::sv_gt(0, "Distance must be greater than 0."))
      iv_2$add_rule("property_rating", shinyvalidate::sv_gt(0, "Rating must be greater than 0."))
      iv$add_validator(iv_2)
      return(iv)
    },
    "3" = {
      input_ids <- leasing_summary_inputs_tbl$id
      required_inputs <- leasing_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(id)
      validation_msgs <- leasing_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(name) |>
        purrr::map_chr(~ paste0(.x, " is required."))
      iv_3 <- shinyvalidate::InputValidator$new()
      purrr::walk2(
        required_inputs,
        validation_msgs,
        function(input_id, validation_msg) {
          iv_3$add_rule(
            input_id,
            shinyvalidate::sv_required(message = validation_msg)
          )
        }
      )
      iv_3$add_rule("current_occupancy", shinyvalidate::sv_between(0, 1))
      iv_3$add_rule("last_year_occupancy", shinyvalidate::sv_between(0, 1))
      iv_3$add_rule("current_pre_lease", shinyvalidate::sv_between(0, 1))
      iv_3$add_rule("last_year_pre_lease", shinyvalidate::sv_between(0, 1))
      iv_3$add_rule("total_renewals", shinyvalidate::sv_integer())
      iv_3$add_rule("total_new_leases", shinyvalidate::sv_integer())
      iv_3$add_rule("total_leases_weekly", shinyvalidate::sv_integer())
      iv_3$add_rule("traffic_weekly", shinyvalidate::sv_integer())
      iv_3$add_rule("incentive_amount", shinyvalidate::sv_numeric())
      iv$add_validator(iv_3)
      return(iv)
    },
    "4" = {
      iv_4 <- shinyvalidate::InputValidator$new()
      iv_4$add_rule("five_month_premium", shinyvalidate::sv_gt(0, "Premium must be greater than 0."))
      iv_4$add_rule("ten_month_premium", shinyvalidate::sv_gt(0, "Premium must be greater than 0."))
      iv_4$add_rule("five_month_quantity", shinyvalidate::sv_integer())
      iv_4$add_rule("ten_month_quantity", shinyvalidate::sv_integer())
      iv$add_validator(iv_4)
      return(iv)
    }
  )

}



intro_ui <- function(ns) {
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
      value = ""
    )
  )
}

property_summary_ui <- function(ns) {

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_body(
          shiny::textInput(ns("property_name"), "Property Name"),
          shiny::textInput(ns("property_website"), "Website URL"),
          shiny::textInput(ns("property_address"), "Address"),
          shiny::textInput(ns("property_phone"), "Phone Number"),
          shiny::radioButtons(ns("image_type"), "Image Input Type", choices = c("URL" = "url", "File Upload" = "file"), selected = "url"),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'url'", ns("image_input_type")),
            shiny::textInput(ns("property_image_url"), "Image URL")
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'file'", ns("image_input_type")),
            shiny::fileInput(ns("property_image_file"), "Upload Image", accept = c('image/png', 'image/jpeg', 'image/gif'))
          )
        )
      ),
      bslib::card(
        bslib::card_body(
          shiny::selectInput(
            ns("property_type"),
            "Property Type",
            choices = get_survey_choices("property_summary", "property_type")
          ),
          shiny::sliderInput(
            ns("property_rating"),
            "Rating",
            min = 0,
            max = 5,
            value = 0,
            step = 0.5
          ),
          shiny::selectInput(
            ns("property_status"),
            "Status",
            choices = get_survey_choices("property_summary", "property_status")
          ),
          shiny::numericInput(
            ns("year_built"),
            "Year Built",
            min = 1900,
            max = lubridate::year(Sys.Date()),
            value = 1990
          ),
          shiny::sliderInput(
            ns("distance_to_campus"),
            "Distance (miles)",
            min = 0,
            max = 10,
            step = 0.1,
            value = 0
          )
        )
      )
    )
  )
}

leasing_summary_ui <- function(ns) {

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        shiny::selectInput(
          ns("reporting_cycle"),
          "Reporting Cycle",
          choices = get_survey_choices(section = "leasing_summary", type = "reporting_cycle")
        ),
        shiny::dateInput(
          ns("lease_launch_date"),
          "Lease Launch Date"
        ),
        shiny::dateInput(
          ns("renewal_launch_date"),
          "Renewal Launch Date"
        ),
        shiny::sliderInput(
          ns("current_occupancy"),
          "Current Occupancy %",
          min = 0,
          max = 1,
          value = 0.95,
          step = 0.01,
          ticks = TRUE,
          post = "%"
        ),
        shiny::sliderInput(
          ns("prior_year_occupancy"),
          "Prior Year Occupancy %",
          min = 0,
          max = 1,
          value = 0.95,
          step = 0.01,
          ticks = TRUE,
          post = "%"
        ),
        shiny::sliderInput(
          ns("current_pre_lease"),
          "Current Pre-Lease %",
          min = 0,
          max = 1,
          value = 0.95,
          step = 0.01,
          ticks = TRUE,
          post = "%"
        ),
        shiny::sliderInput(
          ns("last_year_pre_lease"),
          "Prior Year Pre-Lease %",
          min = 0,
          max = 1,
          value = 0.95,
          step = 0.01,
          ticks = TRUE,
          post = "%"
        )
      ),
      bslib::card(
        shiny::numericInput(
          ns("total_renewals"),
          "Total Renewals",
          value = 0,
          min = 0,
          step = 1
        ),
        shiny::numericInput(
          ns("total_new_leases"),
          "Total New Leases",
          value = 0,
          min = 0,
          step = 1
        ),
        shiny::numericInput(
          ns("weekly_leases"),
          "Weekly Leases",
          value = 0,
          min = 0,
          step = 1
        ),
        shiny::numericInput(
          ns("weekly_traffic"),
          "Weekly Traffic",
          value = 0,
          min = 0,
          step = 1
        ),
        shiny::selectInput(
          ns("current_incentive"),
          "Current Incentive",
          choices = get_survey_choices(section = "leasing_summary", type = "current_incentive")
        ),
        shiny::numericInput(
          ns("incentive_amount"),
          "Incentive Amount",
          value = 0,
          min = 0,
          step = 1
        ) |> shinyjs::disabled()
      )
    )
  )

}

short_term_leases_ui <- function(ns) {
  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("5 Month Term"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::checkboxInput(
              ns("five_month_available"),
              label = "5 Month Term Available",
              value = FALSE
            ),
            shiny::numericInput(
              ns("five_month_premium"),
              label = "5 Month Term Premium",
              value = 0,
              min = 0,
              max = Inf,
              step = 10
            ) |> shinyjs::disaled(),
            shiny::numericInput(
              ns("five_month_quantity"),
              label = "5 Month Term Quantity",
              value = 0,
              min = 0,
              max = Inf,
              step = 1
            ) |> shinyjs::disaled()
          )
        )
      ),
      bslib::card(
        bslib::card_header("10 Month Term"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::checkboxInput(
              ns("ten_month_available"),
              label = "10 Month Term Available",
              value = FALSE
            ),
            shiny::numericInput(
              ns("ten_month_premium"),
              label = "10 Month Term Premium",
              value = 0,
              min = 0,
              max = Inf,
              step = 10
            ) |> shinyjs::disaled(),
            shiny::numericInput(
              ns("ten_month_quantity"),
              label = "10 Month Term Quantity",
              value = 0,
              min = 0,
              max = Inf,
              step = 1
            ) |> shinyjs::disaled()
          )
        )
      )
    )
  )
}


get_intro_data <- function(input) {
  list(
    property = input$survey_property,
    use_current_leasing_week = input$use_current_leasing_week,
    leasing_week = input$survey_leasing_week,
    user = input$survey_user
  )
}

get_property_summary_data <- function(input) {
  list(
    property_name = input$property_name,
    property_website = input$property_website,
    property_address = input$property_address,
    property_phone = input$property_phone,
    property_image = switch(
      input$image_type,
      "url" = input$property_image_url,
      "file" = input$property_image_file$datapath
    ),
    property_type = input$property_type,
    property_rating = input$property_rating,
    property_status = input$property_status,
    year_built = input$year_built,
    distance_to_campus = input$distance_to_campus
  )
}

get_leasing_summary_data <- function(input) {
  list(
    reporting_cycle = input$reporting_cycle,
    lease_launch_date = input$lease_launch_date,
    renewal_launch_date = input$renewal_launch_date,
    current_occupancy = input$current_occupancy,
    prior_year_occupancy = input$prior_year_occupancy,
    current_pre_lease = input$current_pre_lease,
    last_year_pre_lease = input$last_year_pre_lease,
    total_renewals = input$total_renewals,
    total_new_leases = input$total_new_leases,
    weekly_leases = input$weekly_leases,
    weekly_traffic = input$weekly_traffic,
    current_incentive = input$current_incentive,
    incentive_amount = input$incentive_amount,
    data_last_updated = input$data_last_updated
  )
}

get_short_term_leases_data <- function(input) {
  list(
    five_month_available = input$five_month_available,
    five_month_premium = input$five_month_premium,
    five_month_quantity = input$five_month_quantity,
    ten_month_available = input$ten_month_available,
    ten_month_premium = input$ten_month_premium,
    ten_month_quantity = input$ten_month_quantity
  )
}
