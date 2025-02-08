#  ------------------------------------------------------------------------
#
# Title : Survey Utilities Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Utilities Shiny Module
#'
#' @name mod_survey_utilities
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Utilities page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_utilities_ui()`: User Interface (UI) for the module.
#' - `mod_survey_utilities_server()`: Server logic for the module.
#' - `mod_survey_utilities_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_utilities_ui()`: UI output
#' - `mod_survey_utilities_server()`: List of reactive expressions.
#' - `mod_survey_utilities_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_utilities_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_utilities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_utilities_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("plug"),
            htmltools::tags$span(
              "Utilities - ",
              shiny::textOutput(ns("property_name_title"), inline = TRUE)
            ),
            shiny::actionButton(
              ns("refresh"),
              "Refresh Data",
              icon = shiny::icon("sync"),
              class = "btn-sm btn-outline-light float-end",
              style = "width: auto;"
            )
          )
        ),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(12),
            bslib::card(
              bslib::card_header(
                "Core Utilities",
                class = "bg-primary text-white"
              ),
              reactable::reactableOutput(ns("survey_core_utilities_tbl"))
            ),
            bslib::card(
              bslib::card_header(
                "Other Utilities",
                class = "bg-primary text-white"
              ),
              reactable::reactableOutput(ns("survey_other_utilities_tbl"))
            )
          )
        ),
        bslib::card_footer(
          class = "text-muted d-flex justify-content-between align-items-center",
          htmltools::tags$small(
            "Last Updated:",
            htmltools::tags$span(
              class = "fw-bold",
              shiny::textOutput(ns("last_updated_at"), inline = TRUE)
            )
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_utilities
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_utilities_server <- function(
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
      cli::cat_rule("[Module]: mod_survey_utilities_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      # filters
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------
      utilities_data <- shiny::reactive({
        shiny::req(survey_data$utilities)

        if (nrow(survey_data$utilities) == 0) {
          out <- default_tbl_survey_utilities()
          shiny::showNotification(
            paste0(
              "No data available for selected property/competitor.",
              " Using default data for demonstration purposes."
            )
          )
        } else {
          out <- survey_data$utilities |>
            dplyr::select(
              utility_name,
              utility_included,
              utility_available,
              utility_capped,
              utility_per,
              utility_allowance,
              utility_category
            )
        }

        out
      })

      core_utilities_data <- shiny::reactive({
        shiny::req(utilities_data())
        utilities_data() |>
          dplyr::filter(.data$utility_category == "Core") |>
          dplyr::select(-utility_category)
      })

      other_utilities_data <- shiny::reactive({
        shiny::req(utilities_data())
        utilities_data() |>
          dplyr::filter(utility_category == "Other") |>
          dplyr::select(-utility_category)
      })

      # outputs -----------------------------------------------------------------
      output$property_name_title <- shiny::renderText({
        shiny::req(selected_filters)
        prop_id <- selected_filters$property_id
        comp_id <- selected_filters$competitor_id
        prop_name <- selected_filters$property_name %||% get_property_name_by_id(prop_id)
        if (is.null(comp_id) || is.na(comp_id)) {
          paste0(prop_name, "(", prop_id, ")")
        } else {
          paste0(prop_name, "(Competitor #", comp_id, ")")
        }
      })

      output$last_updated_at <- shiny::renderText({
        shiny::req(survey_data$utilities)
        survey_data$utilities |>
          dplyr::pull("updated_at") |>
          max(na.rm = TRUE) |>
          format("%Y-%m-%d %H:%M:%S")
      })

      # tables ------------------------------------------------------------------
      output$survey_core_utilities_tbl <- reactable::renderReactable({
        shiny::req(core_utilities_data())
        tbl_survey_utilities(core_utilities_data())
      })

      output$survey_other_utilities_tbl <- reactable::renderReactable({
        shiny::req(other_utilities_data())
        tbl_survey_utilities(other_utilities_data())
      })

      # edit --------------------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab())

        if (session$userData$selected_survey_tab() != "nav_utilities") {
          return()
        }

        iv$initialize()
        iv$enable()

        # merge data
        core_data <- core_utilities_data()
        other_data <- other_utilities_data()
        merged_data <- dplyr::bind_rows(core_data, other_data)

        inputs <- lapply(1:nrow(merged_data), function(i) {
          utility <- merged_data[i, ]
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              utility$utility_name,
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(
                  shiny::radioButtons(
                    ns(glue::glue("edit_{utility$utility_name}_per")),
                    label = "Per Bed/Unit",
                    choices = c("Bed", "Unit"),
                    selected = utility$utility_per
                  ),
                  shiny::numericInput(
                    ns(glue::glue("edit_{utility$utility_name}_allowance")),
                    label = "Allowance ($)",
                    value = utility$utility_allowance
                  )
                ),
                bslib::card(
                  shiny::checkboxInput(
                    ns(glue::glue("edit_{utility$utility_name}_included")),
                    label = "Included?",
                    value = utility$utility_included
                  ),
                  shiny::checkboxInput(
                    ns(glue::glue("edit_{utility$utility_name}_available")),
                    label = "Available?",
                    value = utility$utility_available
                  ),
                  shiny::checkboxInput(
                    ns(glue::glue("edit_{utility$utility_name}_capped")),
                    label = "Capped?",
                    value = utility$utility_capped
                  )
                )
              )
            )
          )
        })

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Utilities",
            size = "l",
            easyClose = TRUE,
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            ),
            do.call(htmltools::tagList, inputs)
          )
        )

      })

      # inputs ------------------------------------------------------------------
      input_data <- shiny::reactive({
        merged_data <- dplyr::bind_rows(
          core_utilities_data() |> dplyr::mutate(utility_category = "Core"),
          other_utilities_data() |> dplyr::mutate(utility_category = "Other")
        )
        # input values
        purrr::map_dfr(
          1:nrow(merged_data),
          function(i) {
            utility <- merged_data[i, ]
            list(
              utility_name = utility$utility_name,
              utility_category = utility$utility_category,
              utility_per = input[[glue::glue("edit_{utility$utility_name}_per")]],
              utility_allowance = input[[glue::glue("edit_{utility$utility_name}_allowance")]],
              utility_included = input[[glue::glue("edit_{utility$utility_name}_included")]],
              utility_available = input[[glue::glue("edit_{utility$utility_name}_available")]],
              utility_capped = input[[glue::glue("edit_{utility$utility_name}_capped")]]
            )
          }
        )
      })

      # changes -----------------------------------------------------------------
      changed_data <- shiny::reactive({
        dplyr::anti_join(
          input_data(),
          survey_data$utilities,
          by = c(
            "utility_name",
            "utility_category",
            "utility_per",
            "utility_allowance",
            "utility_included",
            "utility_available",
            "utility_capped"
          )
        )
      })

      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NA_integer_
          comp_id <- selected_filters$competitor_id
          prop_name <- selected_filters$competitor_name %||% get_competitor_name_by_id(comp_id)
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NA_integer_
          prop_name <- selected_filters$property_name %||% get_property_name_by_id(prop_id)
        }

        user_id <- selected_filters$user_id

        changed_values <- changed_data() |>
          dplyr::mutate(
            property_id = prop_id,
            competitor_id = comp_id,
            property_name = prop_name,
            updated_by = user_id
          ) |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            utility_name,
            utility_category,
            utility_per,
            utility_allowance,
            utility_included,
            utility_available,
            utility_capped,
            updated_by
          )

        if (nrow(changed_values) == 0) {
          shiny::showNotification("No changes detected.")
          shiny::removeModal()
          return()
        } else {
          shiny::withProgress(
            message = "Saving changes...",
            detail = "Please wait...",
            value = 0,
            {
              db_update_survey_utilities(pool, changed_values)
              shiny::setProgress(value = 1, detail = "Changes saved.")
              db_trigger_func()
              shiny::removeModal()
            }
          )
        }
      })

      return(
        list(
          utilities_data = utilities_data
        )
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_utilities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_utilities_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Utilities",
    window_title = "Demo: Survey Utilities",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Utilities",
      value = "survey_utilities",
      icon = bsicons::bs_icon("house"),
      shiny::actionButton(
        "edit_survey_section",
        "Edit",
        icon = shiny::icon("edit"),
        style = "width: auto;",
        class = "btn-sm btn-primary"
      ),
      mod_survey_utilities_ui("demo")
    )
  )

  server <- function(input, output, session) {

    if (is.null(pool)) pool <- db_connect()

    edit_survey_section <- shiny::reactive({
      input$edit_survey_section
    })

    selected_filters <- shiny::reactiveValues(
      property_id = 739085,
      competitor_id = NULL,
      survey_id = NULL,
      user_id = get_user_id_by_email(pool, "default_user@example.com")
    )

    survey_data <- shiny::reactiveValues(
      utilities = db_read_survey_utilities(
        pool,
        property_id = shiny::isolate(selected_filters$property_id)
      )
    )

    session$userData$selected_survey_tab <- shiny::reactiveVal("nav_utilities")

    db_trigger_func <- function() {
      survey_data$property_summary <- db_read_survey_utilities(
        pool,
        property_id = shiny::isolate(selected_filters$property_id)
      )
    }

    mod_survey_utilities_server(
      "demo",
      pool = pool,
      survey_data = survey_data,
      selected_filters = selected_filters,
      db_trigger_func = db_trigger_func,
      edit_survey_section = edit_survey_section
    )
  }

  shiny::shinyApp(ui, server)
}
