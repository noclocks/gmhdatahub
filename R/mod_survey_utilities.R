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
    bslib::card(
      reactable::reactableOutput(ns("survey_core_utilities_tbl")),
      reactable::reactableOutput(ns("survey_other_utilities_tbl"))
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
        if (is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------
      utilities_data <- shiny::reactive({
        shiny::req(survey_data$utilities)
        survey_data$utilities |>
          dplyr::select(
            utility_name,
            utility_included,
            utility_available,
            utility_capped,
            utility_per,
            utility_allowance,
            utility_category
          )
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

      output$survey_core_utilities_tbl <- reactable::renderReactable({
        shiny::req(core_utilities_data())

        tbl_data <- core_utilities_data()

        if (nrow(tbl_data) == 0) {
          tbl_data <- tibble::tibble(
            utility_name = NA_character_,
            utility_included = as.logical(NA),
            utility_available = as.logical(NA),
            utility_capped = as.logical(NA),
            utility_per = NA_character_,
            utility_allowance = NA_real_
          )
        }

        reactable::reactable(
          data = tbl_data,
          defaultPageSize = nrow(tbl_data),
          searchable = TRUE,
          highlight = TRUE,
          columns = list(
            utility_name = reactable::colDef(
              name = "Utility",
              cell = reactablefmtr::pill_buttons(data = tbl_data)
            ),
            utility_included = reactable::colDef(
              name = "Included?",
              cell = function(value) format_boolean(value)
            ),
            utility_available = reactable::colDef(
              name = "Available?",
              cell = function(value) format_boolean(value)
            ),
            utility_capped = reactable::colDef(
              name = "Capped?",
              cell = function(value) format_boolean(value)
            ),
            utility_per = reactable::colDef(
              name = "Per Bed/Unit",
              cell = reactablefmtr::pill_buttons(data = tbl_data)
            ),
            utility_allowance = reactable::colDef(
              name = "Allowance ($)",
              format = reactable::colFormat(currency = "USD")
            )
          )
        )
      })

      output$survey_other_utilities_tbl <- reactable::renderReactable({
        shiny::req(other_utilities_data())

        tbl_data <- other_utilities_data()

        if (nrow(tbl_data) == 0) {
          tbl_data <- tibble::tibble(
            utility_name = NA_character_,
            utility_included = as.logical(NA),
            utility_available = as.logical(NA),
            utility_capped = as.logical(NA),
            utility_per = NA_character_,
            utility_allowance = NA_real_
          )
        }

        reactable::reactable(
          data = tbl_data,
          defaultPageSize = nrow(tbl_data),
          searchable = TRUE,
          highlight = TRUE,
          columns = list(
            utility_name = reactable::colDef(
              name = "Utility",
              cell = reactablefmtr::pill_buttons(data = tbl_data)
            ),
            utility_included = reactable::colDef(
              name = "Included?",
              cell = function(value) format_boolean(value)
            ),
            utility_available = reactable::colDef(
              name = "Available?",
              cell = function(value) format_boolean(value)
            ),
            utility_capped = reactable::colDef(
              name = "Capped?",
              cell = function(value) format_boolean(value)
            ),
            utility_per = reactable::colDef(
              name = "Per Bed/Unit",
              cell = reactablefmtr::pill_buttons(data = tbl_data)
            ),
            utility_allowance = reactable::colDef(
              name = "Allowance ($)",
              format = reactable::colFormat(currency = "USD")
            )
          )
        )
      })

      output$modal_survey_core_utilities_table <- rhandsontable::renderRHandsontable({
        shiny::req(core_utilities_data())

        core_data <- core_utilities_data()

        rhandsontable::rhandsontable(
          core_data,
          rowHeaders = FALSE,
          colHeaders = c(
            "Utility Name",
            "Included?",
            "Available?",
            "Capped?",
            "Per Bed/Unit",
            "Allowance ($)"
          ),
          contextMenu = TRUE,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_col(col = 1, readOnly = TRUE) |>
          rhandsontable::hot_col(col = 2, type = "checkbox") |>
          rhandsontable::hot_col(col = 3, type = "checkbox") |>
          rhandsontable::hot_col(col = 4, type = "checkbox") |>
          rhandsontable::hot_col(col = 5, type = "dropdown", source = c("Bed", "Unit")) |>
          rhandsontable::hot_col(col = 6, type = "numeric")
      })

      output$modal_survey_other_utilities_table <- rhandsontable::renderRHandsontable({
        shiny::req(other_utilities_data())

        other_data <- other_utilities_data()

        rhandsontable::rhandsontable(
          other_data,
          rowHeaders = FALSE,
          colHeaders = c(
            "Utility Name",
            "Included?",
            "Available?",
            "Capped?",
            "Per Bed/Unit",
            "Allowance ($)"
          ),
          contextMenu = TRUE,
          stretchH = "all",
          width = "100%"
        ) |>
          rhandsontable::hot_col(col = 1, readOnly = TRUE) |>
          rhandsontable::hot_col(col = 2, type = "checkbox") |>
          rhandsontable::hot_col(col = 3, type = "checkbox") |>
          rhandsontable::hot_col(col = 4, type = "checkbox") |>
          rhandsontable::hot_col(col = 5, type = "dropdown", source = c("Bed", "Unit")) |>
          rhandsontable::hot_col(col = 6, type = "numeric")
      })

      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab(), utilities_data())

        if (session$userData$selected_survey_tab() != "nav_utilities") {
          return()
        }

        core_data <- utilities_data() |> dplyr::filter(.data$utility_category == "Core")
        other_data <- utilities_data() |> dplyr::filter(.data$utility_category == "Other")

        iv$initialize()
        iv$enable()

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
            rhandsontable::rHandsontableOutput(ns("modal_survey_core_utilities_table")),
            rhandsontable::rHandsontableOutput(ns("modal_survey_other_utilities_table"))
          )
        )
      })



      shiny::observeEvent(input$save, {

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NA_integer_
          comp_id <- selected_filters$competitor_id
          prop_name <- selected_filters$competitor_name
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NA_integer_
          prop_name <- selected_filters$property_name
        }

        initial_values <- survey_data$utilities |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            utility_name,
            utility_category,
            utility_per,
            utility_available,
            utility_included,
            utility_capped,
            utility_allowance,
            updated_by
          )

        new_values <- rhandsontable::hot_to_r(input$modal_survey_core_utilities_table) |>
          dplyr::mutate(
            utility_category = "Core",
            # convert checkboxes to logical
            utility_available = as.logical(utility_available),
            utility_included = as.logical(utility_included),
            utility_capped = as.logical(utility_capped)
          ) |>
          dplyr::bind_rows(
            rhandsontable::hot_to_r(input$modal_survey_other_utilities_table) |>
              dplyr::mutate(
                utility_category = "Other",
                # convert checkboxes to logical
                utility_available = as.logical(utility_available),
                utility_included = as.logical(utility_included),
                utility_capped = as.logical(utility_capped)
              )
          ) |>
          dplyr::mutate(
            property_id = as.integer(prop_id),
            competitor_id = as.integer(comp_id),
            property_name = prop_name,
            updated_by = selected_filters$user_id
          ) |>
          dplyr::select(
            property_id,
            competitor_id,
            property_name,
            utility_name,
            utility_category,
            utility_per,
            utility_available,
            utility_included,
            utility_capped,
            utility_allowance,
            updated_by
          )

        changed_values <- dplyr::anti_join(
          new_values,
          initial_values,
          by = c(
            "property_id",
            "competitor_id",
            "property_name",
            "utility_name",
            "utility_category",
            "utility_per",
            "utility_available",
            "utility_included",
            "utility_capped",
            "utility_allowance",
            "updated_by"
          )
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
      mod_survey_utilities_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_utilities_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
