#  ------------------------------------------------------------------------
#
# Title : Survey Amenities Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Amenities Shiny Module
#'
#' @name mod_survey_property_amenities
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Amenities page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_property_amenities_ui()`: User Interface (UI) for the module.
#' - `mod_survey_property_amenities_server()`: Server logic for the module.
#' - `mod_survey_property_amenities_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_property_amenities_ui()`: UI output
#' - `mod_survey_property_amenities_server()`: List of reactive expressions.
#' - `mod_survey_property_amenities_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_property_amenities_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_property_amenities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_property_amenities_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      reactable::reactableOutput(ns('survey_property_amenities_tbl'))
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_property_amenities
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_property_amenities_server <- function(
    id,
    pool = NULL,
    global_filters = NULL,
    selected_property_id = NULL,
    edit_survey_section = NULL
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
      cli::cat_rule("[Module]: mod_survey_property_amenities_server()")

      # handle selected property ID
      if (is.null(selected_property_id)) {
        # property_id <- db_read_tbl(pool, "mkt.properties", collect = FALSE) |>
        #   dplyr::filter(.data$is_competitor == FALSE) |>
        #   dplyr::pull("property_id")
        selected_property_id <- shiny::reactive({
          # property_id
          session$userData$selected_survey_property()
        })
      }

      db_refresh_trigger <- shiny::reactiveVal(0)

      property_amentities_data <- shiny::reactive({
        shiny::req(pool, selected_property_id(), session$userData$leasing_week())

        # browser()

        db_read_mkt_property_amenities(
          pool,
          property_id = selected_property_id(),
          leasing_week = session$userData$leasing_week()
        )
      })

      output$survey_property_amenities_tbl <- reactable::renderReactable({
        out <- property_amentities_data() |>
          janitor::clean_names(
            case = 'title',
            use_make_names = FALSE
          )

        if (nrow(out) == 0) {
          out <- tibble::tribble(
            ~'Property Amenities', ~'values',
            'Common Area Rating', 'N/A',
            'University Shuttle', 'No',
            'Private Shuttle', 'No',
            'Limited Access Gates', 'No',
            'Fitness Center', 'No',
            'Computer Lounge', 'No',
            'Game Room', 'No',
            'Spray Tanning', 'No',
            'UV Tanning', 'No',
            'Pool', 'No',
            'Hot Tub', 'No',
            '24hr Package System', 'No',
            'EV Charging Stations', 'No',
            'Car Sharing Services', 'No',
            'Smart Vending', 'No',
            'Mini Market', 'No',
            'Movie Theatre', 'No',
            'Co-Working/Study Spaces', 'No',
            'Free Printing', 'No',
            'Coffee Bar', 'No',
            'Retail', 'No',
            'Sauna/Spa', 'No',
            'Cycling/Yoga Studio', 'No',
            'Rentable Guest Suite', 'No',
            'Wellness Classes', 'No',
            '24hr Concierge', 'No',
            'Outdoor Grill Area', 'No',
            'Sand Volleyball Court', 'No',
            'Basketball Court', 'No',
            'Pets Allowed', 'No',
            'Dog Wash', 'No',
            'Dog Park', 'No'
          )
        }

        # browser()

        reactable::reactable(
          data = out,
          defaultPageSize = nrow(out),
          columns = list(
            'Property Amenities' = reactable::colDef(
              name = "Property Amenities"#,
              # align = "center"
            ),
            values = reactable::colDef(
              name = NA_character_,
              sortable = FALSE
              # align = "center"
            )#,
            # .default = reactable::colDef(
            #   align = "center"
            # )
          ),
          # bordered = TRUE,
          striped = TRUE,
          highlight = TRUE
        )
      })

      # Edit ####
      shiny::observeEvent(edit_survey_section(), {
        if (session$userData$selected_survey_tab() != "nav_property_amenities") {
          return()
        }

        # data <- fees_data()

        # TODO:
        #   `iv` (?)

        shiny::showModal(
          shiny::modalDialog(
            title = "Property Amenities",
            size = "m",
            style = "display: inline-flex; flex-direction: column; align-items: center;",
            rhandsontable::rHandsontableOutput(ns("modal_property_amenities_table")),
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save_changes"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      output$modal_property_amenities_table <- rhandsontable::renderRHandsontable({
        data <- property_amentities_data()

        # browser()

        if (nrow(data) == 0) {
          data <- tibble::tribble(
            ~'Property Amenities', ~'values',
            'Common Area Rating', 'N/A',
            'University Shuttle', 'No',
            'Private Shuttle', 'No',
            'Limited Access Gates', 'No',
            'Fitness Center', 'No',
            'Computer Lounge', 'No',
            'Game Room', 'No',
            'Spray Tanning', 'No',
            'UV Tanning', 'No',
            'Pool', 'No',
            'Hot Tub', 'No',
            '24hr Package System', 'No',
            'EV Charging Stations', 'No',
            'Car Sharing Services', 'No',
            'Smart Vending', 'No',
            'Mini Market', 'No',
            'Movie Theatre', 'No',
            'Co-Working/Study Spaces', 'No',
            'Free Printing', 'No',
            'Coffee Bar', 'No',
            'Retail', 'No',
            'Sauna/Spa', 'No',
            'Cycling/Yoga Studio', 'No',
            'Rentable Guest Suite', 'No',
            'Wellness Classes', 'No',
            '24hr Concierge', 'No',
            'Outdoor Grill Area', 'No',
            'Sand Volleyball Court', 'No',
            'Basketball Court', 'No',
            'Pets Allowed', 'No',
            'Dog Wash', 'No',
            'Dog Park', 'No'
          )
        }

        rhandsontable::rhandsontable(
          data = data,
          contextMenu = FALSE,
          rowHeaders = NULL,
          colHeaders = c('Property Amenities', ' '),
          width = 325,
          # height = height
        ) |>
          rhandsontable::hot_cols(
            colWidths = c(250, 75)
          ) |>
          rhandsontable::hot_col(
            col = 1,
            readOnly = TRUE,
            renderer = "
              function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);

                // Set text to black
                td.style.color = 'black';
              }
          ") |>
          rhandsontable::hot_col(
            col = 2,
            type = 'autocomplete',
            source = c('Yes', 'No'),
            renderer = "
              function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.TextRenderer.apply(this, arguments);

                if (col === 0) {
                  // Don't allow editing
                  cellProperties.readOnly = true;
                } else if (col === 1) {
                  cellProperties.type = 'dropdown';
                  if (row === 0) {
                    cellProperties.source = ['1', '2', '3', '4', '5', 'N/A'];
                  }
                }
              }"
          )
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

#' @rdname mod_survey_property_amenities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_property_amenities_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Amenities",
    window_title = "Demo: Survey Amenities",
    theme = app_theme(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Amenities",
      value = "survey_property_amenities",
      icon = bsicons::bs_icon("house"),
      mod_survey_property_amenities_ui("demo")
    )
  )

  server <- function(input, output, session) {
    mod_survey_property_amenities_server("demo")
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
