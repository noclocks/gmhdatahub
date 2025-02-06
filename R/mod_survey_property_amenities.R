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
    bslib::page_fluid(
      bslib::card(
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h2(
            bsicons::bs_icon("house"),
            "Property Amenities",
            class = "mb-2"
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("property_amenities_summary"))
        ),
        bslib::card_footer(
          shiny::textOutput(ns("last_updated"))
        )
      )
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
    survey_data = NULL,
    selected_filters = NULL,
    edit_survey_section = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_property_amenities_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      # filters
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
        if (is.null(selected_filters$property_name)) {
          selected_filters$property_name <- "1047 Commonwealth Avenue"
        }
      })

      selected_property_name <- shiny::reactive({
        shiny::req(selected_filters)
        selected_filters$property_name
      })

      property_amentities_data <- shiny::reactive({
        shiny::req(pool, selected_property_name())

        db_read_tbl(pool, "survey.property_amenities") |>
          dplyr::filter(.data$property_name == selected_property_name()) |>
          dplyr::select(
            property_name,
            amenity_name,
            amenity_value
          )
      })

      # Reactive initialization -----------------------------------------------
      initial_data <- shiny::reactiveVal()
      input_changes <- shiny::reactiveVal(0)
      db_refresh_trigger <- shiny::reactiveVal(0)

      # Data handling ---------------------------------------------------------
      property_amenities_data <- shiny::reactive({
        shiny::req(pool, selected_property_name())
        db_read_tbl(pool, "survey.property_amenities") |>
          dplyr::filter(.data$property_name == selected_property_name()) |>
          dplyr::select("property_name", "amenity_name", "amenity_value")
      })

      input_data <- shiny::reactive({
        input_changes() # Trigger on input changes
        amenities <- property_amenities$amenity

        purrr::map_df(amenities, ~ {
          tibble::tibble(
            amenity_name = .x,
            amenity_value = input[[.x]] %||% FALSE
          )
        }) |>
          dplyr::mutate(property_name = selected_property_name())
      })

      # Changes tracking ------------------------------------------------------
      changes <- shiny::reactive({
        shiny::req(initial_data(), input_data())

        initial <- initial_data() |>
          dplyr::select(amenity_name, initial = amenity_value)

        current <- input_data() |>
          dplyr::select(amenity_name, current = amenity_value)

        dplyr::full_join(initial, current, by = "amenity_name") |>
          dplyr::mutate(
            old = dplyr::if_else(initial, "Yes", "No"),
            new = dplyr::if_else(current, "Yes", "No"),
            changed = initial != current
          ) |>
          dplyr::filter(changed) |>
          dplyr::select(-changed)
      })

      # Modal handling --------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab(), property_amenities_data())

        if (session$userData$selected_survey_tab() != "nav_property_amenities") {
          return()
        }

        data <- property_amenities_data()
        initial_data(data)

        # Create input controls
        property_amenities_inputs <- lapply(
          unique(property_amenities$category),
          function(category) {
            bslib::accordion_panel(
              title = category,
              icon = amenity_section_icons |>
                dplyr::filter(category == !!category) |>
                dplyr::pull(icon) |>
                bsicons::bs_icon(),
              bslib::layout_column_wrap(
                style = "padding: 1.5rem; gap: 1rem;",
                purrr::map(
                  property_amenities |>
                    dplyr::filter(category == !!category) |>
                    dplyr::pull(amenity),
                  function(amenity) {
                    current_val <- data |>
                      dplyr::filter(amenity_name == amenity) |>
                      dplyr::pull(amenity_value)

                    htmltools::tags$div(
                      class = "d-flex align-items-center bg-light p-2 rounded",
                      bslib::input_switch(
                        id = ns(amenity),
                        label = htmltools::tagList(
                          bsicons::bs_icon(
                            property_amenities |>
                              dplyr::filter(amenity == !!amenity) |>
                              dplyr::pull(icon)
                          ),
                          amenity
                        ),
                        value = current_val
                      )
                    )
                  }
                )
              )
            )
          }
        )

        # Input change observer
        lapply(
          property_amenities$amenity,
          function(amenity) {
            shiny::observeEvent(input[[amenity]],
              {
                input_changes(input_changes() + 1)
              },
              ignoreInit = TRUE
            )
          }
        )

        # Show modal dialog
        shiny::showModal(
          shiny::modalDialog(
            title = icon_text("pencil", "Edit Property Amenities"),
            size = "l",
            htmltools::tags$div(
              htmltools::tags$h5("Pending Changes", class = "text-primary"),
              shiny::tableOutput(ns("changes_preview"))
            ),
            htmltools::tags$hr(),
            bslib::accordion(
              id = ns("amenities_accordion"),
              multiple = TRUE,
              !!!property_amenities_inputs
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("save_changes"), "Save Changes", class = "btn-primary")
            )
          )
        )
      })

      # Changes preview -------------------------------------------------------
      output$changes_preview <- shiny::renderUI({
        diff <- changes() |>
          dplyr::mutate(
            amenity_name,
            change = paste0(.data$old, " -> ", .data$new)
          )

        if (nrow(diff) == 0) {
          return(htmltools::tags$p("No changes detected", class = "text-muted"))
        }

        htmltools::tagList(
          htmltools::tags$p(
            "The following changes will be applied to the property amenities:",
            class = "text-muted"
          ),
          htmltools::tags$table(
            class = "table table-sm",
            htmltools::tags$thead(
              htmltools::tags$tr(
                htmltools::tags$th("Amenity"),
                htmltools::tags$th("Change")
              )
            ),
            htmltools::tags$tbody(
              lapply(seq_len(nrow(diff)), function(i) {
                htmltools::tags$tr(
                  htmltools::tags$td(diff$amenity_name[i]),
                  htmltools::tags$td(diff$change[i])
                )
              })
            )
          )
        )
      })


      # Save handler ----------------------------------------------------------
      shiny::observeEvent(input$save_changes, {
        new_values <- input_data() |>
          dplyr::mutate(
            updated_by = session$userData$user_id %||% "53b1207a-9066-49e4-9fcd-a6f439159759"
          )

        db_update_survey_property_amenities(
          pool = pool,
          new_values = new_values
        )

        db_refresh_trigger(db_refresh_trigger() + 1)
        shiny::removeModal()
      })

      # Summary display ------------------------------------------------------
      output$property_amenities_summary <- shiny::renderUI({
        purrr::map(unique(property_amenities$category), function(category) {
          amenities <- property_amenities_data() |>
            dplyr::filter(
              amenity_value == TRUE,
              amenity_name %in% (
                property_amenities |>
                  dplyr::filter(category == !!category) |>
                  dplyr::pull(amenity)
              )
            )

          htmltools::tagList(
            htmltools::tags$h3(
              class = "text-primary mb-2",
              bsicons::bs_icon(amenity_section_icons |>
                dplyr::filter(category == !!category) |>
                dplyr::pull(icon)),
              category
            ),
            if (nrow(amenities) > 0) {
              htmltools::tags$div(
                class = "mb-3",
                htmltools::tags$p(
                  glue::glue("{nrow(amenities)} {category} amenities selected"),
                  class = "text-muted"
                ),
                htmltools::tags$div(
                  class = "d-flex flex-wrap gap-2",
                  purrr::map(amenities$amenity_name, ~ {
                    htmltools::tags$span(
                      class = "badge bg-primary",
                      bsicons::bs_icon(
                        property_amenities |>
                          dplyr::filter(amenity == .x) |>
                          dplyr::pull(icon)
                      ),
                      .x
                    )
                  })
                )
              )
            } else {
              htmltools::tags$p("No amenities selected", class = "text-muted")
            }
          )
        })
      })

      output$last_updated <- shiny::renderText({
        sprintf("Last updated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      })

      return(list(
        property_amenities_data = property_amenities_data,
        refresh_trigger = db_refresh_trigger
      ))
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_property_amenities
#' @export
mod_survey_property_amenities_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Amenities",
    window_title = "Demo: Survey Amenities",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Amenities",
      value = "survey_property_amenities",
      icon = bsicons::bs_icon("house"),
      mod_survey_property_amenities_ui("demo"),
      shiny::actionButton(
        "edit_survey_section",
        "Edit",
        icon = shiny::icon("edit"),
        style = "width: auto;",
        class = "btn-sm btn-primary"
      )
    )
  )

  server <- function(input, output, session) {
    pool <- db_connect()
    edit_survey_section <- shiny::reactive({
      input$edit_survey_section
    })
    mod_survey_property_amenities_server(
      "demo",
      pool = pool,
      edit_survey_section = edit_survey_section
    )
  }

  shiny::shinyApp(ui, server)
}
