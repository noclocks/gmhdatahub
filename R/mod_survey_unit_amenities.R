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
#' @name mod_survey_unit_amenities
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Amenities page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_unit_amenities_ui()`: User Interface (UI) for the module.
#' - `mod_survey_unit_amenities_server()`: Server logic for the module.
#' - `mod_survey_unit_amenities_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_unit_amenities_ui()`: UI output
#' - `mod_survey_unit_amenities_server()`: List of reactive expressions.
#' - `mod_survey_unit_amenities_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_unit_amenities_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_unit_amenities
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_unit_amenities_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::card(
        class = "mx-auto mb-4",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h2(
            bsicons::bs_icon("buildings"),
            "Unit Amenities",
            class = "mb-2"
          ),
          htmltools::tags$p(
            "Per-Unit features and furnishings for the selected property or competitor.",
            class = "lead mb-0"
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("unit_amenities_summary"))
        ),
        bslib::card_footer(
          shiny::textOutput(ns("last_updated"))
        )
      )
    )
  )

}


# server ------------------------------------------------------------------

#' @rdname mod_survey_unit_amenities
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_unit_amenities_server <- function(
    id,
    pool = NULL,
    selected_property_id = NULL,
    selected_competitor_id = NULL,
    edit_survey_section = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_unit_amenities_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # handle selected property ID
      if (is.null(selected_property_id)) {
        prop_id <- get_property_id_by_name("1047 Commonwealth Avenue")
        selected_property_id <- shiny::reactive({
          prop_id
        })
        selected_property_name <- shiny::reactive({
          get_property_name_by_id(prop_id)
        })
      }

      # handle selected competitor ID
      if (is.null(selected_competitor_id)) {
        selected_competitor_id <- shiny::reactive({ "none" })
      } else if (shiny::is.reactive(selected_competitor_id)) {
        if (selected_competitor_id() == "none") {
          selected_competitor_id <- shiny::reactive({ "none" })
        }
      }

      # initialize reactives
      initial_data <- shiny::reactiveVal()
      input_changes <- shiny::reactiveVal(0)
      db_refresh_trigger <- shiny::reactiveVal(0)

      # selected property/competitor ID
      current_id <- shiny::reactive({
        # determine if reactive has any value
        # if (shiny::is.reactive(selected_competitor_id)) {
        #   if (selected_competitor_id() != "none") {
        #     selected_compertitor_id()
        #   } else {
            selected_property_id()
          # }
        # } else {
          # selected_property_id()
        # }
      })

      current_name <- shiny::reactive({
        # if (selected_competitor_id() != "none") {
        #   get_competitor_name_by_id(selected_competitor_id())
        # } else {
          get_property_name_by_id(selected_property_id())
        # }
      })

      # initial data
      unit_amenities_data <- shiny::reactive({
        shiny::req(pool, current_id())

        db_read_tbl(pool, "survey.unit_amenities") |>
          dplyr::filter(.data$property_name == current_name()) |>
          dplyr::select("property_name", "amenity_name", "amenity_value")

      })

      unit_amenities_rates_data <- shiny::reactive({
        shiny::req(pool, current_id())

        db_read_tbl(pool, "survey.unit_amenities_rates_premiums") |>
          dplyr::filter(.data$property_name == current_name()) |>
          dplyr::select("property_name", "amenity_name", "rentable_rate", "premium")
      })

      # input data
      input_data <- shiny::reactive({
        input_changes()
        amenities <- unit_amenities$amenity

        purrr::map_df(amenities, ~ {
          tibble::tibble(
            amenity_name = .x,
            amenity_value = input[[.x]] %||% FALSE
          )
        }) |>
          dplyr::mutate(property_name = selected_property_name())
      })

      rates_input_data <- shiny::reactive({
        input_changes()

        amenities <- c(
          "tv_rentable_rate",
          "tv_bedroom",
          "tv_common_area",
          "furniture_rentable_rate",
          "floor_premiums",
          "poolside_premiums",
          "top_floor_premiums",
          "view_premiums",
          "other_premiums"
        )

        tibble::tibble(
          amenity_name = amenities,
          rentable_rate = c(
            input[["tv_rentable_rate"]],
            input[["tv_bedroom"]],
            input[["tv_common_area"]],
            input[["furniture_rentable_rate"]],
            rep(NA_real_, 5)
          ),
          premium = c(
            rep(NA_real_, 4),
            input[["floor_premiums"]],
            input[["poolside_premiums"]],
            input[["top_floor_premiums"]],
            input[["view_premiums"]],
            input[["other_premiums"]]
          )
        ) |>
          dplyr::mutate(property_name = selected_property_name())

      })

      # changes tracking
      changes <- shiny::reactive({
        shiny::req(initial_data(), input_data())

        initial <- initial_data() |>
          dplyr::select(amenity_name, initial = amenity_value) |>
          dplyr::mutate(initial = as.logical(initial))

        current <- input_data() |>
          dplyr::select(amenity_name, current = amenity_value)

        dplyr::full_join(initial, current, by = "amenity_name") |>
          dplyr::mutate(
            old = dplyr::if_else(as.logical(initial), "Yes", "No"),
            new = dplyr::if_else(as.logical(current), "Yes", "No"),
            changed = initial != current
          ) |>
          dplyr::filter(changed) |>
          dplyr::select(-changed)
      })

      # modal
      shiny::observeEvent(edit_survey_section(), {
        data <- unit_amenities_data()
        initial_data(data)

        # input controls
        unit_amenities_inputs <- htmltools::tagList(
          lapply(
          unique(unit_amenities$category),
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
                  unit_amenities |>
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
                            unit_amenities |>
                              dplyr::filter(amenity == !!amenity) |>
                              dplyr::pull(icon)
                          ),
                          amenity
                        ),
                        value = as.logical(current_val) %||% FALSE
                      )
                    )
                  }
                )
              )
            )
          }
        ),
        bslib::accordion_panel(
          title = "TV",
          icon = bsicons::bs_icon("tv"),
          htmltools::tags$div(
            class = "p-3 bg-light rounded",
            bslib::input_switch(
              id = ns("tv_included_in_rent"),
              label = "TV Included in Rent?",
              value = unit_amenities_data() |>
                dplyr::filter(amenity_name == "TV Included in Rent") |>
                dplyr::pull(amenity_value) %||% FALSE
            ),
            shiny::numericInput(
              ns("tv_rentable_rate"),
              "TV Rentable Rate ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "TV Rentable Rate") |>
                dplyr::pull(rentable_rate) %||% 0,
              min = 0
            ),
            shiny::numericInput(
              ns("tv_bedroom"),
              "Bedroom TV ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "TV Bedroom") |>
                dplyr::pull(rentable_rate) %||% 0,
              min = 0
            ),
            shiny::numericInput(
              ns("tv_common_area"),
              "Common Area TV ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "TV Common Area") |>
                dplyr::pull(rentable_rate) %||% 0,
              min = 0
            )
          )
        ),
        bslib::accordion_panel(
          title = "Furniture",
          icon = bsicons::bs_icon("lamp"),
          htmltools::tags$div(
            class = "p-3 bg-light rounded",
            bslib::input_switch(
              id = ns("furniture_included_in_rent"),
              label = "Furniture Included in Rent?",
              value = unit_amenities_data() |>
                dplyr::filter(amenity_name == "Furniture Included in Rent") |>
                dplyr::pull(amenity_value) %||% FALSE
            ),
            shiny::numericInput(
              ns("furniture_rentable_rate"),
              "Furniture Rentable Rate ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "Furniture Rentable Rate") |>
                dplyr::pull(rentable_rate) %||% 0,
              min = 0
            )
          )
        ),
        bslib::accordion_panel(
          title = "Other Premiums",
          icon = bsicons::bs_icon("currency-dollar"),
          htmltools::tags$div(
            class = "p-3 bg-light rounded",
            shiny::numericInput(
              ns("floor_premiums"),
              "Floor Premiums ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "Floor Premiums") |>
                dplyr::pull(premium) %||% 0,
              min = 0
            ),
            shiny::numericInput(
              ns("poolside_premiums"),
              "Poolside Premiums ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "Poolside Premiums") |>
                dplyr::pull(premium) %||% 0,
              min = 0
            ),
            shiny::numericInput(
              ns("top_floor_premiums"),
              "Top Floor Premiums ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "Top Floor Premiums") |>
                dplyr::pull(premium) %||% 0,
              min = 0
            ),
            shiny::numericInput(
              ns("view_premiums"),
              "View Premiums ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "View Premiums") |>
                dplyr::pull(premium) %||% 0,
              min = 0
            ),
            shiny::numericInput(
              ns("other_premiums"),
              "Other Premiums ($)",
              value = unit_amenities_rates_data() |>
                dplyr::filter(amenity_name == "Other Premiums") |>
                dplyr::pull(premium) %||% 0,
              min = 0
            )
          )
        )
      )

        # input change observer
        lapply(
          c(unit_amenities$amenity, "tv_included_in_rent", "furniture_included_in_rent"),
          function(amenity) {
            shiny::observeEvent(input[[amenity]],
                                {
                                  input_changes(input_changes() + 1)
                                },
                                ignoreInit = TRUE
            )
          }
        )

        # modal
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
              !!!unit_amenities_inputs
            ),
            footer = htmltools::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(ns("save_changes"), "Save Changes", class = "btn-primary")
            )
          )
        )

        # changes preview
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

        # save
        shiny::observeEvent(input$save_changes, {

          new_values <- input_data() |>
            dplyr::mutate(
              updated_by = session$userData$user_id %||% "53b1207a-9066-49e4-9fcd-a6f439159759"
            )

          new_rates_values <- rates_input_data() |>
            dplyr::mutate(
              updated_by = session$userData$user_id %||% "53b1207a-9066-49e4-9fcd-a6f439159759"
            )

          db_update_survey_unit_amenities(
            pool = pool,
            new_values = new_values
          )

          db_update_survey_unit_amenities_rates_premiums(
            pool = pool,
            new_values = new_rates_values
          )

          db_refresh_trigger(db_refresh_trigger() + 1)

          shiny::removeModal()
        })
      })

      # summary display
      output$unit_amenities_summary <- shiny::renderUI({

        purrr::map(unique(unit_amenities$category), function(category) {

          # browser()

          amenities <- unit_amenities_data() |>
            dplyr::filter(
              amenity_value == TRUE,
              amenity_name %in% (
                unit_amenities |>
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
                        unit_amenities |>
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
            },
            htmltools::tags$h5(
              class = "text-primary mb-2",
              bsicons::bs_icon("tv"),
              "TV"
            ),
            htmltools::tags$div(
              class = "mb-3",
              htmltools::tags$p(
                "TV amenities selected",
                class = "text-muted"
              ),
              htmltools::tags$div(
                class = "d-flex flex-wrap gap-2",
                # display whether included in rent followed by numeric values
                htmltools::tags$span(
                  class = if (unit_amenities_data() |>
                               dplyr::filter(amenity_name == "TV Included in Rent") |>
                               dplyr::pull(amenity_value)) {
                    "badge bg-success"
                  } else {
                    "badge bg-danger"
                  },
                  htmltools::tags$strong("TV Included in Rent"),
                  ": ",
                  dplyr::if_else(
                    unit_amenities_data() |>
                      dplyr::filter(amenity_name == "TV Included in Rent") |>
                      dplyr::pull(amenity_value),
                    "Yes",
                    "No"
                  )
                ),
                htmltools::tags$br(),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("TV Rentable Rate"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "TV Rentable Rate") |>
                    dplyr::pull(rentable_rate) |>
                    scales::dollar(accuracy = 0.01)
                ),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Bedroom TV"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "TV Bedroom") |>
                    dplyr::pull(rentable_rate) |>
                    scales::dollar(accuracy = 0.01)
                ),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Common Area TV"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "TV Common Area") |>
                    dplyr::pull(rentable_rate) |>
                    scales::dollar(accuracy = 0.01)
                )
              )
            ),
            # Furniture
            htmltools::tags$h5(
              class = "text-primary mb-2",
              bsicons::bs_icon("lamp"),
              "Furniture"
            ),
            htmltools::tags$div(
              class = "mb-3",
              htmltools::tags$p(
                "Furniture amenities selected",
                class = "text-muted"
              ),
              htmltools::tags$div(
                class = "d-flex flex-wrap gap-2",
                # display whether included in rent followed by numeric values
                htmltools::tags$span(
                  class = if (unit_amenities_data() |>
                               dplyr::filter(amenity_name == "Furniture Included in Rent") |>
                               dplyr::pull(amenity_value)) {
                    "badge bg-success"
                  } else {
                    "badge bg-danger"
                  },
                  htmltools::tags$strong("Furniture Included in Rent"),
                  ": ",
                  dplyr::if_else(
                    unit_amenities_data() |>
                      dplyr::filter(amenity_name == "Furniture Included in Rent") |>
                      dplyr::pull(amenity_value),
                    "Yes",
                    "No"
                  )
                ),
                htmltools::tags$br(),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Furniture Rentable Rate"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "Furniture Rentable Rate") |>
                    dplyr::pull(rentable_rate) |>
                    scales::dollar(accuracy = 0.01)
                )
              )
            ),
            # Other Premiums
            htmltools::tags$h5(
              class = "text-primary mb-2",
              bsicons::bs_icon("currency-dollar"),
              "Other Premiums"
            ),
            htmltools::tags$div(
              class = "mb-3",
              htmltools::tags$p(
                "Other Premiums selected",
                class = "text-muted"
              ),
              htmltools::tags$div(
                class = "d-flex flex-wrap gap-2",
                # display numeric values
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Floor Premiums"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "Floor Premiums") |>
                    dplyr::pull(premium) |>
                    scales::dollar(accuracy = 0.01)
                ),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Poolside Premiums"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "Poolside Premiums") |>
                    dplyr::pull(premium) |>
                    scales::dollar(accuracy = 0.01)
                ),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Top Floor Premiums"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "Top Floor Premiums") |>
                    dplyr::pull(premium) |>
                    scales::dollar(accuracy = 0.01)
                ),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("View Premiums"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "View Premiums") |>
                    dplyr::pull(premium) |>
                    scales::dollar(accuracy = 0.01)
                ),
                htmltools::tags$span(
                  class = "badge bg-primary",
                  htmltools::tags$strong("Other Premiums"),
                  ": ",
                  unit_amenities_rates_data() |>
                    dplyr::filter(amenity_name == "Other Premiums") |>
                    dplyr::pull(premium) |>
                    scales::dollar(accuracy = 0.01)
                )
              )
            )
          )
        })
      })

      output$last_updated <- shiny::renderText({
        sprintf("Last updated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      })

      return(list(
        unit_amenities_data = unit_amenities_data,
        refresh_trigger = db_refresh_trigger
      ))

    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_unit_amenities
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_unit_amenities_demo <- function() {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Amenities",
    window_title = "Demo: Survey Amenities",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Amenities",
      value = "survey_unit_amenities",
      icon = bsicons::bs_icon("buildings"),
      mod_survey_unit_amenities_ui("demo"),
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
    mod_survey_unit_amenities_server(
      "demo",
      pool = pool,
      edit_survey_section = edit_survey_section
    )
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------
