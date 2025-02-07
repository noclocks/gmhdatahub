#  ------------------------------------------------------------------------
#
# Title : Survey Notes Section Module
#    By : Jimmy Briggs
#  Date : 2025-01-18
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Survey Notes Section Module
#'
#' @name mod_survey_notes
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Market Survey "Notes" Section.
#'
#' Includes the following functions:
#'
#' - `mod_survey_notes_ui()`: User Interface (UI) for the module.
#' - `mod_survey_notes_server()`: Server logic for the module.
#' - `mod_survey_notes_demo()`: Demo application for the module.
#'
#' plus an additional UI utility function to create note cards:
#'
#' - `create_note_card()`: Create a note card for display.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#' @param global_filters Reactive expression for global filters.
#' @param note A single note object.
#' @param ns Namespace function.
#'
#' @returns
#' - `mod_survey_notes_ui()`: UI output.
#' - `mod_survey_notes_server()`: List of reactive expressions.
#' - `mod_survey_notes_demo()`: `NULL`, runs a demo application.
#' - `create_note_card()`: Utility function to create note cards.
#'
#' @examplesIf interactive()
#' mod_survey_notes_demo()
NULL

#' @rdname mod_survey_notes
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_column_wrap value_box card card_header tooltip
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS textOutput actionButton icon selectInput uiOutput
mod_survey_notes_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("sticky"),
            htmltools::tags$span(
              "Notes - ",
              shiny::textOutput(ns("property_name_title"), inline = TRUE)
            ),
            shiny::actionButton(
              ns("add_note"),
              "Add New Note",
              icon = shiny::icon("plus"),
              class = "btn-sm btn-outline-success float-end"
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
          bslib::layout_column_wrap(
            width = 1 / 4,
            bslib::value_box(
              "Total Notes",
              shiny::textOutput(ns("total_notes")),
              showcase = bsicons::bs_icon("clipboard")
            ),
            bslib::value_box(
              "Actionable Items",
              shiny::textOutput(ns("actionable_notes")),
              showcase = bsicons::bs_icon("exclamation-circle")
            ),
            bslib::value_box(
              "Completed",
              shiny::textOutput(ns("completed_notes")),
              showcase = bsicons::bs_icon("check-circle")
            ),
            bslib::value_box(
              "Pending",
              shiny::textOutput(ns("pending_notes")),
              showcase = bsicons::bs_icon("clock")
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                "Leasing Specials Notes"
              ),
              bslib::card_body(
                shiny::textAreaInput(
                  ns("leasing_specials_notes"),
                  "Add Leasing Specials Notes",
                  width = "100%",
                  height = "150px",
                  resize = "vertical"
                )
              )
            ),
            bslib::card(
              bslib::card_header(
                "Property Notes",
                class = "bg-primary text-white"
              ),
              bslib::card_body(
                shiny::textAreaInput(
                  ns("property_notes"),
                  "Add Property Notes",
                  width = "100%",
                  height = "150px",
                  resize = "vertical"
                )
              )
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

#' @rdname mod_survey_notes
#' @export
#' @importFrom bslib layout_column_wrap card_body
#' @importFrom htmltools tagList tags
#' @importFrom shiny moduleServer reactiveVal reactive renderText observeEvent
#' @importFrom shiny showModal selectInput textInput checkboxInput textAreaInput
#' @importFrom shiny actionButton icon modalButton observe modalDialog removeModal
#' @importFrom shiny showNotification renderUI req
mod_survey_notes_server <- function(
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
      cli::cat_rule("[Module]: mod_survey_notes_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- shinyvalidate::InputValidator$new()

      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      output$property_name_title <- shiny::renderText({
        shiny::req(survey_data$notes)

        prop_id <- survey_data$notes$property_id |> unique()
        comp_id <- survey_data$notes$competitor_id |> unique()
        prop_name <- survey_data$notes$property_name |> unique()

        if (is.na(comp_id)) {
          paste0(name, " (", prop_id, ")")
        } else {
          paste0(name, " (Competitor #", comp_id, ")")
        }
      })

      # data --------------------------------------------------------------------
      notes_data <- shiny::reactive({
        shiny::req(survey_data$notes)

        if (nrow(survey_data$notes) == 0) {
          default_tbl_survey_notes()
        } else {
          survey_data$notes |>
            dplyr::select(
              note_id,
              note_type,
              note_actionable,
              note_status,
              note_tags,
              note_content
            )
        }
      })

      output$last_updated_at <- shiny::renderText({
        shiny::req(survey_data$notes)
        survey_data$notes$updated_at |>
          max(na.rm = TRUE) |>
          format("%B %d, %Y %I:%M %p")
      })

      shiny::observe({
        shiny::req(notes_data())

        # get note content - leasing specials
        leasing_notes <- notes_data() |>
          dplyr::filter(.data$note_type == "Leasing Specials") |>
          dplyr::pull(.data$note_content) |>
          # merge separating on separate lines
          paste(collapse = "\n")

        if (leasing_notes == "") {
          shiny::updateTextAreaInput(
            session,
            "leasing_specials_notes",
            value = NULL,
            placeholder = "No leasing specials notes available."
          )
        } else {
          shiny::updateTextAreaInput(
            session,
            "leasing_specials_notes",
            value = leasing_notes
          )
        }

        property_notes <- notes_data() |>
          dplyr::filter(.data$note_type == "Property Notes") |>
          dplyr::pull(.data$note_content) |>
          # merge separating on separate lines
          paste(collapse = "\n")

        if (property_notes == "") {
          shiny::updateTextAreaInput(
            session,
            "property_notes",
            value = NULL,
            placeholder = "No property notes available."
          )
        } else {
          shiny::updateTextAreaInput(
            session,
            "property_notes",
            value = property_notes
          )
        }
      })

      # outputs -----------------------------------------------------------------
      output$total_notes <- shiny::renderText({
        nrow(notes_data())
      })

      output$actionable_notes <- shiny::renderText({
        sum(notes_data()$note_actionable, na.rm = TRUE)
      })

      output$completed_notes <- shiny::renderText({
        sum(notes_data()$note_status == "Complete", na.rm = TRUE)
      })

      output$pending_notes <- shiny::renderText({
        sum(notes_data()$note_status == "Pending", na.rm = TRUE)
      })

      # add -------------------------------------------------------------------
      shiny::observeEvent(input$add_note, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Add New Note",
            size = "l",
            easyClose = TRUE,
            bslib::layout_column_wrap(
              width = 1 / 2,
              shiny::selectInput(
                ns("note_type"),
                "Note Type",
                choices = c("Leasing Notes", "Property Notes")
              ),
              shiny::textInput(
                ns("note_tags"),
                "Note Tags",
                placeholder = "Urgent, Follow-Up, Maintenance"
              )
            ),
            bslib::layout_column_wrap(
              width = 1 / 2,
              shiny::checkboxInput(
                ns("note_actionable"),
                "Note Actionable?",
                value = FALSE
              ),
              shiny::selectInput(
                ns("note_status"),
                "Note Status",
                choices = c(
                  "Pending",
                  "In Progress",
                  "Complete",
                  "Cancelled"
                )
              )
            ),
            shiny::textAreaInput(
              ns("note_content"),
              "Note Content",
              width = "100%",
              height = "150px",
              resize = "vertical"
            ),
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Add Note",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })


      shiny::observeEvent(input$save, {

        new_note <- tibble::tibble(
          note_type = input$note_type,
          note_actionable = input$note_actionable,
          note_status = input$note_status,
          note_tags = input$note_tags,
          note_content = input$note_content
        ) |>
          dplyr::mutate(
            note_type = dplyr::coalesce(.data$note_type, "General"),
            note_actionable = dplyr::coalesce(.data$note_actionable, FALSE),
            note_status = dplyr::coalesce(.data$note_status, "Pending"),
            note_tags = dplyr::coalesce(.data$note_tags, "{}")
          )

        db_update_survey_notes(
          pool,
          new_note
        )

        shiny::removeModal()
        shiny::showNotification("Note added successfully!", duration = 5)
      })

      return(
        list(
          notes_data = notes_data
        )
      )
    })
}

mod_survey_notes_demo <- function(pool = NULL) {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Notes Section",
    window_title = "Demo: Survey Property Summary",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Property Summary",
      value = "survey_property_summary",
      icon = bsicons::bs_icon("house"),
      shinyjs::useShinyjs(),
      mod_survey_notes_ui("demo")
    )
  )

  server <- function(input, output, session) {
    if (is.null(pool)) pool <- db_connect()
    mod_survey_notes_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)
}


# utilities ---------------------------------------------------------------

#' @rdname mod_survey_notes
#' @export
#' @importFrom bslib card card_header card_body tooltip
#' @importFrom shiny NS actionButton textOutput selectInput icon showModal modalDialog
#' @importFrom shiny showNotification removeModal
#' @importFrom htmltools tags tagList
create_note_card <- function(note, ns) {
  status_colors <- c(
    "Pending" = "warning",
    "In Progress" = "info",
    "Complete" = "success",
    "Cancelled" = "danger"
  )

  bslib::card(
    class = if (note$note_actionable) "mb-3 border-primary" else "mb-3",
    height = "100%",
    bslib::card_header(
      htmltools::tags$div(
        class = "d-flex justify-content-between align-items-center",
        htmltools::tags$div(
          htmltools::tags$h5(note$note_type, class = "card-title")
        ),
        htmltools::tags$div(
          class = "text-end",
          htmltools::tags$span(note$timestamp, class = "text-muted small d-block"),
          htmltools::tags$span(paste("By:", note$submitted_by), class = "text-muted small d-block")
        )
      )
    ),
    bslib::card_body(
      htmltools::tags$p(class = "lead", note$note_content),
      htmltools::tags$div(
        class = "d-flex flex-wrap gap-2 mb-3",
        if (note$note_actionable) {
          htmltools::tags$span(class = "badge bg-primary", shiny::icon("exclamation-circle"), "Actionable")
        },
        htmltools::tags$span(
          class = paste0("badge bg-", status_colors[note$note_status]),
          shiny::icon(switch(note$note_status,
                             "Pending" = "clock",
                             "In Progress" = "spinner",
                             "Complete" = "check",
                             "Cancelled" = "ban")),
          stringr::str_to_title(gsub("_", " ", note$note_status))
        ),
        if (!is.null(note$note_tags) && note$note_tags != "") {
          lapply(strsplit(note$note_tags, ",")[[1]], function(tag) {
            htmltools::tags$span(class = "badge bg-secondary", shiny::icon("tag"), trimws(tag))
          })
        }
      ),
      htmltools::tags$hr(),
      htmltools::tags$div(
        class = "d-flex gap-2 justify-content-end",
        bslib::tooltip(
          shiny::actionButton(ns(paste0("edit_", note$note_id)),
                              label = "",
                              icon = shiny::icon("edit"),
                              class = "btn-sm btn-outline-primary"
          ),
          "Edit note"
        ),
        if (note$status != "completed") {
          bslib::tooltip(
            shiny::actionButton(ns(paste0("complete_", note$note_id)),
                                label = "",
                                icon = shiny::icon("check"),
                                class = "btn-sm btn-outline-success"
            ),
            "Mark as complete"
          )
        },
        bslib::tooltip(
          shiny::actionButton(ns(paste0("delete_", note$note_id)),
                              label = "",
                              icon = shiny::icon("trash"),
                              class = "btn-sm btn-outline-danger"
          ),
          "Delete note"
        )
      )
    )
  )
}
