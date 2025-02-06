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
#' @importFrom bslib card card_header card_body tooltip
#' @importFrom shiny NS actionButton textOutput selectInput icon showModal modalDialog
#' @importFrom shiny showNotification removeModal
#' @importFrom htmltools tags tagList
create_note_card <- function(note, ns) {
  status_colors <- c(
    "pending" = "warning",
    "in_progress" = "info",
    "completed" = "success",
    "cancelled" = "danger"
  )

  bslib::card(
    class = if (note$actionable) "mb-3 border-primary" else "mb-3",
    height = "100%",
    bslib::card_header(
      htmltools::tags$div(
        class = "d-flex justify-content-between align-items-center",
        htmltools::tags$div(
          htmltools::tags$h5(note$type, class = "mb-0"),
          htmltools::tags$span(note$property, class = "text-muted small")
        ),
        htmltools::tags$div(
          class = "text-end",
          htmltools::tags$span(note$timestamp, class = "text-muted small d-block"),
          htmltools::tags$span(paste("By:", note$submitted_by), class = "text-muted small d-block")
        )
      )
    ),
    bslib::card_body(
      htmltools::tags$p(class = "lead", note$text),
      htmltools::tags$div(
        class = "d-flex flex-wrap gap-2 mb-3",
        if (note$actionable) {
          htmltools::tags$span(class = "badge bg-primary", shiny::icon("exclamation-circle"), "Actionable")
        },
        htmltools::tags$span(
          class = paste0("badge bg-", status_colors[note$status]),
          shiny::icon(switch(note$status,
            "pending" = "clock",
            "in_progress" = "spinner",
            "completed" = "check",
            "cancelled" = "ban"
          )),
          stringr::str_to_title(gsub("_", " ", note$status))
        ),
        if (!is.null(note$tags) && note$tags != "") {
          lapply(strsplit(note$tags, ",")[[1]], function(tag) {
            htmltools::tags$span(class = "badge bg-secondary", shiny::icon("tag"), trimws(tag))
          })
        }
      ),
      htmltools::tags$hr(),
      htmltools::tags$div(
        class = "d-flex gap-2 justify-content-end",
        bslib::tooltip(
          shiny::actionButton(ns(paste0("edit_", note$id)),
            label = "",
            icon = shiny::icon("edit"),
            class = "btn-sm btn-outline-primary"
          ),
          "Edit note"
        ),
        if (note$status != "completed") {
          bslib::tooltip(
            shiny::actionButton(ns(paste0("complete_", note$id)),
              label = "",
              icon = shiny::icon("check"),
              class = "btn-sm btn-outline-success"
            ),
            "Mark as complete"
          )
        },
        bslib::tooltip(
          shiny::actionButton(ns(paste0("delete_", note$id)),
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

#' @rdname mod_survey_notes
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib layout_column_wrap value_box card card_header tooltip
#' @importFrom htmltools tagList tags
#' @importFrom shiny NS textOutput actionButton icon selectInput uiOutput
mod_survey_notes_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
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
    bslib::card(
      bslib::card_header(
        htmltools::tags$div(
          class = "d-flex justify-content-between align-items-center",
          htmltools::tags$h4("Property Notes", class = "mb-0"),
          htmltools::tags$div(
            class = "d-flex gap-2",
            bslib::tooltip(
              shiny::actionButton(
                ns("add_note"),
                label = "Add Note",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              ),
              "Add a new note"
            ),
            shiny::selectInput(
              ns("filter_status"),
              NULL,
              choices = c(
                "All Status" = "",
                "Pending" = "pending",
                "In Progress" = "in_progress",
                "Completed" = "completed",
                "Cancelled" = "cancelled"
              ),
              selected = "",
              width = "150px"
            ),
            shiny::selectInput(
              ns("filter_type"),
              NULL,
              choices = c(
                "All Types" = "",
                "Leasing Notes" = "Leasing Notes",
                "Property Notes" = "Property Notes"
              ),
              selected = "",
              width = "150px"
            )
          )
        )
      ),
      shiny::uiOutput(ns("notes_display"))
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
    selected_property_id = NULL,
    selected_competitor_id = NULL,
    edit_survey_section = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # check database connection
    if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
    check_db_conn(pool)

    # Initialize reactive values with demo data
    notes_data <- shiny::reactiveVal(
      data.frame(
        id = 1:3,
        timestamp = c(
          "2024-01-15 09:30:00",
          "2024-01-14 14:45:00",
          "2024-01-13 11:20:00"
        ),
        type = c(
          "Leasing Notes",
          "Property Notes",
          "Leasing Notes"
        ),
        property = c(
          "Building A",
          "Building B",
          "Building A"
        ),
        text = c(
          "Potential tenant inquired about lease terms for the 3rd floor space. Follow up needed next week.",
          "HVAC maintenance scheduled for next month. Building engineer to coordinate access.",
          "Current tenant in Unit 204 requested information about lease renewal options."
        ),
        actionable = c(
          TRUE,
          TRUE,
          FALSE
        ),
        status = c(
          "pending",
          "in_progress",
          "completed"
        ),
        tags = c(
          "follow-up, potential tenant",
          "maintenance, scheduled",
          "tenant, renewal"
        ),
        submitted_by = c(
          "John Smith",
          "Jane Doe",
          "John Smith"
        ),
        stringsAsFactors = FALSE
      )
    )

    # Filtered notes
    filtered_notes <- shiny::reactive({
      notes <- notes_data()

      if (!is.null(input$filter_status) && input$filter_status != "") {
        notes <- notes[notes$status == input$filter_status, ]
      }

      if (!is.null(input$filter_type) && input$filter_type != "") {
        notes <- notes[notes$type == input$filter_type, ]
      }

      notes
    })

    # Statistics outputs
    output$total_notes <- shiny::renderText({
      nrow(notes_data())
    })

    output$actionable_notes <- shiny::renderText({
      sum(notes_data()$actionable)
    })

    output$completed_notes <- shiny::renderText({
      sum(notes_data()$status == "completed")
    })

    output$pending_notes <- shiny::renderText({
      sum(notes_data()$status == "pending")
    })

    # Add note button click
    shiny::observeEvent(input$add_note, {
      shiny::showModal(modalDialog(
        title = "Add New Note",
        size = "l",
        bslib::layout_column_wrap(
          width = 1 / 2,
          shiny::selectInput(session$ns("note_type"),
            "Note Type",
            choices = c("Leasing Notes", "Property Notes")
          ),
          shiny::selectInput(session$ns("property"),
            "Property",
            choices = c("Building A", "Building B", "Building C")
          )
        ),
        shiny::textInput(session$ns("tags"),
          "Tags",
          placeholder = "urgent, follow-up, maintenance"
        ),
        bslib::layout_column_wrap(
          width = 1 / 2,
          shiny::checkboxInput(session$ns("is_actionable"),
            "Contains Actionable Information",
            value = FALSE
          ),
          shiny::selectInput(session$ns("status"),
            "Status",
            choices = c("pending", "in_progress", "completed", "cancelled")
          )
        ),
        shiny::textAreaInput(session$ns("note_text"),
          "Note Content",
          width = "100%",
          height = "150px",
          resize = "vertical"
        ),
        footer = htmltools::tagList(
          shiny::actionButton(session$ns("submit_note"),
            "Submit Note",
            icon = shiny::icon("plus"),
            class = "btn-primary"
          ),
          shiny::modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    # Handle edit button clicks
    shiny::observe({
      notes <- notes_data()
      for (note in 1:nrow(notes)) {
        local({
          note_id <- notes$id[note]
          shiny::observeEvent(input[[paste0("edit_", note_id)]], {
            current_edit_id(note_id)
            current_note <- notes[notes$id == note_id, ]
            shiny::showModal(shiny::modalDialog(
              title = "Edit Note",
              size = "l",
              shiny::textAreaInput(session$ns("edit_text"), "Note Content",
                value = current_note$text,
                height = "150px"
              ),
              shiny::textInput(session$ns("edit_tags"), "Tags (comma-separated)",
                value = current_note$tags
              ),
              shiny::selectInput(session$ns("edit_status"), "Status",
                choices = c("pending", "in_progress", "completed", "cancelled"),
                selected = current_note$status
              ),
              footer = tagList(
                shiny::actionButton(session$ns("save_edit"),
                  label = "Save Changes",
                  class = "btn-primary",
                  icon = icon("save")
                ),
                shiny::modalButton("Cancel")
              ),
              easyClose = TRUE
            ))
          })
        })
      }
    })

    # Handle save edits
    shiny::observeEvent(input$save_edit, {
      notes <- notes_data()
      note_id <- current_edit_id()

      notes[notes$id == note_id, "text"] <- input$edit_text
      notes[notes$id == note_id, "tags"] <- input$edit_tags
      notes[notes$id == note_id, "status"] <- input$edit_status

      notes_data(notes)
      shiny::removeModal()
      shiny::showNotification("Note updated successfully!", type = "success")
    })

    # Handle complete button clicks
    shiny::observe({
      notes <- notes_data()
      for (note in 1:nrow(notes)) {
        local({
          note_id <- notes$id[note]
          shiny::observeEvent(input[[paste0("complete_", note_id)]], {
            notes <- notes_data()
            notes[notes$id == note_id, "status"] <- "completed"
            notes_data(notes)
            shiny::showNotification("Note marked as complete!", type = "success")
          })
        })
      }
    })

    # Handle delete button clicks
    shiny::observe({
      notes <- notes_data()
      for (note in 1:nrow(notes)) {
        local({
          note_id <- notes$id[note]
          shiny::observeEvent(input[[paste0("delete_", note_id)]], {
            shiny::showModal(shiny::modalDialog(
              title = "Confirm Deletion",
              "Are you sure you want to delete this note?",
              footer = tagList(
                shiny::actionButton(session$ns(paste0("confirm_delete_", note_id)),
                  "Delete",
                  class = "btn-danger"
                ),
                shiny::modalButton("Cancel")
              ),
              easyClose = TRUE
            ))
          })

          # Handle delete confirmation
          shiny::observeEvent(input[[paste0("confirm_delete_", note_id)]], {
            notes <- notes_data()
            notes_data(notes[notes$id != note_id, ])
            shiny::removeModal()
            shiny::showNotification("Note deleted successfully!", type = "warning")
          })
        })
      }
    })

    # Render notes display
    output$notes_display <- shiny::renderUI({
      notes <- filtered_notes() # Use filtered notes instead of all notes

      if (nrow(notes) == 0) {
        return(
          bslib::card_body(
            htmltools::tags$div(
              class = "text-center text-muted p-4",
              shiny::icon("notebook"),
              htmltools::tags$p("No notes have been submitted yet.")
            )
          )
        )
      }

      # Sort notes by timestamp (most recent first)
      notes <- notes[order(notes$timestamp, decreasing = TRUE), ]

      note_cards <- lapply(1:nrow(notes), function(i) {
        create_note_card(notes[i, ], session$ns)
      })

      bslib::card_body(
        htmltools::tags$div(
          class = "note-cards",
          style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); gap: 1rem;",
          !!!note_cards
        )
      )
    })

    # Handle note submission (modified to close modal)
    shiny::observeEvent(input$submit_note, {
      shiny::req(input$note_text, input$property)

      if (nchar(input$note_text) > 0) {
        new_note <- data.frame(
          id = max(notes_data()$id) + 1,
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          type = input$note_type,
          property = input$property,
          text = input$note_text,
          actionable = input$is_actionable,
          status = input$status,
          tags = input$tags,
          submitted_by = "Current User",
          stringsAsFactors = FALSE
        )

        notes_data(bind_rows(new_note, notes_data()))

        shiny::removeModal()
        shiny::showNotification("Note added successfully!", type = "success")
      }
    })
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
