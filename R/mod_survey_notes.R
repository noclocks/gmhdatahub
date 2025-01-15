library(shiny)
library(bslib)
library(dplyr)

# Custom note card function
create_note_card <- function(note, ns) {
  status_colors <- c(
    "pending" = "warning",
    "in_progress" = "info",
    "completed" = "success",
    "cancelled" = "danger"
  )

  card(
    class = if (note$actionable) "mb-3 border-primary" else "mb-3",
    height = "100%",
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        div(
          h5(note$type, class = "mb-0"),
          span(note$property, class = "text-muted small")
        ),
        div(
          class = "text-end",
          span(note$timestamp, class = "text-muted small d-block"),
          span(paste("By:", note$submitted_by), class = "text-muted small d-block")
        )
      )
    ),
    card_body(
      p(class = "lead", note$text),
      div(
        class = "d-flex flex-wrap gap-2 mb-3",
        if (note$actionable) {
          span(class = "badge bg-primary", icon("exclamation-circle"), "Actionable")
        },
        span(
          class = paste0("badge bg-", status_colors[note$status]),
          icon(switch(
            note$status,
            "pending" = "clock",
            "in_progress" = "spinner",
            "completed" = "check",
            "cancelled" = "ban"
          )),
          stringr::str_to_title(gsub("_", " ", note$status))
        ),
        if (!is.null(note$tags) && note$tags != "") {
          lapply(strsplit(note$tags, ",")[[1]], function(tag) {
            span(class = "badge bg-secondary", icon("tag"), trimws(tag))
          })
        }
      ),
      hr(),
      div(
        class = "d-flex gap-2 justify-content-end",
        tooltip(
          actionButton(ns(paste0("edit_", note$id)),
                       label = "",
                       icon = icon("edit"),
                       class = "btn-sm btn-outline-primary"),
          "Edit note"
        ),
        if (note$status != "completed") {
          tooltip(
            actionButton(ns(paste0("complete_", note$id)),
                         label = "",
                         icon = icon("check"),
                         class = "btn-sm btn-outline-success"),
            "Mark as complete"
          )
        },
        tooltip(
          actionButton(ns(paste0("delete_", note$id)),
                       label = "",
                       icon = icon("trash"),
                       class = "btn-sm btn-outline-danger"),
          "Delete note"
        )
      )
    )
  )
}

# Module UI function
mod_survey_notes_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # layout_column_wrap(
    #   width = 1/4,
    #   value_box(
    #     "Total Notes",
    #     textOutput(ns("total_notes")),
    #     showcase = bsicons::bs_icon("clipboard")
    #   ),
    #   value_box(
    #     "Actionable Items",
    #     textOutput(ns("actionable_notes")),
    #     showcase = bsicons::bs_icon("exclamation-circle")
    #   ),
    #   value_box(
    #     "Completed",
    #     textOutput(ns("completed_notes")),
    #     showcase = bsicons::bs_icon("check-circle")
    #   ),
    #   value_box(
    #     "Pending",
    #     textOutput(ns("pending_notes")),
    #     showcase = bsicons::bs_icon("clock")
    #   )
    # ),
    card(
      card_header(
        div(
          class = "d-flex justify-content-between align-items-center",
          h4("Property Notes", class = "mb-0"),
          div(
            class = "d-flex gap-2",
            tooltip(
              actionButton(ns("add_note"),
                           label = "Add Note",
                           icon = icon("plus"),
                           class = "btn-primary"),
              "Add a new note"
            ),
            selectInput(ns("filter_status"),
                        NULL,
                        choices = c("All Status" = "",
                                    "Pending" = "pending",
                                    "In Progress" = "in_progress",
                                    "Completed" = "completed",
                                    "Cancelled" = "cancelled"),
                        selected = "",
                        width = "150px"),
            selectInput(ns("filter_type"),
                        NULL,
                        choices = c("All Types" = "",
                                    "Leasing Notes" = "Leasing Notes",
                                    "Property Notes" = "Property Notes"),
                        selected = "",
                        width = "150px")
          )
        )
      ),
      uiOutput(ns("notes_display"))
    )
  )
}

# Module server function
mod_survey_notes_server <- function(id, pool = NULL, global_filters = NULL) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values with demo data
    notes_data <- reactiveVal(
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
    filtered_notes <- reactive({
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
    output$total_notes <- renderText({
      nrow(notes_data())
    })

    output$actionable_notes <- renderText({
      sum(notes_data()$actionable)
    })

    output$completed_notes <- renderText({
      sum(notes_data()$status == "completed")
    })

    output$pending_notes <- renderText({
      sum(notes_data()$status == "pending")
    })

    # Add note button click
    observeEvent(input$add_note, {
      showModal(modalDialog(
        title = "Add New Note",
        size = "l",
        layout_column_wrap(
          width = 1/2,
          selectInput(session$ns("note_type"),
                      "Note Type",
                      choices = c("Leasing Notes", "Property Notes")),
          selectInput(session$ns("property"),
                      "Property",
                      choices = c("Building A", "Building B", "Building C"))
        ),
        textInput(session$ns("tags"),
                  "Tags",
                  placeholder = "urgent, follow-up, maintenance"),
        layout_column_wrap(
          width = 1/2,
          checkboxInput(session$ns("is_actionable"),
                        "Contains Actionable Information",
                        value = FALSE),
          selectInput(session$ns("status"),
                      "Status",
                      choices = c("pending", "in_progress", "completed", "cancelled"))
        ),
        textAreaInput(session$ns("note_text"),
                      "Note Content",
                      width = "100%",
                      height = "150px",
                      resize = "vertical"),
        footer = tagList(
          actionButton(session$ns("submit_note"),
                       "Submit Note",
                       icon = icon("plus"),
                       class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    })

    # Handle edit button clicks
    observe({
      notes <- notes_data()
      for (note in 1:nrow(notes)) {
        local({
          note_id <- notes$id[note]
          observeEvent(input[[paste0("edit_", note_id)]], {
            current_edit_id(note_id)
            current_note <- notes[notes$id == note_id, ]
            showModal(modalDialog(
              title = "Edit Note",
              size = "l",
              textAreaInput(session$ns("edit_text"), "Note Content",
                            value = current_note$text,
                            height = "150px"),
              textInput(session$ns("edit_tags"), "Tags (comma-separated)",
                        value = current_note$tags),
              selectInput(session$ns("edit_status"), "Status",
                          choices = c("pending", "in_progress", "completed", "cancelled"),
                          selected = current_note$status),
              footer = tagList(
                actionButton(session$ns("save_edit"),
                             label = "Save Changes",
                             class = "btn-primary",
                             icon = icon("save")),
                modalButton("Cancel")
              ),
              easyClose = TRUE
            ))
          })
        })
      }
    })

    # Handle save edits
    observeEvent(input$save_edit, {
      notes <- notes_data()
      note_id <- current_edit_id()

      notes[notes$id == note_id, "text"] <- input$edit_text
      notes[notes$id == note_id, "tags"] <- input$edit_tags
      notes[notes$id == note_id, "status"] <- input$edit_status

      notes_data(notes)
      removeModal()
      showNotification("Note updated successfully!", type = "success")
    })

    # Handle complete button clicks
    observe({
      notes <- notes_data()
      for (note in 1:nrow(notes)) {
        local({
          note_id <- notes$id[note]
          observeEvent(input[[paste0("complete_", note_id)]], {
            notes <- notes_data()
            notes[notes$id == note_id, "status"] <- "completed"
            notes_data(notes)
            showNotification("Note marked as complete!", type = "success")
          })
        })
      }
    })

    # Handle delete button clicks
    observe({
      notes <- notes_data()
      for (note in 1:nrow(notes)) {
        local({
          note_id <- notes$id[note]
          observeEvent(input[[paste0("delete_", note_id)]], {
            showModal(modalDialog(
              title = "Confirm Deletion",
              "Are you sure you want to delete this note?",
              footer = tagList(
                actionButton(session$ns(paste0("confirm_delete_", note_id)),
                             "Delete",
                             class = "btn-danger"),
                modalButton("Cancel")
              ),
              easyClose = TRUE
            ))
          })

          # Handle delete confirmation
          observeEvent(input[[paste0("confirm_delete_", note_id)]], {
            notes <- notes_data()
            notes_data(notes[notes$id != note_id, ])
            removeModal()
            showNotification("Note deleted successfully!", type = "warning")
          })
        })
      }
    })

    # Render notes display
    output$notes_display <- renderUI({
      notes <- filtered_notes()  # Use filtered notes instead of all notes

      if (nrow(notes) == 0) {
        return(card_body(
          div(
            class = "text-center text-muted p-4",
            icon("notebook"),
            p("No notes have been submitted yet.")
          )
        ))
      }

      # Sort notes by timestamp (most recent first)
      notes <- notes[order(notes$timestamp, decreasing = TRUE), ]

      note_cards <- lapply(1:nrow(notes), function(i) {
        create_note_card(notes[i, ], session$ns)
      })

      card_body(
        div(
          class = "note-cards",
          style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); gap: 1rem;",
          !!!note_cards
        )
      )
    })

    # Handle note submission (modified to close modal)
    observeEvent(input$submit_note, {
      req(input$note_text, input$property)

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

        removeModal()
        showNotification("Note added successfully!", type = "success")
      }
    })
  })
}
