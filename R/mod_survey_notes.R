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
              ns("refresh"),
              "Refresh Data",
              icon = shiny::icon("sync"),
              class = "btn-sm btn-outline-light float-end",
              style = "width: auto;"
            ),
            shiny::actionButton(
              ns("add_note"),
              label = "Add Note",
              icon = shiny::icon("plus"),
              class = "btn-sm btn-outline-success float-end",
            ),
            shiny::selectInput(
              ns("filter_status"),
              NULL,
              choices = c(
                "All" = "",
                "Pending" = "Pending",
                "In Progress" = "In Progress",
                "Complete" = "Complete",
                "Cancelled" = "Cancelled"
              ),
              selected = "",
              width = "150px"
            ),
            shiny::selectInput(
              ns("filter_type"),
              NULL,
              choices = c(
                "All Types" = "",
                "Leasing Specials" = "Leasing Specials",
                "Property Notes" = "Property Notes"
              ),
              selected = "",
              width = "150px"
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
            col_widths = c(12),
            shiny::uiOutput(ns("notes_display"))
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
              note_content,
              timestamp = updated_at,
              submitted_by = updated_by
            ) |>
            dplyr::arrange(dplyr::desc(updated_at))
        }
      })

      filtered_notes <- shiny::reactive({
        shiny::req(notes_data())
        notes <- notes_data()
        if (!is.null(input$filter_status) && input$filter_status != "") {
          notes <- notes[notes$status == input$filter_status, ]
        }
        if (!is.null(input$filter_type) && input$filter_type != "") {
          notes <- notes[notes$note_type == input$filter_type, ]
        }
        notes
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
                ns("add_note"),
                "Add Note",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      # edit --------------------------------------------------------------------
      # shiny::observe({
      #   notes <- notes_data()
      #   for (note in 1:nrow(notes)) {
      #     local({
      #
      #       note_id <- notes$note_id
      #
      #       shiny::observeEvent(input[[paste0("edit_", note_id)]], {
      #         current_edit_id(note_id)
      #         current_note <- notes[notes$id == note_id, ]
      #         shiny::showModal(
      #           shiny::modalDialog(
      #             title = "Edit Note",
      #             size = "l",
      #             shiny::textAreaInput(
      #               ns("edit_text"),
      #               "Note Content",
      #               value = current_note$text,
      #               height = "150px"
      #             ),
      #             shiny::textInput(
      #               ns("edit_tags"),
      #               "Tags (Comma-Separated)",
      #               value = current_note$tags
      #             ),
      #             shiny::selectInput(
      #               ns("edit_status"), "Status",
      #               choices = c(
      #                 "Pending",
      #                 "In Progress",
      #                 "Complete",
      #                 "Cancelled"
      #               ),
      #               selected = current_note$status
      #             ),
      #             footer = htmltools::tagList(
      #               shiny::actionButton(
      #                 ns("save"),
      #                 label = "Save Changes",
      #                 class = "btn-primary",
      #                 icon = shiny::icon("save")
      #               ),
      #               shiny::modalButton("Cancel")
      #             ),
      #             easyClose = TRUE
      #           )
      #         )
      #       })
      #     })
      #   }
      # })

      # save --------------------------------------------------------------------
      # shiny::observeEvent(input$save, {
      #
      #   notes <- notes_data()
      #   note_id <- current_edit_id()
      #
      #   notes[notes$note_id == note_id, "note_content"] <- input$edit_text
      #   notes[notes$note_id == note_id, "note_tags"] <- input$edit_tags
      #   notes[notes$note_id == note_id, "note_status"] <- input$edit_status
      #
      #   shiny::withProgress(
      #     message = "Saving changes...",
      #     detail = "Please wait...",
      #     value = 0,
      #     {
      #       db_update_survey_notes(pool, notes)
      #       shiny::setProgress(1, detail = "Changes saved!")
      #       shiny::showNotification("Note updated successfully!", type = "success")
      #       db_trigger_func()
      #       shiny::removeModal()
      #     }
      #   )
      #
      # })

      # # Handle complete button clicks
      # shiny::observe({
      #   notes <- notes_data()
      #   for (note in 1:nrow(notes)) {
      #     local({
      #       note_id <- notes$id[note]
      #       shiny::observeEvent(input[[paste0("complete_", note_id)]], {
      #         notes <- notes_data()
      #         notes[notes$id == note_id, "status"] <- "completed"
      #         notes_data(notes)
      #         shiny::showNotification("Note marked as complete!", type = "success")
      #       })
      #     })
      #   }
      # })
      #
      # # Handle delete button clicks
      # shiny::observe({
      #   notes <- notes_data()
      #   for (note in 1:nrow(notes)) {
      #     local({
      #       note_id <- notes$id[note]
      #       shiny::observeEvent(input[[paste0("delete_", note_id)]], {
      #         shiny::showModal(shiny::modalDialog(
      #           title = "Confirm Deletion",
      #           "Are you sure you want to delete this note?",
      #           footer = tagList(
      #             shiny::actionButton(session$ns(paste0("confirm_delete_", note_id)),
      #                                 "Delete",
      #                                 class = "btn-danger"
      #             ),
      #             shiny::modalButton("Cancel")
      #           ),
      #           easyClose = TRUE
      #         ))
      #       })
      #
      #       # Handle delete confirmation
      #       shiny::observeEvent(input[[paste0("confirm_delete_", note_id)]], {
      #         notes <- notes_data()
      #         notes_data(notes[notes$id != note_id, ])
      #         shiny::removeModal()
      #         shiny::showNotification("Note deleted successfully!", type = "warning")
      #       })
      #     })
      #   }
      # })

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
      shiny::observeEvent(input$add_note, {
        shiny::req(
          input$note_type,
          input$note_actionable,
          input$note_status,
          input$note_tags,
          input$note_content
        )

        if (nchar(input$note_content) > 0) {
          new_note <- tibble::tibble(
            note_type = input$note_type,
            note_actionable = input$note_actionable,
            note_status = input$note_status,
            note_tags = input$note_tags,
            note_content = input$note_content
          )
        } else {
          shiny::showNotification("Note content cannot be empty!", type = "warning")
          shiny::removeModal()
          return()
        }

        shiny::withProgress(
          message = "Adding note...",
          detail = "Please wait...",
          value = 0,
          {
            # db_insert_survey_notes(pool, new_note)
            shiny::setProgress(1, detail = "Note added!")
            db_trigger_func()
            shiny::removeModal()
          }
        )
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
