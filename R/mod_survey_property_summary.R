
#  ------------------------------------------------------------------------
#
# Title : Survey Property Summary Shiny Module
#    By : Jimmy Briggs
#  Date : 2025-01-12
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Survey Property Summary Shiny Module
#'
#' @name mod_survey_property_summary
#'
#' @description
#' A Shiny Module for the GMH Data Hub's Survey Property Summary page.
#'
#' Includes the following functions:
#'
#' - `mod_survey_property_summary_ui()`: User Interface (UI) for the module.
#' - `mod_survey_property_summary_server()`: Server logic for the module.
#' - `mod_survey_property_summary_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#'
#' @returns
#' - `mod_survey_property_summary_ui()`: UI output
#' - `mod_survey_property_summary_server()`: List of reactive expressions.
#' - `mod_survey_property_summary_demo()`: `NULL`, runs the demo application.
#'
#' @examplesIf interactive()
#' mod_survey_property_summary_demo()
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_survey_property_summary
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
#' @importFrom bslib card
mod_survey_property_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::card(
      full_screen = TRUE,
      style = "overflow-y: visible;",
      class = "property-card shadow-sm",
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        htmltools::tags$h3(
          class = "m-0",
          shiny::textOutput(ns("property_name_title"))
        )
      ),
      bslib::card_body(
        class = "pt-3 pb-0",
        style = "overflow-y: visible;",
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          gap = "1rem",

          # image card display
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              htmltools::tags$h3(
                class = "m-0",
                "Property Image"
              )
            ),
            bslib::card_body(
              style = "overflow-y: visible;",
              htmltools::tags$div(
                class = "position-relative",
                shiny::uiOutput(ns("property_image_card"))
              ),
              htmltools::tags$h3(
                class = "mt-3 mb-2 fw-bold",
                shiny::textOutput(ns("property_name"))
              ),
              shiny::uiOutput(ns("rating_stars")),
              htmltools::tags$p(
                shiny::icon("globe"),
                htmltools::tags$a(
                  shiny::textOutput(ns("website_url"), inline = TRUE),
                  target = "_blank"
                )
              ),
              htmltools::tags$p(
                shiny::icon("location-dot"),
                shiny::textOutput(ns("address"), inline = TRUE)
              ),
              htmltools::tags$p(
                shiny::icon("phone"),
                shiny::textOutput(ns("phone"), inline = TRUE)
              )
            )
          ),

          # property information card
          bslib::card(
            # full_screen = TRUE,
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              htmltools::tags$h3(
                class = "m-0",
                "Property Information"
              ),
              shiny::actionButton(
                ns("edit"),
                "Edit",
                icon = shiny::icon("edit"),
                class = "btn-sm btn-primary"
              )
            ),
            bslib::card_body(
              # Use icons or bold labels
              htmltools::tags$p(
                bsicons::bs_icon("buildings"),
                htmltools::tags$strong(" Developer: "),
                shiny::textOutput(ns("developer"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("person-workspace"),
                htmltools::tags$strong(" Manager: "),
                shiny::textOutput(ns("manager"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("people"),
                htmltools::tags$strong(" Owner: "),
                shiny::textOutput(ns("owner"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("house"),
                htmltools::tags$strong(" Type: "),
                shiny::textOutput(ns("type"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("info-circle"),
                htmltools::tags$strong(" Status: "),
                shiny::textOutput(ns("status"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("graph-up"),
                htmltools::tags$strong(" Comp Status: "),
                shiny::textOutput(ns("comp_status"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("calendar2-date"),
                htmltools::tags$strong(" Year Built: "),
                shiny::textOutput(ns("year_built"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("cash-stack"),
                htmltools::tags$strong(" Last Sale: "),
                shiny::textOutput(ns("last_sale"), inline = TRUE)
              ),
              htmltools::tags$p(
                bsicons::bs_icon("geo-alt"),
                htmltools::tags$strong(" Distance: "),
                shiny::textOutput(ns("distance"), inline = TRUE)
              )
            ),
            bslib::card_footer(
              class = "bg-light",
              htmltools::tags$small(
                class = "text-muted",
                "Need to update info? Click 'Edit'."
              ),
              shiny::actionButton(
                ns("refresh"),
                "Refresh Data",
                icon = shiny::icon("sync"),
                class = "btn-sm btn-outline-primary float-end"
              )
            )
          ),

          # property map card
          bslib::card(
            full_screen = TRUE,
            min_height = "300px",
            bslib::card_header(
              class = "d-flex justify-content-between align-items-center",
              htmltools::tags$h3(
                class = "m-0",
                "Property Map"
              )
            ),
            bslib::card_body(
              class = "p-0",
              leaflet::leafletOutput(ns("property_map"), height = 800) |>
                with_loader(),
              htmltools::tags$small(
                "Click the marker for more information."
              )
            )
          )
        )
      ),
      # footer with last updated at timestamp & refresh button
      bslib::card_footer(
        class = "text-muted d-flex justify-content-between align-items-center",
        htmltools::tags$small(
          "Last Updated:"
        ),
        htmltools::tags$span(
          class = "fw-bold",
          shiny::textOutput(ns("last_updated_at"), inline = TRUE)
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_property_summary
#' @export
#' @importFrom shiny moduleServer reactive
#' @importFrom cli cat_rule
mod_survey_property_summary_server <- function(
  id,
  pool = NULL,
  selected_property_id = NULL
) {

  # validation of reactives
  if (is.null(selected_property_id)) {
    property_id <- db_read_tbl(pool, "mkt.properties", collect = FALSE) |>
      dplyr::filter(.data$is_competitor == FALSE) |>
      dplyr::pull("property_id")
    selected_property_id <- shiny::reactive({ property_id })
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_property_summary_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool
      check_db_conn(pool)

      stopifnot(shiny::is.reactive(selected_property_id))

      db_refresh_trigger <- shiny::reactiveVal(0)

      iv <- property_summary_validator()

      # property data
      property_data <- reactive({
        shiny::req(selected_property_id(), db_refresh_trigger())
        property_id <- selected_property_id()
        db_read_mkt_property_summary(pool, property_id)
      }) |>
        shiny::bindEvent(selected_property_id(), db_refresh_trigger())

      # if property data is empty notify user and launch modal form
      shiny::observe({
        shiny::req(property_data())
        if (nrow(property_data()) == 0) {
          shiny::showNotification(
            "No Survey Data Found for the Selected Property",
            type = "error"
          )
          iv$initialize()
          shinyjs::click("edit")
          shinyjs::enable("save_changes")
        }
      })

      property_map_data <- reactive({
        db_read_mkt_locations(pool, property_data()$property_id)
      })

      shiny::observeEvent(input$refresh, {
        db_refresh_trigger(db_refresh_trigger() + 1)
        shiny::showNotification(
          "Data Refreshed",
          type = "default"
        )
      })

      # inputs data
      input_data <- shiny::reactive({
        tibble::tibble(
          property_id = selected_property_id(),
          property_name = input$property_name_input,
          website = input$website_input,
          address = input$address_input,
          phone_number = input$phone_input,
          property_image = property_data()$property_image,
          developer = input$developer_input,
          manager = input$manager_input,
          owner = input$owner_input,
          property_type = input$type_input,
          property_rating = input$rating_input,
          property_status = input$status_input,
          comp_status = input$comp_status_input,
          year_built = input$year_built_input,
          most_recent_sale = input$last_sale_input,
          distance_from_campus = input$distance_input
        )
      })

      output$property_name_title <- shiny::renderText({
        shiny::req(property_data())
        id <- property_data()$property_id
        name <- property_data()$property_name
        paste0(name, " (", id, ")")
      })

      output$property_image_card <- shiny::renderUI({

        data <- property_data()
        img_src <- data$property_image
        if (is.na(img_src)) { img_src <- "https://placehold.co/600x400.png" }

        bslib::card_image(
          src = img_src,
          alt = "Property Image",
          href = data$website,
          height = "400px",
          width = "auto"
        )
      })

      output$rating_stars <- shiny::renderUI({
        rating <- property_data()$property_rating
        stars <- lapply(1:5, function(i) {
          if (i <= rating) icon("star", class = "text-warning")
          else if (i - 0.5 == rating) icon("star-half-alt", class = "text-warning")
          else icon("star", class = "text-muted")
        })
        htmltools::tags$div(
          class = "d-flex",
          stars,
          htmltools::tags$span(
            class = "ms-2",
            paste0(rating, "/5")
          )
        )
      })

      output$property_map <- leaflet::renderLeaflet({
        map_data <- property_map_data()

        latitude <- map_data$latitude
        longitude <- map_data$longitude

        leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::setView(lng = longitude, lat = latitude, zoom = 15) |>
          leaflet::addMarkers(
            lng = longitude,
            lat = latitude,
            popup = map_data$map_popup_html
          )
      })

      shiny::observe({
        shiny::req(property_map_data())
        map_data <- property_map_data()
        leaflet::leafletProxy("property_map") |>
          leaflet::clearPopups() |>
          leaflet::addPopups(
            lng = map_data$longitude,
            lat = map_data$latitude,
            popup = map_data$map_popup_html
          )
          # TODO: set view up some so full popup is in the view

      })

      output$property_name <- shiny::renderText({ property_data()$property_name })
      output$website_url   <- shiny::renderText({ property_data()$website })
      output$address       <- shiny::renderText({ property_data()$address })
      output$phone         <- shiny::renderText({ property_data()$phone_number })
      output$developer     <- shiny::renderText({ property_data()$developer })
      output$manager       <- shiny::renderText({ property_data()$manager })
      output$owner         <- shiny::renderText({ property_data()$owner })
      output$type          <- shiny::renderText({ property_data()$property_type })
      output$status        <- shiny::renderText({ property_data()$property_status })
      output$comp_status   <- shiny::renderText({ property_data()$comp_status })
      output$year_built    <- shiny::renderText({ property_data()$year_built })

      output$last_sale <- shiny::renderText({
        format(property_data()$most_recent_sale, "%B %Y")
      })

      output$distance <- shiny::renderText({
        paste(property_data()$distance_from_campus, "miles")
      })

      shiny::observeEvent(input$edit, {
        data <- property_data()

        iv$initialize()
        iv$enable()

        if (nrow(data) == 0) {
          prop_id <- selected_property_id()
          gmh_property_data <- db_read_tbl(pool, "gmh.properties", collect = FALSE) |>
            dplyr::filter(property_id == prop_id) |>
            dplyr::collect()
          gmh_location_data <- db_read_tbl(pool, "gmh.locations", collect = FALSE) |>
            dplyr::filter(.data$location_name %in% gmh_property_data$property_name) |>
            dplyr::collect()
          data <- tibble::tibble(
            property_id = prop_id,
            property_name = gmh_property_data$property_name,
            website = gmh_property_data$property_url,
            address = gmh_location_data$address,
            phone_number = gmh_location_data$phone_number,
            property_image = gmh_location_data$image_url,
            developer = "",
            manager = "",
            owner = "",
            property_type = get_survey_choices("property_summary", "property_type")[[1]],
            property_rating = gmh_location_data$gmaps_rating,
            property_status = get_survey_choices("property_summary", "property_status")[[1]],
            comp_status = get_survey_choices("property_summary", "comp_status")[[1]],
            year_built = 1970,
            most_recent_sale = as.Date("1900-01-01"),
            distance_from_campus = 0
          )
        }

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Property",
            size = "l",
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_body(
                  shiny::textInput(
                    session$ns("property_name_input"),
                    "Property Name",
                    value = data$property_name
                  ),
                  shiny::textInput(
                    session$ns("website_input"),
                    "Website URL",
                    value = data$website
                  ),
                  shiny::textInput(
                    session$ns("address_input"),
                    "Address",
                    value = data$address
                  ),
                  shiny::textInput(
                    session$ns("phone_input"),
                    "Phone Number",
                    value = data$phone_number
                  ),
                  shiny::radioButtons(
                    session$ns("image_input_type"),
                    "Image Input Type",
                    choices = c("URL" = "url", "File Upload" = "file"),
                    selected = "url"
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'url'", session$ns("image_input_type")),
                    shiny::textInput(
                      session$ns("image_url_input"),
                      "Image URL",
                      value = data$property_image
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'file'", session$ns("image_input_type")),
                    shiny::fileInput(
                      session$ns("image_file"),
                      "Upload Image",
                      accept = c('image/png', 'image/jpeg', 'image/gif')
                    )
                  )
                )
              ),
              bslib::card(
                bslib::card_body(
                  shiny::selectInput(
                    session$ns("type_input"),
                    "Property Type",
                    choices = c("Student", "Conventional", "Affordable", "Innovative"),
                    selected = data$property_type
                  ),
                  shiny::sliderInput(
                    session$ns("rating_input"),
                    "Rating",
                    min = 0,
                    max = 5,
                    value = data$property_rating %||% 0,
                    step = 0.5
                  ),
                  shiny::selectInput(
                    session$ns("status_input"),
                    "Status",
                    choices = c("New Construction", "Operational", "Undergoing Renovation"),
                    selected = data$property_status
                  ),
                  shiny::numericInput(
                    session$ns("year_built_input"),
                    "Year Built",
                    value = data$year_built,
                    min = 1900,
                    max = 2023
                  ),
                  shiny::sliderInput(
                    session$ns("distance_input"),
                    "Distance (miles)",
                    min = 0,
                    max = 10,
                    value = data$distance_from_campus,
                    step = 0.1
                  )
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              bslib::card(
                bslib::card_header("Review Changes"),
                bslib::card_body(
                  shiny::uiOutput(session$ns("changes_preview"))
                )
              )
            ),
            footer = tagList(
              actionButton(session$ns("save_changes"), "Save Changes", class = "btn-primary") |>
                shinyjs::disabled(),
              modalButton("Cancel")
            ),
            easyClose = TRUE
          ))
      })

      changes <- shiny::reactive({
        req(input_data())
        original_data <- property_data()
        new_values <- input_data()
        if (nrow(original_data) == 0) {
          changes <- "new"
        } else {
          changes <- list()
          for (field in names(new_values)) {
            if (!is.null(new_values[[field]]) && !isTRUE(all.equal(new_values[[field]], original_data[[field]]))) {
              changes[[field]] <- list(
                old = original_data[[field]],
                new = new_values[[field]]
              )
            }
          }
        }
        return(changes)
      })

      shiny::observe({
        req(changes())
        if (length(changes()) > 0) {
          shinyjs::enable("save_changes")
        }
      })

      output$changes_preview <- renderUI({
        req(changes())

        changes_data <- changes()

        if (length(changes_data) == 0) {
          return(p("No changes made"))
        }

        if (changes_data == "new") {
          return(
            strong(
              p("New property being added", style = "color: #007bff;")
            )
          )
        }

        # Create the change preview UI
        changes_ui <- lapply(names(changes_data), function(field) {
          div(
            p(
              strong(paste0(tools::toTitleCase(gsub("_", " ", field)), ":")),
              span(paste("Current:", changes_data[[field]]$old), style = "color: #666;"),
              span("→", style = "margin: 0 10px;"),
              span(paste("New:", changes_data[[field]]$new), style = "color: #007bff;")
            )
          )
        })

        do.call(tagList, changes_ui)
      })

      shiny::observeEvent(input$save_changes, {

        pd <- property_data()
        pid <- selected_property_id()

        new_values <- tibble::tibble(
          property_id         = pid,
          property_name       = input$property_name_input,
          website             = input$website_input,
          address             = input$address_input,
          phone_number        = input$phone_input,
          developer           = input$developer_input,
          manager             = input$manager_input,
          owner               = input$owner_input,
          property_type       = input$type_input,
          property_rating     = input$rating_input,
          property_status     = input$status_input,
          comp_status         = input$comp_status_input,
          year_built          = input$year_built_input,
          most_recent_sale    = input$last_sale_input,
          distance_from_campus = input$distance_input
        )

        # Example DB update (replace with your actual logic)
        db_update_mkt_property_summary(
          pool        = pool,
          property_id = pid,
          new_values  = new_values
        )

        # Trigger a refresh of the property data
        db_refresh_trigger(db_refresh_trigger() + 1)
        shiny::removeModal()
      })

      return(
        list(
          property_data = property_data,
          selected_property_id = selected_property_id
        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_survey_property_summary
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_navbar nav_panel
#' @importFrom bsicons bs_icon
#' @importFrom shiny shinyApp
mod_survey_property_summary_demo <- function(pool = NULL) {

  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Property Summary",
    window_title = "Demo: Survey Property Summary",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Survey Property Summary",
      value = "survey_property_summary",
      icon = bsicons::bs_icon("house"),
      shinyjs::useShinyjs(),
      mod_survey_property_summary_ui("demo")
    )
  )

  server <- function(input, output, session) {
    if (is.null(pool)) pool <- db_connect()
    mod_survey_property_summary_server("demo", pool = pool)
  }

  shiny::shinyApp(ui, server)
}

# utilities ---------------------------------------------------------------

