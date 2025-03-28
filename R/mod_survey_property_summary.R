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
#' A Shiny Module for the Market Survey's Property Summary Section.
#'
#' Includes the following functions:
#'
#' - `mod_survey_property_summary_ui()`: User Interface (UI) for the module.
#' - `mod_survey_property_summary_server()`: Server logic for the module.
#' - `mod_survey_property_summary_demo()`: Demo application for the module.
#'
#' @param id Module's namespace ID.
#' @param pool Database connection pool.
#' @param survey_data Reactive data for the survey.
#' @param map_data Reactive data for the map.
#' @param selected_filters Reactive data for the selected filters.
#' @param db_trigger_func Reactive Function to trigger database updates.
#' @param edit_survey_section Reactive Function representing the action button to edit the survey section.
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
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_fluid card card_header card_body layout_columns card_footer
#' @importFrom htmltools tagList tags
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS textOutput actionButton icon uiOutput
mod_survey_property_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    # page --------------------------------------------------------------------
    bslib::page_fluid(
      # card --------------------------------------------------------------------
      bslib::card(
        full_screen = TRUE,
        class = "mx-auto",
        bslib::card_header(
          class = "bg-primary text-white",
          htmltools::tags$h4(
            bsicons::bs_icon("building"),
            htmltools::tags$span(
              "Property Summary - ",
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
            col_widths = c(4, 4, 4),
            gap = "1rem",
            # property information ----------------------------------------------------
            bslib::card(
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::tags$h5(bsicons::bs_icon("info-circle"), " Property Information")
              ),
              bslib::card_body(
                htmltools::tags$small("View and edit various property details."),
                htmltools::tags$p(
                  bsicons::bs_icon("building"),
                  htmltools::tags$strong(" Property Name: "),
                  shiny::textOutput(ns("property_name"), inline = TRUE)
                ),
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
                ),
                htmltools::tags$p(
                  bsicons::bs_icon("star"),
                  htmltools::tags$strong(" Property Rating: "),
                  shiny::textOutput(ns("property_rating"), inline = TRUE)
                ),
                htmltools::tags$p(
                  bsicons::bs_icon("globe"),
                  htmltools::tags$strong(" Website: "),
                  htmltools::tags$a(
                    shiny::textOutput(ns("website_url"), inline = TRUE),
                    href = "#",
                    target = "_blank"
                  )
                ),
                htmltools::tags$p(
                  bsicons::bs_icon("geo-alt"),
                  htmltools::tags$strong(" Address: "),
                  shiny::textOutput(ns("address"), inline = TRUE)
                ),
                htmltools::tags$p(
                  bsicons::bs_icon("envelope"),
                  htmltools::tags$strong(" Email: "),
                  shiny::textOutput(ns("email"), inline = TRUE)
                ),
                htmltools::tags$p(
                  bsicons::bs_icon("phone"),
                  htmltools::tags$strong(" Phone: "),
                  shiny::textOutput(ns("phone"), inline = TRUE)
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              # property image ----------------------------------------------------------
              bslib::card(
                bslib::card_header(
                  class = "bg-primary text-white",
                  htmltools::tags$h5(bsicons::bs_icon("image"), " Property Image")
                ),
                bslib::card_body(
                  style = "overflow-y: visible;",
                  htmltools::tags$small(
                    "Click the image to visit the property's website."
                  ),
                  htmltools::tags$div(
                    class = "position-relative",
                    shiny::uiOutput(ns("property_image_card")),
                    htmltools::tags$br(),
                    shiny::uiOutput(ns("rating_stars"))
                  )
                )
              ),
              # property description ----------------------------------------------------
              bslib::card(
                bslib::card_header(
                  class = "bg-primary text-white",
                  htmltools::tags$p(bsicons::bs_icon("info-circle"), " Property Description")
                ),
                bslib::card_body(
                  htmltools::tags$small("View the property's description and details."),
                  htmltools::tags$p(
                    shiny::textOutput(ns("property_description"))
                  )
                )
              )
            ),
            # property map  -----------------------------------------------------------
            bslib::card(
              full_screen = TRUE,
              min_height = "300px",
              bslib::card_header(
                class = "bg-primary text-white",
                htmltools::tags$h4(bsicons::bs_icon("map"), " Property Map")
              ),
              bslib::card_body(
                htmltools::tags$small("View the property location on the map."),
                leaflet::leafletOutput(ns("property_map"), height = "500px"),
                htmltools::tags$small(
                  "Click the markers for more information."
                )
              )
            )
          )
        ),
        # footer ------------------------------------------------------------------
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
      ),
      # history -----------------------------------------------------------------
      bslib::layout_columns(
        col_widths = c(12),
        bslib::card(
          bslib::card_header(
            class = "bg-primary text-white",
            htmltools::tags$p(bsicons::bs_icon("clock"), " Property Summary History")
          ),
          bslib::card_body(
            htmltools::tags$small("View the property's history and updates."),
            htmltools::tags$p(
              bsicons::bs_icon("calendar2-date"),
              htmltools::tags$strong(" Last Updated: "),
              shiny::textOutput(ns("last_updated_at"), inline = TRUE)
            )
          )
        )
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_survey_property_summary
#' @export
#' @importFrom bslib layout_columns card card_body card_header
#' @importFrom cli cat_rule
#' @importFrom dplyr filter
#' @importFrom htmltools tags tagList
#' @importFrom leaflet renderLeaflet setView addProviderTiles leaflet providers flyTo clearPopups leafletProxy
#' @importFrom lubridate today year
#' @importFrom shiny moduleServer reactiveVal observe req reactive renderText renderUI icon observeEvent showModal
#' @importFrom shiny modalDialog textInput sliderInput dateInput radioButtons conditionalPanel fileInput selectInput
#' @importFrom shiny numericInput textAreaInput uiOutput actionButton modalButton removeModal withProgress incProgress showNotification
#' @importFrom tibble tibble
#' @importFrom tools toTitleCase
mod_survey_property_summary_server <- function(
    id,
    pool = NULL,
    survey_data = NULL,
    map_data = NULL,
    selected_filters = NULL,
    db_trigger_func = NULL,
    edit_survey_section = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # setup ------------------------------------------------------------
      ns <- session$ns
      cli::cat_rule("[Module]: mod_survey_property_summary_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # refresh trigger & validator
      db_refresh_trigger <- shiny::reactiveVal(0)
      iv <- property_summary_validator()

      # filters
      shiny::observe({
        shiny::req(selected_filters)
        if (is.null(selected_filters$competitor_id) && is.null(selected_filters$property_id)) {
          selected_filters$property_id <- 739085
        }
      })

      # data --------------------------------------------------------------------

      # initial section data
      property_data <- shiny::reactive({
        shiny::req(survey_data$property_summary)
        survey_data$property_summary
      })

      # inputs data
      inputs_data <- shiny::reactive({
        shiny::req(
          selected_filters,
          input$property_name_input,
          input$website_input,
          input$address_input,
          input$email_input,
          input$phone_input,
          input$developer_input,
          input$manager_input,
          input$owner_input,
          input$type_input,
          input$rating_input,
          input$status_input,
          input$comp_status_input,
          input$year_built_input,
          input$last_sale_input,
          input$distance_input,
          input$image_url_input,
          input$description_input
        )
        tibble::tibble(
          property_id = selected_filters$property_id,
          competitor_id = selected_filters$competitor_id,
          property_name = input$property_name_input,
          property_website = input$website_input,
          property_address = input$address_input,
          property_email = input$email_input,
          property_phone = input$phone_input,
          property_developer = input$developer_input,
          property_manager = input$manager_input,
          property_owner = input$owner_input,
          property_type = input$type_input,
          property_rating = input$rating_input,
          property_status = input$status_input,
          comp_status = input$comp_status_input,
          year_built = input$year_built_input,
          most_recent_sale = input$last_sale_input,
          distance_from_campus = input$distance_input,
          property_image_url = input$image_url_input,
          property_description = input$description_input
        )
      })

      # UI outputs ------------------------------------------------------------------------------------------------------

      # last updated
      output$last_updated_at <- shiny::renderText({
        shiny::req(property_data())
        property_data()$updated_at |>
          max(na.rm = TRUE) |>
          format("%B %d, %Y %I:%M %p")
      })

      # property name
      output$property_name <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_name
      })

      # property name title
      output$property_name_title <- shiny::renderText({
        shiny::req(property_data())
        prop_id <- property_data()$property_id
        comp_id <- property_data()$competitor_id
        name <- property_data()$property_name
        if (is.na(comp_id)) {
          paste0(name, " (", prop_id, ")")
        } else {
          paste0(name, " (Competitor #", comp_id, ")")
        }
      })

      # website url
      output$website_url <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_website
      })

      # address
      output$address <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_address
      })

      # phone
      output$phone <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_phone |> format_phone_number()
      })

      # developer
      output$developer <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_developer
      })

      # manager
      output$manager <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_manager
      })

      output$owner <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_owner
      })
      output$type <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_type
      })
      output$status <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_status
      })
      output$comp_status <- shiny::renderText({
        shiny::req(property_data())
        property_data()$comp_status
      })
      output$year_built <- shiny::renderText({
        shiny::req(property_data())
        property_data()$year_built
      })
      output$last_sale <- shiny::renderText({
        shiny::req(property_data())
        sale_date <- property_data()$most_recent_sale
        if (is.na(sale_date)) {
          return("N/A")
        } else {
          format(sale_date, "%B %Y")
        }
      })
      output$distance <- shiny::renderText({
        shiny::req(property_data())
        paste(round(property_data()$distance_from_campus, 2), "Miles")
      })
      output$property_image_card <- shiny::renderUI({
        data <- property_data()

        img_src <- data$property_image_url %||% "https://placehold.co/600x400.png"
        # img_src <- get_property_image_url(pool, data$property_id) |>

        htmltools::tags$img(
          src = img_src,
          class = "img-fluid",
          alt = "Property Image",
          href = data$property_website,
          target = "_blank",
          style = "cursor: pointer; border-radius: 6px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);",
          height = "auto",
          width = "100%"
        )
      })
      output$property_rating <- shiny::renderText({
        shiny::req(property_data())
        paste0(
          property_data()$property_rating,
          " / 5 Stars"
        )
      })
      output$rating_stars <- shiny::renderUI({
        rating <- ifelse(
          length(property_data()$property_rating) == 0,
          0,
          property_data()$property_rating
        )
        stars <- lapply(1:5, function(i) {
          if (i <= rating) {
            shiny::icon("star", class = "text-warning")
          } else if (i - 0.5 == rating) {
            shiny::icon("star-half-alt", class = "text-warning")
          } else {
            shiny::icon("star", class = "text-muted")
          }
        })
        htmltools::tags$span(stars, style = "font-size: 1.25rem;")
      })
      output$property_description <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_description
      })
      output$email <- shiny::renderText({
        shiny::req(property_data())
        property_data()$property_email
      })

      # map ---------------------------------------------------------------------
      output$property_map <- leaflet::renderLeaflet({
        shiny::req(map_data())

        if (nrow(map_data()) == 0) {
          return(
            leaflet::leaflet() |>
              leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
              leaflet::setView(lng = -98.35, lat = 39.5, zoom = 4)
          )
        }

        map_data_split <- map_data() |> split(~map_layer)

        properties <- map_data_split$properties
        competitors <- map_data_split$competitors
        universities <- map_data_split$universities

        map_survey_locations(
          properties = properties,
          competitors = competitors,
          universities = universities
        )
      })

      shiny::observeEvent(
        list(
          selected_filters$property_id,
          selected_filters$competitor_id
        ),
        {
          shiny::req(map_data())

          if (is.null(selected_filters$competitor_id)) {
            prop_name <- get_property_name_by_id(selected_filters$property_id)
            selected_data <- map_data() |> dplyr::filter(location_name == prop_name)
          } else {
            comp_name <- get_competitor_name_by_id(selected_filters$competitor_id)
            selected_data <- map_data() |> dplyr::filter(location_name == comp_name)
          }

          if (nrow(selected_data) == 0) {
            return()
          }

          leaflet::leafletProxy("property_map") |>
            leaflet::clearPopups() |>
            leaflet::flyTo(
              lng = selected_data$longitude,
              lat = selected_data$latitude,
              zoom = 15
            )
        },
        ignoreInit = TRUE
      )

      # edit modal --------------------------------------------------------------
      shiny::observeEvent(edit_survey_section(), {
        shiny::req(session$userData$selected_survey_tab(), property_data())

        if (session$userData$selected_survey_tab() != "nav_property_summary") {
          return()
        }

        data <- property_data()

        iv$initialize()
        iv$enable()

        shiny::showModal(
          shiny::modalDialog(
            title = "Edit Property Information",
            size = "xl",
            easyClose = TRUE,
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(
                bslib::card_body(
                  shiny::textInput(
                    ns("property_name_input"),
                    "Property Name",
                    value = data$property_name
                  ),
                  shiny::textInput(
                    session$ns("website_input"),
                    "Website URL",
                    value = data$property_website
                  ),
                  shiny::textInput(
                    session$ns("address_input"),
                    "Address",
                    value = data$property_address
                  ),
                  shiny::textInput(
                    session$ns("phone_input"),
                    "Phone Number",
                    value = data$property_phone
                  ),
                  shiny::textInput(
                    session$ns("email_input"),
                    "Email Address",
                    value = data$property_email
                  ),
                  shiny::sliderInput(
                    session$ns("rating_input"),
                    "Rating",
                    min = 0,
                    max = 5,
                    step = 0.5,
                    value = ifelse(
                      length(data$property_rating) == 0,
                      0,
                      data$property_rating %||% 0
                    )
                  ),
                  shiny::dateInput(
                    session$ns("last_sale_input"),
                    "Most Recent Sale",
                    value = ifelse(
                      is.na(data$most_recent_sale),
                      lubridate::today(),
                      data$most_recent_sale
                    )
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
                      value = data$property_image_url
                    )
                  ),
                  shiny::conditionalPanel(
                    condition = sprintf("input['%s'] == 'file'", session$ns("image_input_type")),
                    shiny::fileInput(
                      session$ns("image_file"),
                      "Upload Image",
                      accept = c("image/png", "image/jpeg", "image/gif")
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
                  shiny::selectInput(
                    session$ns("status_input"),
                    "Status",
                    choices = c("New Construction", "Operational", "Undergoing Renovation"),
                    selected = data$property_status
                  ),
                  shiny::selectInput(
                    session$ns("comp_status_input"),
                    "Comp Status",
                    choices = get_survey_choices(section = "property_summary", type = "comp_status"),
                    selected = data$comp_status
                  ),
                  shiny::textInput(
                    session$ns("developer_input"),
                    "Developer",
                    value = data$property_developer
                  ),
                  shiny::textInput(
                    session$ns("manager_input"),
                    "Manager",
                    value = data$property_manager
                  ),
                  shiny::textInput(
                    session$ns("owner_input"),
                    "Owner",
                    value = data$property_owner
                  ),
                  shiny::numericInput(
                    session$ns("year_built_input"),
                    "Year Built",
                    min = 1970,
                    max = lubridate::year(lubridate::today()),
                    value = ifelse(
                      length(data$year_built) == 0,
                      lubridate::year(lubridate::today()),
                      data$year_built
                    )
                  ),
                  shiny::sliderInput(
                    session$ns("distance_input"),
                    "Distance (miles)",
                    min = 0,
                    max = 10,
                    step = 0.1,
                    value = ifelse(
                      length(data$distance_from_campus) == 0,
                      0,
                      data$distance_from_campus
                    )
                  )
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(12),
              shiny::textAreaInput(
                session$ns("description_input"),
                "Description",
                value = data$property_description,
                rows = 5
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
            footer = htmltools::tagList(
              shiny::actionButton(
                ns("save"),
                "Save",
                class = "btn-primary"
              ),
              shiny::modalButton("Cancel")
            )
          )
        )
      })

      # changes -----------------------------------------------------------------
      changes <- shiny::reactive({
        shiny::req(property_data(), inputs_data())

        original_data <- property_data()
        new_data <- inputs_data()

        changes <- purrr::map(
          names(new_data),
          function(field) {
            if (!is.na(new_data[[field]]) && !isTRUE(all.equal(new_data[[field]], original_data[[field]]))) {
              old_value <- if (is.numeric(original_data[[field]])) {
                round(original_data[[field]], 2)
              } else {
                original_data[[field]]
              }
              new_value <- if (is.numeric(new_data[[field]])) {
                round(new_data[[field]], 2)
              } else {
                new_data[[field]]
              }
              if (!isTRUE(all.equal(old_value, new_value))) {
                list(
                  field = field,
                  old = old_value,
                  new = new_value
                )
              }
            }
          }
        ) |>
          rlang::set_names(names(new_data)) |>
          purrr::compact()

        changes

      })

      shiny::observe({
        shiny::req(changes())
        num_changes <- length(changes())
        cli::cli_alert_info("{.field {num_changes}} changes detected in the Property Summary.")
        cli::cli_alert_info(changes())
      })

      # changes preview UI
      output$changes_preview <- shiny::renderUI({
        shiny::req(changes())

        changes_data <- changes()

        if (length(changes_data) == 0) {
          return(htmltools::tags$p("No changes detected.", class = "text-muted"))
        }

        changes_ui <- lapply(names(changes_data), function(field) {
          htmltools::tags$div(
            htmltools::tags$p(
              htmltools::tags$strong(
                paste0(
                  tools::toTitleCase(
                    gsub("_", " ", field)
                  ),
                  ":"
                )
              ),
              htmltools::tags$span(
                paste(
                  "Current:",
                  changes_data[[field]]$old
                ),
                style = "color: #666;"
              ),
              htmltools::tags$span(
                "→",
                style = "margin: 0 10px;"
              ),
              htmltools::tags$span(
                paste(
                  "New:",
                  changes_data[[field]]$new
                ),
                style = "color: #007bff;"
              )
            )
          )
        })
        do.call(htmltools::tagList, changes_ui)
      })

      # save --------------------------------------------------------------------
      shiny::observeEvent(input$save, {
        shiny::req(changes())

        if (!is.na(selected_filters$competitor_id) && !is.null(selected_filters$competitor_id)) {
          prop_id <- NULL
          comp_id <- selected_filters$competitor_id
        } else {
          prop_id <- selected_filters$property_id
          comp_id <- NULL
        }

        new_values <- tibble::tibble(
          property_id = prop_id,
          competitor_id = comp_id,
          property_name = input$property_name_input,
          property_website = input$website_input,
          property_address = input$address_input,
          property_email = input$email_input,
          property_phone = input$phone_input,
          property_developer = input$developer_input,
          property_manager = input$manager_input,
          property_owner = input$owner_input,
          property_type = input$type_input,
          property_rating = input$rating_input,
          property_status = input$status_input,
          comp_status = input$comp_status_input,
          year_built = input$year_built_input,
          most_recent_sale = input$last_sale_input,
          distance_from_campus = input$distance_input,
          property_image_url = input$image_url_input,
          property_description = input$description_input,
          updated_by = selected_filters$user_id
        )

        db_update_survey_property_summary(pool, new_values)

        # Trigger a refresh of the property data
        db_refresh_trigger(db_refresh_trigger() + 1)
        db_trigger_func()
        shiny::removeModal()
      })



      # # enable "Save" button only if changes exist
      # shiny::observe({
      #   shinyjs::toggleState("save", length(changes()) > 0)
      # })

      # refresh -----------------------------------------------------------------
      shiny::observeEvent(
        list(
          input$refresh,
          db_refresh_trigger()
        ),
        {
          shiny::withProgress(
            message = "Refreshing Data...",
            detail = "Please wait...",
            value = 0,
            {
              shiny::incProgress(1 / 2, detail = "Refreshing Data...")
              survey_data$property_summary <- db_read_survey_property_summary(
                pool,
                property_id = selected_filters$property_id,
                competitor_id = selected_filters$competitor_id
              )
              shiny::incProgress(1 / 2, detail = "Data Refreshed")
            }
          )
          shiny::showNotification("Data Refreshed", type = "default")
        },
        ignoreInit = TRUE
      )

      # shiny::observe({
      #   shiny::req(property_data())
      #   cli::cli_alert_info("Property Summary Data Updated in Module:")
      #   dplyr::glimpse(property_data())
      # }) |> shiny::bindEvent(property_data())


      # return ------------------------------------------------------------------
      list(
        property_data = property_data,
        changes = changes
      )
    }
  )
}

# demo --------------------------------------------------------------------

#' @rdname mod_survey_property_summary
#' @export
#' @importFrom bsicons bs_icon
#' @importFrom bslib page_navbar nav_spacer nav_panel
#' @importFrom pkgload load_all
#' @importFrom shiny actionButton icon reactive reactiveValues isolate reactiveVal shinyApp
#' @importFrom shinyjs useShinyjs
mod_survey_property_summary_demo <- function(pool = NULL) {
  pkgload::load_all()

  ui <- bslib::page_navbar(
    title = "Demo: Survey Property Summary",
    window_title = "Property Summary",
    theme = app_theme_ui(),
    lang = "en",
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = "Property Summary",
      value = "survey_property_summary",
      icon = bsicons::bs_icon("house"),
      shinyjs::useShinyjs(),
      shiny::actionButton(
        "edit_survey_section",
        "Edit",
        icon = shiny::icon("edit"),
        style = "width: auto;",
        class = "btn-sm btn-primary"
      ),
      mod_survey_property_summary_ui("demo")
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

    map_data <- shiny::reactive({
      db_read_gmh_map_data(
        pool,
        property_id = selected_filters$property_id
      )
    })

    survey_data <- shiny::reactiveValues(
      property_summary = db_read_survey_property_summary(pool, property_id = shiny::isolate(selected_filters$property_id))
    )

    session$userData$selected_survey_tab <- shiny::reactiveVal("nav_property_summary")

    db_trigger_func <- function() {
      survey_data$property_summary <- db_read_survey_property_summary(
        pool,
        property_id = selected_filters$property_id
      )
    }

    mod_survey_property_summary_server(
      "demo",
      pool = pool,
      survey_data = survey_data,
      map_data = map_data,
      selected_filters = selected_filters,
      db_trigger_func = db_trigger_func,
      edit_survey_section = edit_survey_section
    )
  }

  shiny::shinyApp(ui, server)
}
