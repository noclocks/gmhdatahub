welcome_banner_ui <- function() {
  bslib::layout_columns(
    col_widths = c(12),
    bslib::card(
      class = "my-3",
      bslib::card_header(
        class = "bg-primary",
        htmltools::tags$h2(
          "Welcome to GMH Communities Data Hub",
          class = "mb-0"
        )
      ),
      bslib::card_body(
        htmltools::tags$p(
          class = "lead",
          "Your centralized platform for student housing portfolio analytics and insights"
        )
      )
    )
  )
}

gmaps_properties_map_embed_iframe <- function(width = "100%", height = "500px") {

  url <- "https://www.google.com/maps/d/embed?mid=19tP5Bf66khGcrNnqTBsk879W2fS-u7U&ehbc=2E312F"

  htmltools::tags$iframe(
    src = url,
    width = width,
    height = height
  )

}

with_loader <- function(x) { shinycustomloader::withLoader(x) }

with_tooltip <- function(input, tooltip_text, placement = "right") {

  current_label <- input$children[[1]]$children[[1]]

  updated_label <- htmltools::tags$span(
    current_label,
    bslib::tooltip(
      bsicons::bs_icon("info-circle", style = "cursor: help;"),
      tooltip_text,
      placement = placement
    )
  )

  input$children[[1]]$children[[1]] <- updated_label

  return(input)
}

icon_text <- function(icon, text, .function = shiny::icon) {

  if (is.character(icon) && length(icon) == 1) {
    icon <- .function(icon)
  }

  text <- paste0(" ", text)

  htmltools::tags$span(
    icon,
    text
  )

}

text_icon <- function(text, icon, .function = shiny::icon) {

  icon <- .function(icon)
  text <- paste0(text, " ")

  htmltools::tags$span(
    text,
    icon
  )

}

parse_request <- function(req) {

  http_method <- req$REQUEST_METHOD
  path_info <- req$PATH_INFO
  protocol <- req$HTTP_X_FORWARDED_PROTO

  list(
    http_method = http_method,
    path_info = path_info,
    protocol = protocol
  )

}

get_default_app_choices <- function(type) {
  type <- rlang::arg_match0(type, names(app_choices_lst))
  app_choices_lst[[type]]
}

get_survey_choices <- function(section, type) {
  section <- rlang::arg_match0(section, names(survey_choices_lst))
  type <- rlang::arg_match0(type, names(survey_choices_lst[[section]]))
  survey_choices_lst[[section]][[type]]
}

get_property_name_by_id <- function(property_id) {

  valid_property_ids <- get_default_app_choices("properties")
  if (!property_id %in% valid_property_ids) {
    cli::cli_abort("{.arg property_id} is not a valid property ID.")
  }

  names(
    get_default_app_choices("properties")
  )[which(get_default_app_choices("properties") == property_id)]

}

get_property_id_by_name <- function(property_name) {

  valid_property_names <- names(get_default_app_choices("properties"))
  if (!property_name %in% valid_property_names) {
    cli::cli_abort("{.arg property_name} is not a valid property name.")
  }

  get_default_app_choices("properties")[[property_name]]

}
