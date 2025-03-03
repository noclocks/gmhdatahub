
#  ------------------------------------------------------------------------
#
# Title : Shiny Related Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------

#' With Tooltip
#'
#' @description
#' Add a Tooltip to a Shiny Input.
#'
#' @param input The input to add the tooltip to.
#' @param tooltip_text The text to display in the tooltip.
#' @inheritParams bslib::tooltip
#' @param ... Additional arguments to pass to [bslib::tooltip()].
#'
#' @returns
#' The input with the tooltip added.
#'
#' @export
#'
#' @seealso [bslib::tooltip()]
#'
#' @importFrom bslib tooltip
#' @importFrom bsicons bs_icon
#' @importFrom htmltools tags
#'
#' @examples
#' shiny::selectInput(
#'   inputId = "example",
#'   label = "Example",
#'   choices = c("A", "B", "C"),
#'   selected = "A"
#' ) |>
#'   with_tooltip("This is an example tooltip.")
with_tooltip <- function(
    input,
    tooltip_text,
    placement = c("auto", "top", "right", "bottom", "left"),
    ...
) {

  placement <- rlang::arg_match(placement)

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

#' With Loader
#'
#' @description
#' Add a Loader to a Shiny Output or UI Element.
#'
#' @inheritParams shinycustomloader::withLoader
#' @param ... Additional arguments to pass to [shinycustomloader::withLoader()].
#'
#' @returns
#' The output or UI element with the loader added.
#'
#' @importFrom shinycustomloader withLoader
#'
#' @seealso [shinycustomloader::withLoader()]
#'
#' @export
with_loader <- function(
    ui_element,
    ...
) {
  shinycustomloader::withLoader(
    ui_element,
    ...
  )
}

#' Icon Text
#'
#' @description
#' Create an HTML element with an icon and text.
#'
#' - `icon_text()`: Icon followed by text.
#' - `text_icon()`: Text followed by icon.
#'
#' @param icon Character string for the icon name.
#' @param text Character string for the text.
#' @param .function Function to create the icon. Defaults to [shiny::icon()].
#'
#' @returns
#' HTML element with the icon and text.
#'
#' @importFrom htmltools tags
#'
#' @export
#'
#' @examples
#' icon_text("home", "Home")
#' text_icon("Home", "home")
icon_text <- function(icon, text, .function = shiny::icon) {

  stopifnot(is.function(.function))

  if (is.character(icon) && length(icon) == 1) {
    icon <- .function(icon)
  }

  text <- paste0(" ", text)

  htmltools::tags$span(
    icon,
    text
  )
}

#' @rdname icon_text
#' @export
#' @importFrom htmltools tags
text_icon <- function(text, icon, .function = shiny::icon) {

  stopifnot(is.function(.function))

  icon <- .function(icon)
  text <- paste0(text, " ")

  htmltools::tags$span(
    text,
    icon
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
