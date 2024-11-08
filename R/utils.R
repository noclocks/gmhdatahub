
#  ------------------------------------------------------------------------
#
# Title : Utilities
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

icon_text <- function(icon, text, .function = shiny::icon) {
  if (is.character(icon)) i <- .function(icon) else i <- icon
  t <- paste0(" ", text)
  htmltools::tagList(htmltools::tags$div(i, t))
}
