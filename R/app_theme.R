#  ------------------------------------------------------------------------
#
# Title : Shiny App Theme
#    By : Jimmy Briggs
#  Date : 2024-12-13
#
#  ------------------------------------------------------------------------


# colors ------------------------------------------------------------------

#' GMH Colors
#'
#' @description
#' Function for GMH color palette.
#'
#' @param ... The colors to return.
#'
#' @returns
#' A vector of colors.
#'
#' @export
#'
#' @examples
#' gmh_colors("primary", "secondary")
gmh_colors <- function(...) {
  colors <- c(
    primary = "#0e2b4c",
    secondary = "#6c757d",
    success = "#28a745",
    danger = "#dc3545",
    warning = "#ffc107",
    info = "#17a2b8",
    light = "#f8f9fa",
    dark = "#343a40",
    white = "#ffffff",
    black = "#000000",
    gray = "#6c757d"
  )
  dots <- list(...)
  if (length(dots) == 0) {
    return(colors)
  } else {
    requested_colors <- unlist(dots)
    return(unname(unlist(colors[requested_colors])))
  }
}

#' Chart Colors
#'
#' @description
#' Function for GMH chart color palette.
#'
#' @param ... The colors to return.
#'
#' @returns
#' A vector of colors.
#'
#' @export
#'
#' @examples
#' chart_colors("primary", "secondary")
chart_colors <- function(...) {
  colors <- c(
    "primary" = "#0e2b4c",
    "secondary" = "#18BC9C",
    "tertiary" = "#86a5b1",
    "accent1" = "#f8b400",
    "accent2" = "#f78e69",
    "accent3" = "#2a9d8f",
    "accent4" = "#e76f51",
    "accent5" = "#264653",
    "accent6" = "#457b9d",
    "accent7" = "#a8dadc",
    "accent8" = "#f4a261",
    "accent9" = "#e9c46a",
    "accent10" = "#f4a261"
  )

  dots <- list(...)

  if (length(dots) == 0) {
    return(colors)
  } else {
    requested_colors <- unlist(dots)
    return(unname(unlist(colors[requested_colors])))
  }
}



# themes ------------------------------------------------------------------

#' App Theme
#'
#' @description
#' This function generates a custom Bootstrap theme for the GMH DataHub Shiny app.
#'
#' @inheritParams bslib::bs_theme
#' @inheritDotParams bslib::bs_theme
#'
#' @returns
#' A Bootstrap theme generated via [bslib::bs_theme()] leveraging the GMH Communities brand assets.
#'
#' @export
#'
#' @seealso [preview_app_theme()] to preview the theme.
#'
#' @importFrom bslib bs_theme
app_theme_ui <- function(preset = "shiny", ...) {
  bslib::bs_theme(
    version = 5,
    preset = preset,
    spacer = "1rem",
    "enable-shadows" = TRUE,
    bg = gmh_colors("white"),
    fg = gmh_colors("black"),
    primary = gmh_colors("primary"),
    secondary = gmh_colors("secondary"),
    success = gmh_colors("success"),
    info = gmh_colors("info"),
    warning = gmh_colors("warning"),
    danger = gmh_colors("danger"),
    light = gmh_colors("light"),
    dark = gmh_colors("dark"),
    ...
  )
}

#' Preview App Theme
#'
#' @description
#' This function previews the GMH DataHub Shiny app theme via [bslib::bs_theme_preview()].
#'
#' @inheritDotParams bslib::bs_theme_preview
#'
#' @returns
#' A preview of the GMH DataHub Shiny app theme.
#'
#' @export
#'
#' @importFrom bslib bs_theme_preview
preview_app_theme <- function(...) {
  bslib::bs_theme_preview(
    theme = app_theme_ui(),
    with_themer = TRUE,
    ...
  )
}


#' Reactable Theme
#'
#' @description
#' This function generates a custom Reactable theme for the GMH DataHub Shiny app.
#'
#' @param ... Named arguments to pass to the Reactable theme.
#'
#' @returns
#' A [reactable::reactableTheme()] leveraging the GMH Communities brand assets.
#'
#' @export
#'
#' @importFrom reactable reactableTheme
reactable_theme <- function(...) {
  header_style <- list(
    backgroundColor = gmh_colors("primary"),
    color = gmh_colors("light"),
    fontSize = "16px",
    fontWeight = "bold",
    "&:hover[aria-sort]" = list(
      background = "hsl(0, 0%, 96%)"
    ),
    "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
      background = "hsl(0, 0%, 96%)"
    ),
    "&[aria-sort='ascending']::after" = list(
      content = "' ▲'",
      display = "inline-block"
    ),
    "&[aria-sort='descending']::after" = list(
      content = "' ▼'",
      display = "inline-block"
    )
  )

  reactable::reactableTheme(
    color = gmh_colors("black"),
    backgroundColor = gmh_colors("white"),
    borderColor = gmh_colors("primary"),
    stripedColor = "#f6f8fa",
    highlightColor = "#e8eef7",
    cellPadding = "8px 12px",
    style = list(fontSize = "0.8rem"),
    headerStyle = header_style,
    rowSelectedStyle = list(
      backgroundColor = "#d3e1f3",
      boxShadow = "inset 2px 0 0 0 #0e2b4c"
    ),
    inputStyle = list(
      backgroundColor = "#ffffff",
      color = "#000000",
      border = "1px solid #0e2b4c"
    ),
    searchInputStyle = list(width = "100%"),
    ...
  )
}
