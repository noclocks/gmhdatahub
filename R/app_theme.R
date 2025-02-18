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
    primary = "#063C6C",
    secondary = "#0E2B4C",
    success = "#28a745",
    danger = "#87202a",
    warning = "#ffc107",
    info = "#6BD3D0",
    light = "#f5f5f5",
    dark = "#031633",
    white = "#ffffff",
    black = "#000000",
    gray = "#6c757d",
    dark_gray = "#2c3e50",
    royal_blue = "#1e4d92"
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
app_theme_ui <- function(preset = "bootstrap", ...) {

  accordion_rules <- glue::glue(
    .open = "{{",
    .close = "}}",
    "/* Styles for the open (expanded) accordion panel */
      .accordion-button:not(.collapsed) {
        background-color: {{gmh_colors('primary')}};
        color: #ffffff;
        font-weight: bold;
      }

      /* Styles for the collapsed accordion panels */
      .accordion-button {
        background-color: {{gmh_colors('light')}};
        color: #000000;
      }"
    )

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
  ) |>
    bslib::bs_add_rules(accordion_rules)
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
  reactable::reactableTheme(
    color = gmh_colors("black"),
    backgroundColor = gmh_colors("white"),
    borderColor = gmh_colors("primary"),
    cellPadding = "8px 12px",
    stripedColor = "#f6f8fa",
    highlightColor = "#e8eef7",
    style = list(fontSize = "0.8rem"),
    headerStyle = reactable_header_style(),
    groupHeaderStyle = reactable_header_groups_style(),
    footerStyle = reactable_footer_style(),
    rowSelectedStyle = reactable_selected_row_style(),
    inputStyle = reactable_input_style(),
    ...
  )
}

reactable_selected_row_style <- function() {
  list(
    backgroundColor = "#d3e1f3",
    boxShadow = paste0("inset 2px 0 0 0 ", gmh_colors("primary"))
  )
}

reactable_input_style <- function() {
  list(
    backgroundColor = gmh_colors("light"),
    color = gmh_colors("dark"),
    border = paste0("1px solid ", gmh_colors("primary"))
  )
}

reactable_header_style <- function() {
  list(
    backgroundColor = gmh_colors("primary"),
    color = gmh_colors("light"),
    fontSize = "16px",
    fontWeight = "bold",
    "&:hover[aria-sort]" = list(
      background = gmh_colors("primary")
    ),
    "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
      background = gmh_colors("primary")
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
}

reactable_footer_style <- function() {
  list(
    background = gmh_colors("primary"),
    color = "white",
    fontWeight = "600"
  )
}

reactable_header_groups_style <- function() {
  list(
    backgroundColor = gmh_colors("primary"),
    color = gmh_colors("light"),
    borderTop = paste0("2px solid ", gmh_colors("primary")),
    borderBottom = paste0("2px solid ", gmh_colors("primary"))
  )
}

reactable_default_col_def <- function(totals = NULL) {
  reactable::colDef(
    align = "center",
    headerVAlign = "center",
    vAlign = "center",
    format = reactable::colFormat(separators = TRUE),
    headerStyle = htmltools::css(
      font_weight = 600,
      border_bottom = "2px solid black"
    ),
    footerStyle = htmltools::css(
      font_weight = 600,
      border_top = "2px solid black"
    ),
    footer = function(values, col_name) {
      totals[[col_name]]
    }
  )
}
