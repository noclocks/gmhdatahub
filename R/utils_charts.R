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
