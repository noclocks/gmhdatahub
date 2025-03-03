
format_status <- function(value) {
  class <- paste0("badge status-", tolower(value))
  htmltools::tags$div(class = class, value)
}

status_badge <- function(color = "#aaa", width = "0.55rem", height = width) {
  htmltools::tags$span(style = list(
    display = "inline-block",
    marginRight = "0.5rem",
    width = width,
    height = height,
    backgroundColor = color,
    borderRadius = "50%"
  ))
}

rating_stars <- function(rating, max_rating = 5) {
  star_icon <- function(empty = FALSE) {
    htmltools::tagAppendAttributes(
      shiny::icon("star"),
      style = paste("color:", if (empty) "#edf0f2" else "orange"),
      "aria-hidden" = "true"
    )
  }
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) star_icon() else star_icon(empty = TRUE)
  })
  label <- sprintf("%s out of %s stars", rating, max_rating)
  htmltools::tags$div(title = label, role = "img", stars)
}

format_boolean <- function(value) {
  if (is.null(value) || is.na(value)) {
    return("N/A")
  }
  icon <- if (value) "✓" else "✗"
  div(
    style = list(
      color = if (value) gmh_colors("success") else gmh_colors("danger"),
      fontWeight = "bold"
    ),
    icon
  )
}

html_tel <- function(phone) {
  phone_digits <- gsub("[^0-9]", "", phone)
  glue::glue("<a href='tel:{phone_digits}' target='_blank'>{phone}</a>")
}

format_address <- function(address) {
  address_parts <- strsplit(address, ", ")[[1]]
  street <- address_parts[1]
  city <- address_parts[2]
  state <- address_parts[3]
  zip <- address_parts[4]
  city_state_zip <- glue::glue("{city}, {state}, {zip}")
  glue::glue("{street}\n{city_state_zip}") |> cat()
}

format_phone_number <- function(phone) {
  phone_digits <- gsub("[^0-9]", "", phone)
  if (nchar(phone_digits) == 11) {
    phone_digits <- substr(phone_digits, 2, 11)
  }
  glue::glue("+1 ({substr(phone_digits, 1, 3)}) {substr(phone_digits, 4, 6)}-{substr(phone_digits, 7, 10)}")
}
