app_favicon <- function(path = "www/favicon.ico") {

  favicon_mime_type <- mime::guess_type(
    path,
    mime_extra = c("ico" = "image/x-icon")
  )

  htmltools::tags$head(
    htmltools::tags$link(
      rel = "icon",
      type = favicon_mime_type,
      href = path
    )
  )

}

app_header_ui <- function(title = "GMH Data Hub Platform") {
  htmltools::tags$div(
    class = "px-4 py-3 app-header",
    htmltools::tags$div(
      class = "d-flex align-items-center",
      bsicons::bs_icon("building-fill", size = "1.5rem", class = "me-2"),
      htmltools::h3(title, class = "m-0")
    ),
    htmltools::p(
      class = "m-0 mt-1",
      "Your centralized platform for student housing portfolio analytics and insights."
    )
  )
}

app_title_ui <- function() {
  htmltools::tags$span(
    class = "navbar-brand",
    htmltools::tags$img(
      src = "www/logo.svg",
      alt = "GMH Communities Logo Dark",
      width = "auto",
      height = 50,
      class = "logo-light"
    )
  )
}

app_footer_ui <- function() {
  tags$footer(
    class = "footer py-3",  # Removed mt-auto and bg-light classes
    tags$div(
      class = "container",
      tags$span(
        "© 2024 GMH DataHub"  # Removed text-muted class
      )
    )
  )
}


