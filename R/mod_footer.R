
#  ------------------------------------------------------------------------
#
# Title : Shiny App Footer Module
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------




# topic -------------------------------------------------------------------

#' Footer Module
#'
#' @name mod_footer
#'
#' @description
#' A Shiny module for creating the footer of the GMH Data Hub's Leasing Dashboard.
#'
#' Composed of the `ui`, `server`, and helper functions:
#'
#' - `mod_footer_ui()`: The UI function for the footer which defines the app's
#'   HTML `<footer>` element. Wraps [bslib::card_footer()] with custom footer
#'   content specific to the app.
#' - `mod_footer_server()`: The server function for the footer, currently
#'   empty as no server-side logic is required.
#' - `footer_top_section()`: Helper function for the top section of the footer.
#' - `footer_client_section()`: Helper function for the client section of the footer.
#' - `footer_logo_section()`: Helper function for the logo section of the footer.
#' - `footer_copyright_section()`: Helper function for the copyright section of the footer.
#' - `validate_image()`: Helper function to validate image paths.
#'
#' @param id The module's namespace ID.
#' @param align The alignment of the footer content. Default is `"center"`.
#' @param class The CSS class for the footer. Default is `"footer"`.
#' @param app_info A list containing app-specific information:
#'   - `name`: The name of the app.
#'   - `version`: The version of the app.
#'   - `logo`: The logo URL for the app.
#'   - `symbol`: The symbol URL for the app.
#'   - `repo_url`: The URL for the app's GitHub repository.
#'   - `docs_url`: The URL for the app's documentation.
#' @param client_info A list containing client-specific information:
#'   - `name`: The name of the client or organization.
#'   - `url`: The URL for the client or organization.
#'   - `logo`: The URL or file path for the client logo.
#'   - `symbol`: The URL or file path for the client symbol.
#' @param developer_info A list containing developer-specific information:
#'   - `name`: The name of the developer or development team.
#'   - `url`: The URL for the developer or development team.
#'   - `logo`: The URL or file path for the developer's logo.
#'   - `symbol`: The URL or file path for the developer's symbol.
#' @param copyright_holder The name of the copyright holder.
#' @param year The year to display in the footer. Defaults to the current year.
#' @param ... Additional arguments passed to [bslib::card_footer()].
#'
#' @return
#' - `mod_footer_ui()`: [htmltools::div()] tag with the app's `<footer>` content
#' - `mod_footer_server()`: Server logic for the footer (currently empty)
#' - `footer_top_section()`: [htmltools::div()] tag for the top section of the footer
#' - `footer_client_section()`: [htmltools::div()] tag for the client section of the footer
#' - `footer_logo_section()`: [htmltools::div()] tag for the logo section of the footer
#' - `footer_copyright_section()`: [htmltools::div()] tag for the copyright section of the footer
#' - `validate_image()`: Validates image paths and returns the correct path or a placeholder image path.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(bslib)
#'   ui <- bslib::page_fluid(
#'     title = "Footer Module",
#'     theme = bslib::bs_theme(version = 5),
#'     lang = "en",
#'     mod_footer_ui("footer")
#'   )
#'
#'   server <- function(input, output, session) {
#'     mod_footer_server("footer")
#'   }
#'
#'   shinyApp(ui, server)
#' }
NULL

# ui ----------------------------------------------------------------------

#' @rdname mod_footer
#' @export
#' @importFrom htmltools tags HTML
#' @importFrom bslib card_footer layout_columns
#' @importFrom shiny icon NS includeCSS
#' @importFrom fontawesome fa
mod_footer_ui <- function(
    id = NULL,
    align = "center",
    class = "footer",
    app_info = gmhdatahub::app_info,
    client_info = client_info,
    developer_info = developer_info,
    copyright_holder = "No Clocks, LLC",
    year = format(Sys.Date(), "%Y"),
    ...
) {

  ns <- shiny::NS(id)

  # Ensure the 'www' directory exists within the package
  www_dir <- system.file("www", package = "gmhdatahub")
  if (nzchar(www_dir) && !"www" %in% names(shiny::resourcePaths())) {
    shiny::addResourcePath("www", www_dir)
  } else if (!nzchar(www_dir)) {
    stop("The 'www' directory was not found in the 'gmhdatahub' package.")
  }

  # Include the CSS file from the package's 'www' directory
  css_file <- system.file("www/styles/css/footer.css", package = "gmhdatahub")
  if (file.exists(css_file)) {
    css_content <- shiny::includeCSS(css_file)
  } else {
    warning("CSS file 'footer.css' not found in the 'gmhdatahub' package.")
    css_content <- NULL
  }

  # Set default copyright holder if not provided
  if (is.null(copyright_holder)) {
    copyright_holder <- developer_info$name
  }

  # Validate images
  app_info$logo <- validate_image(app_info$logo)
  app_info$symbol <- validate_image(app_info$symbol)
  client_info$logo <- validate_image(client_info$logo)
  client_info$symbol <- validate_image(client_info$symbol)
  developer_info$logo <- validate_image(developer_info$logo)
  developer_info$symbol <- validate_image(developer_info$symbol)

  # Entrata
  entrata_info <- .entrata_info
  entrata_info$logo <- validate_image(entrata_info$logo)

  bslib::card_footer(
    class = class,
    css_content,
    htmltools::tags$footer(
      style = paste0("text-align: ", align, ";"),
      htmltools::tags$hr(),
      footer_top_section(app_info, developer_info),
      htmltools::tags$br(),
      footer_client_section(client_info),
      htmltools::tags$hr(),
      footer_copyright_section(year, copyright_holder, developer_info),
      htmltools::tags$hr(),
      footer_logo_section(developer_info, client_info, entrata_info)
    )
  )
}

# server ------------------------------------------------------------------

#' @rdname mod_footer
#' @export
#' @importFrom shiny moduleServer
mod_footer_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Add any necessary server logic for the footer
  })
}

# helper functions --------------------------------------------------------

#' @rdname mod_footer
#' @export
#' @importFrom htmltools tags HTML
#' @importFrom shiny icon
#' @importFrom fontawesome fa
footer_top_section <- function(app_info, developer_info) {
  htmltools::tags$div(
    htmltools::tags$span(
      htmltools::tags$strong(app_info$name),
      " v",
      app_info$version,
      htmltools::HTML("&middot;"),
      "Developed by ",
      htmltools::tags$a(
        href = developer_info$url,
        paste0(developer_info$name, " ")
      ),
      htmltools::tags$img(
        src = developer_info$symbol,
        alt = paste0(developer_info$name, " logo symbol"),
        height = 20,
        class = "footer-img"
      ),
      " using ",
      htmltools::tags$a(
        href = "https://shiny.rstudio.com/",
        htmltools::tags$span(
          "R Shiny ",
          fontawesome::fa(name = "r-project", fill = "steelblue")
        )
      ),
      htmltools::HTML("&middot;"),
      htmltools::tags$a(
        href = app_info$docs_url,
        htmltools::tags$span(
          "View Documentation ",
          shiny::icon("book")
        )
      ),
      htmltools::HTML("&middot;"),
      htmltools::tags$a(
        href = app_info$repo_url,
        htmltools::tags$span(
          "View on GitHub ",
          shiny::icon("github")
        )
      )
    )
  )
}

#' @rdname mod_footer
#' @export
#' @importFrom htmltools tags HTML
footer_client_section <- function(client_info) {
  htmltools::tags$div(
    htmltools::tags$span(
      "Client: ",
      htmltools::tags$a(
        href = client_info$url,
        client_info$name
      )
    ),
    if (!is.null(client_info$symbol)) {
      htmltools::tags$img(
        src = client_info$symbol,
        alt = paste0(client_info$name, " logo symbol"),
        height = 20,
        class = "footer-img"
      )
    }
  )
}

#' @rdname mod_footer
#' @export
#' @importFrom bslib layout_columns
#' @importFrom htmltools tags
footer_logo_section <- function(app_info = NULL, developer_info = NULL, client_info = NULL) {

  # Initialize empty lists for columns and column widths
  columns <- list()
  col_widths <- c()

  # Define the total units (e.g., 12 for a Bootstrap grid system)
  total_width_units <- 12

  # Define the units for side margins (ensure it's an integer)
  side_margin_units <- 1

  # Collect non-NULL content sections
  content_sections <- list()

  if (!is.null(app_info)) {
    content_sections[[length(content_sections) + 1]] <- htmltools::tags$div(
      htmltools::tags$a(
        href = app_info$url,
        htmltools::tags$img(
          src = app_info$logo,
          alt = paste0(app_info$name, " logo"),
          height = "50px"
        )
      )
    )
  }

  if (!is.null(developer_info)) {
    content_sections[[length(content_sections) + 1]] <- htmltools::tags$div(
      htmltools::tags$a(
        href = developer_info$url,
        htmltools::tags$img(
          src = developer_info$logo,
          alt = paste0(developer_info$name, " logo"),
          height = "50px"
        )
      )
    )
  }

  if (!is.null(client_info)) {
    content_sections[[length(content_sections) + 1]] <- htmltools::tags$div(
      htmltools::tags$a(
        href = client_info$url,
        htmltools::tags$img(
          src = client_info$logo,
          alt = paste0(client_info$name, " logo"),
          height = "50px"
        )
      )
    )
  }

  num_content_sections <- length(content_sections)

  # If no content sections are provided, return an empty div
  if (num_content_sections == 0) {
    return(htmltools::tags$div())
  }

  # Calculate the content units available after subtracting side margins
  content_units <- total_width_units - 2 * side_margin_units

  # Calculate the base width for each content section (ensure it's an integer)
  content_column_width <- floor(content_units / num_content_sections)

  # Calculate total assigned units and remaining units to distribute
  assigned_units <- 2 * side_margin_units + num_content_sections * content_column_width
  remaining_units <- total_width_units - assigned_units

  # Distribute remaining units among content columns
  content_col_widths <- rep(content_column_width, num_content_sections)
  if (remaining_units > 0) {
    # Distribute the extra units to the first few columns
    for (i in seq_len(remaining_units)) {
      content_col_widths[i] <- content_col_widths[i] + 1
    }
  }

  # Start building the columns list with the side margin at the beginning
  columns[[1]] <- htmltools::tags$div()  # Left margin
  col_widths[1] <- side_margin_units

  # Add each content section to the columns list and update col_widths
  for (i in seq_along(content_sections)) {
    columns[[length(columns) + 1]] <- content_sections[[i]]
    col_widths <- c(col_widths, content_col_widths[i])
  }

  # Add the side margin at the end
  columns[[length(columns) + 1]] <- htmltools::tags$div()  # Right margin
  col_widths <- c(col_widths, side_margin_units)

  # Ensure that col_widths sums to total_width_units
  if (sum(col_widths) != total_width_units) {
    stop("Column widths do not sum up to total width units.")
  }

  # Prepare arguments for layout_columns
  args <- c(list(col_widths = col_widths), columns)

  # Create the layout using do.call
  do.call(bslib::layout_columns, args)
}

#' @rdname mod_footer
#' @export
#' @importFrom htmltools tags HTML
footer_copyright_section <- function(year, copyright_holder, developer_info) {
  htmltools::tags$div(
    htmltools::tags$span(
      htmltools::HTML("&copy;"),
      year,
      " ",
      htmltools::tags$a(
        href = developer_info$url,
        copyright_holder
      ),
      " | All rights reserved."
    )
  )
}


# validate_image ----------------------------------------------------------

#' @rdname mod_footer
#' @export
validate_image <- function(img_path) {
  full_path <- system.file(img_path, package = "gmhdatahub")
  if (file.exists(full_path)) {
    return(img_path)
  } else {
    warning(paste("Image not found:", img_path, "- using placeholder image."))
    return("www/img/placeholders/default-image.png")
  }
}

# test app ----------------------------------------------------------------

if (FALSE) {
  pkgload::load_all()
  ui <- bslib::page_fluid(
    title = "Footer Module",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_footer_ui("footer")
  )
  # Main Server
  server <- function(input, output, session) {
    mod_footer_server("footer")
  }
  shiny::shinyApp(ui, server)
}
