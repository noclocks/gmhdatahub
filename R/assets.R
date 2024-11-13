
#  ------------------------------------------------------------------------
#
# Title : Assets
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------


# add external resources --------------------------------------------------

#' Add External Resources
#'
#' @description
#' Add external resources to the shiny app.
#'
#' This function performs the following:
#'
#' - Adds this package's installed `www` folder to the shiny app resource paths
#'   via [shiny::addResourcePath()].
#'
#'
#' @return NULL
#' @export
add_external_resources <- function() {

  shiny::addResourcePath("www", pkg_sys("www"))

  htmltools::tags$head(
    htmltools::tags$link(
      rel = "shortcut icon",
      type = "image/x-icon",
      href = "www/favicon.ico"
    ),
    golem::bundle_resources(
      path = system.file("www", package = "gmhdatahub"),
      app_title = "gmhdatahub"
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter()
  )
}


# compile SASS ------------------------------------------------------------

#' Compile Styles
#'
#' @description
#' Compile SASS files to CSS.
#'
#' @param path The path to the `styles` folder. Default is `pkg_sys("www", "styles")`.
#'   This folder must contain a `scss` folder with a `index.scss` file.
#'
#' @return The path to the compiled CSS file.
#'
#' @export
#'
#' @examples
#' compile_styles()
#'
#' @importFrom sass sass sass_file sass_options output_template
#' @importFrom cli cli_alert_danger cli_alert_success
compile_styles <- function(
  path = pkg_sys("www", "styles"),
  source_map = TRUE,
  cache = FALSE
) {

  # paths
  scss_dir <- file.path(path, "scss")
  css_dir <- file.path(path, "css")

  # create css directory if it doesn't exist
  if (!dir.exists(css_dir)) {
    dir.create(css_dir)
  }

  # output template
  sass_output_template <- sass::output_template(
    basename = "styles",
    dirname = "css",
    fileext = ".min.css",
    path = path
  )

  # main scss file
  scss_main <- file.path(scss_dir, "index.scss")
  output_file <- file.path(css_dir, "styles.min.css")

  # compile scss to css
  tryCatch({
    sass::sass(
      sass::sass_file(scss_main),
      output = sass_output_template(suffix = ""),
      options = sass::sass_options(
        output_style = "compressed",
        include_path = list.files(
          scss_dir,
          full.names = TRUE
        ),
        source_map_embed = source_map
      ),
      cache = cache
    )
  }, error = function(e) {
    cli::cli_alert_danger("Error compiling SASS: {e$message}")
  })

  cli::cli_alert_success("SASS compiled successfully: {.field {output_file}}")

  # return the path to the compiled css file
  return(output_file)

}
