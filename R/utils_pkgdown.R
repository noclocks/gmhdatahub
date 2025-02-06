#' Open Documentation Website (Locally)
#'
#' @description
#' This function launches the `pkgdown` documentation for the `gmhdatahub`
#' package.
#'
#' @importFrom utils browseURL
#'
#' @export
pkg_docs <- function() {
  guide_path <- pkg_sys("docs/index.html")
  if (guide_path == "") {
    cli::cli_abort(
      "There is no pkgdown site in {.path {guide_path}}. "
    )
  }
  utils::browseURL(paste0('file://', guide_path))
}
