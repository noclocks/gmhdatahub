#  ------------------------------------------------------------------------
#
# Title : Package Utilities
#    By : Jimmy Briggs
#  Date : 2025-01-11
#
#  ------------------------------------------------------------------------

#' Package System File
#'
#' @name pkg_sys
#'
#' @description
#' This function is a wrapper for the `system.file` function. It is used to
#' retrieve the path to a file within the package directory.
#'
#' @param ... A character vector of subdirectories and file name.
#'
#' @return A character string of the file path.
#'
#' @export
#'
#' @examples
#' pkg_sys("www", "styles", "css", "styles.min.css")
#' pkg_sys("config", "config.yml")
#' pkg_sys("extdata")
pkg_sys <- function(...) {
  system.file(..., package = "gmhdatahub")
}

#' @rdname pkg_sys
#' @export
pkg_sys_www <- function(...) {
  pkg_sys("www", ...)
}

#' @rdname pkg_sys
#' @export
pkg_sys_config <- function(...) {
  pkg_sys("config", ...)
}

#' @rdname pkg_sys
#' @export
pkg_sys_templates <- function(...) {
  pkg_sys("templates", ...)
}

#' @rdname pkg_sys
#' @export
pkg_sys_extdata <- function(...) {
  pkg_sys("extdata", ...)
}

#' Package Documentation
#'
#' @description
#' Open the package documentation in the default web browser.
#'
#' @returns
#' Opens the package documentation in the default web browser.
#'
#' @export
#'
#' @importFrom utils browseURL
#'
#' @examplesIf interactive()
#' pkg_docs()
pkg_docs <- function() {
  guide_path <- pkg_sys("docs/index.html", package = "gmhdatahub")
  if (guide_path == "") {
    cli::cli_abort("Documentation not found.")
  }
  utils::browseURL(paste0("file://", guide_path))
}
