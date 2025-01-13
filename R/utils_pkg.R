
#  ------------------------------------------------------------------------
#
# Title : Package Utilities
#    By : Jimmy Briggs
#  Date : 2025-01-11
#
#  ------------------------------------------------------------------------

#' Package System File
#'
#' @param ... Arguments passed to [base::system.file()]
#'
#' @returns
#' The path to the system file.
#'
#' @export
#'
#' @examples
#' pkg_sys("extdata", "data.csv")
pkg_sys <- function(...) { system.file(..., package = "gmhdatahub") }
