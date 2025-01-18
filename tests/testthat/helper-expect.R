expect_shinytaglist <- function(object) {
  rlang::check_installed("testthat", "to run the tests.", version = "3.0.0")
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  act$class <- class(object)
  testthat::expect(
    "shiny.tag.list" %in% act$class,
    sprintf("%s has class '%s', not class 'shiny.tag.list'.", act$lab, paste(act$class, collapse = ", "))
  )
  invisible(act$val)
}

expect_shinytag <- function(object) {
  rlang::check_installed("testthat", "to run the tests.", version = "3.0.0")
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  act$class <- class(object)
  testthat::expect(
    "shiny.tag" %in% act$class,
    sprintf("%s has class '%s', not class 'shiny.tag'.", act$lab, paste(act$class, collapse = ", "))
  )
  invisible(act$val)
}

expect_bslib_fragment <- function(object) {
  rlang::check_installed("testthat", "to run the tests.", version = "3.0.0")
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  act$class <- class(object)
  testthat::expect(
    "bslib_fragment" %in% act$class,
    sprintf("%s has class '%s', not class 'bslib.fragment'.", act$lab, paste(act$class, collapse = ", "))
  )
  invisible(act$val)
}
