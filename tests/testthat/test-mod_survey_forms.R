#  ------------------------------------------------------------------------
#
# Title : Survey Forms Shiny Module Tests
#    By : Jimmy Briggs
#  Date : 2025-01-17
#
#  ------------------------------------------------------------------------


# server ------------------------------------------------------------------

test_that("mod_survey_forms_server", {
  shiny::testServer(
    mod_survey_forms_server,
    args = list(),
    {
      ns <- session$ns
      expect_true(
        inherits(ns, "function")
      )
      expect_true(
        grepl(id, ns(""))
      )
      expect_true(
        grepl("test", ns("test"))
      )
    }
  )
})


# UI ----------------------------------------------------------------------

test_that("mod_survey_forms_ui", {
  ui <- mod_survey_forms_ui("test")
  expect_shinytaglist(ui)
  fmls <- formals(mod_survey_forms_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})


# demo --------------------------------------------------------------------

test_that("mod_survey_forms_demo", {
  demo <- mod_survey_forms_demo()
  expect_no_error(demo)
  expect_s3_class(demo, "shiny.appobj")
})
