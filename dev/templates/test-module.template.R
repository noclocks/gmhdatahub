
#  ------------------------------------------------------------------------
#
# Title : {{title}} Shiny Module Tests
#    By : {{author}}
#  Date : {{date}}
#
#  ------------------------------------------------------------------------


# server ------------------------------------------------------------------

test_that("mod_{{name}}_server", {

  shiny::testServer(
    mod_{{name}}_server,
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

test_that("mod_{{name}}_ui", {

  ui <- mod_{{name}}_ui("test")
  expect_shinytaglist(ui)
  fmls <- formals(mod_{{name}}_ui)
  for (i in c("id")) { expect_true(i %in% names(fmls)) }

})


# demo --------------------------------------------------------------------

test_that("mod_{{name}}_demo", {
  demo <- mod_{{name}}_demo()
  expect_no_error(demo)
  expect_s3_class(demo, "shiny.appobj")
})

