
#  ------------------------------------------------------------------------
#
# Title : Module Tests
#    By : Jimmy Briggs
#  Date : 2025-01-17
#
#  ------------------------------------------------------------------------

test_that("mod_dashboard_server", {

  shiny::testServer(
    mod_dashboard_server,
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

test_that("mod_dashboard_ui", {

  ui <- mod_dashboard_ui("test")
  expect_shinytaglist(ui)
  fmls <- formals(mod_dashboard_ui)
  for (i in c("id")) { expect_true(i %in% names(fmls)) }

})
