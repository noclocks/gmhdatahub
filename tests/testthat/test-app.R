library(shinytest2)

testthat::test_that("shiny app runs", {
  testthat::skip_on_cran()
  app <- run_app()
  testthat::expect_no_error(app)
})

testthat::test_that("shiny app object is correct", {
  app <- run_app()
  testthat::expect_s3_class(app, "shiny.appobj")
})

testthat::test_that("shiny app runs with healthcheck", {
  testthat::skip_on_cran()
  port <- httpuv::randomPort()
  app <- shinybg::runBackgroundApp(appFile = pkg_sys("shiny/app/app.R"), port = port)
  while (!pingr::is_up(destination = "127.0.0.1", port = port)) {
    if (!app$is_alive()) stop(app$read_all_error())
    Sys.sleep(0.01)
  }
  tryCatch({
    url <- glue::glue("http://127.0.0.1:{port}/health")
    resp <- httr2::request(url) |> httr2::req_perform()
  }, error = function(e) {
    cli::cli_abort("Healthcheck failed: {e$message}")
  }, finally = {
    shinybg::kill_all_apps()
  })
  testthat::expect_no_error(httr2::resp_check_status(resp))
  testthat::expect_equal(httr2::resp_status(resp), 200L)
  resp_json <- httr2::resp_body_json(resp)
  testthat::expect_equal(resp_json$status, "OK")
})

testthat::test_that("shiny app UI is correct", {
  ui <- app_ui()
  testthat::expect_s3_class(ui, "shiny.tag.list")
})

testthat::test_that("shiny app server is correct", {
  server <- app_server
  testthat::expect_match(class(server), "function")
})
