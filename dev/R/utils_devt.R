
#  ------------------------------------------------------------------------
#
# Title : Package Development Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-17
#
#  ------------------------------------------------------------------------

run_attachment <- function() {

  requireNamespace("attachment")

  # move sysdata (for some reason causes attachment to fail)
  cli::cli_alert_info("Moving {.file R/sysdata.rda} to {.path dev/sysdata.rda}")
  fs::file_move("R/sysdata.rda", "dev/sysdata.rda")

  # ensure gets moved back on exit
  withr::defer(
    expr = {
      cli::cli_alert_info("Moving {.file dev/sysdata.rda} back to {.path R/sysdata.rda}")
      fs::file_move("dev/sysdata.rda", "R/sysdata.rda")
      pkgload::load_all()
      cli::cli_alert_success("Finished performing attachment tasks.")
    }
  )

  # run attachment
  cli::cli_alert_info("Running {.code attachment::att_amend_desc()}")
  attachment::att_amend_desc()

  # document
  cli::cli_alert_info("Running {.code devtools::document()}")
  devtools::document()

  # set remotes
  cli::cli_alert_info("Running {.code attachment::set_remotes_to_desc()}")
  attachment::set_remotes_to_desc()


}
