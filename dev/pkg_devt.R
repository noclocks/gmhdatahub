
#  ------------------------------------------------------------------------
#
# Title : Package Development Script
#    By : Jimmy Briggs
#  Date : 2024-08-13
#
#  ------------------------------------------------------------------------


# defaults ----------------------------------------------------------------

c(
  "aaa",
  "zzz",
  "data",
  "utils",
  "config"
) |>
  purrr::walk(usethis::use_r, open = FALSE)

# R/app_* -----------------------------------------------------------------

c(
  "app_run",
  "app_ui",
  "app_server"
) |>
  purrr::walk(usethis::use_r, open = FALSE)


# misc --------------------------------------------------------------------

c(
  "assets",
  "theme"
) |>
  purrr::walk(usethis::use_r, open = FALSE)

# R_mod_* -----------------------------------------------------------------

c(
  "mod_header",
  "mod_navbar",
  "mod_sidebar",
  "mod_footer",
  "mod_summary",
  "mod_properties",
  "mod_leases",
  "mod_residents",
  "mod_reports"
) |>
  purrr::walk(usethis::use_r, open = FALSE)


# tests -------------------------------------------------------------------

c(
  "mod_footer"
) |>
  purrr::walk(usethis::use_test, open = FALSE)


# coverage ----------------------------------------------------------------

usethis::use_coverage()
covr::codecov()
covrpage::covrpage()



