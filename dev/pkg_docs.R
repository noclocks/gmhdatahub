
#  ------------------------------------------------------------------------
#
# Title : Package Documentation Development
#    By : Jimmy Briggs
#  Date : 2024-11-12
#
#  ------------------------------------------------------------------------

# vignettes ---------------------------------------------------------------

usethis::use_vignette("gmhdatahub")

usethis::use_vignette("workflow")

c(
  "entrata",
  "properties"
) |>
  purrr::walk(usethis::use_vignette)
