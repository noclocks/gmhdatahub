
#  ------------------------------------------------------------------------
#
# Title : Package Documentation Development
#    By : Jimmy Briggs
#  Date : 2025-01-28
#
#  ------------------------------------------------------------------------


# vignettes ---------------------------------------------------------------

usethis::use_vignette("dates", "dates")


# articles --------------------------------------------------------------------------------------------------------

usethis::use_article("excel", "Excel Data Preparation")

# internal docs -----------------------------------------------------------

require(papillon)

papillon::open_pkgdown_function()

papillon::build_pkgdown(
  move = TRUE,
  clean_after = FALSE,
  favicon = pkg_sys("www/images/favicon.png")
)

papillon::create_pkg_desc_file(

)

papillon::build_book()
