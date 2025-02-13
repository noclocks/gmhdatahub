
#  ------------------------------------------------------------------------
#
# Title : Package Initialization
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

# libraries ---------------------------------------------------------------

require(devtools)
require(usethis)
require(roxygen2)
require(testthat)
require(rmarkdown)
require(knitr)
require(attachment)
require(pak)
require(purrr)
require(lifecycle)
require(rlang)
require(cli)
require(pkgload)
require(pkgbuild)
require(rcmdcheck)
require(fs)

# initialize --------------------------------------------------------------
if (FALSE) {
  usethis::create_package("gmhdatahub")
  usethis::use_git()
  usethis::use_namespace()
  usethis::use_roxygen_md()
  usethis::use_proprietary_license("No Clocks, LLC")
  usethis::use_readme_rmd()
  usethis::use_package_doc()
  usethis::use_import_from("rlang", ".data")
  usethis::use_import_from("rlang", ".env")
}

attachment::att_amend_desc()
devtools::document()


# setup dev directory -----------------------------------------------------
if (FALSE) {
  usethis::use_directory("dev", ignore = TRUE)
  fs::dir_create("dev/R")
}

# description -------------------------------------------------------------

if (FALSE) {
  usethis::use_description(
    fields = list(
      Title = "Shiny Dashboard and API Client for GMH Communities Leasing Data",
      Description = paste0(
        "Provides a Shiny dashboard and API client for interacting",
        "with GMH Communities leasing data via the Entrata API. Includes",
        "functions for API authentication, data retrieval, and data",
        "visualization."
      ),
      Language =  "en-US"
    )
  )
}


# authors -----------------------------------------------------------------

if (FALSE) {

  usethis::use_author(
    "Jimmy",
    "Briggs",
    email = "jimmy.briggs@noclocks.dev",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0002-7489-8787")
  )

  usethis::use_author(
    "Patrick",
    "Howard",
    email = "patrick.howard@noclocks.dev",
    role = c("aut", "rev")
  )

  usethis::use_author(
    "No Clocks, LLC",
    email = "team@noclocks.dev",
    role = c("cph", "fnd")
  )

}


# initial docs ------------------------------------------------------------

if (FALSE) {
  usethis::use_readme_rmd(open = FALSE)
  usethis::use_proprietary_license("No Clocks, LLC")
  usethis::use_package_doc(open = FALSE)
  usethis::use_vignette("gmhdatahub")
}


# logo --------------------------------------------------------------------

logo_url <- "https://mma.prnewswire.com/media/2150945/GMH__box_only__NAVY_01__002_Logo.jpg"
fs::dir_create("man/figures")
download.file(logo_url, destfile = "man/figures/gmh-icon.jpg", mode = "wb")
usethis::use_logo("man/figures/gmh-icon.jpg")

# git & github ------------------------------------------------------------
usethis::use_git()
usethis::use_github()
usethis::use_github_links()

# testing ----------------------------------------------------------------
usethis::use_testthat(edition = 3, parallel = TRUE)

# code coverage
usethis::use_coverage(type = "codecov", repo_spec = "noclocks/gmhdatahub")
usethis::use_covr_ignore("dev/")
usethis::use_covr_ignore("inst/")
usethis::use_covr_ignore("data-raw")
usethis::use_build_ignore("codecov.yml")
usethis::use_build_ignore(".covrignore")
usethis::use_build_ignore("~\\$.*", escape = FALSE)

# initial tests
usethis::use_test("gmhdatahub")

# httptest2
httptest2::use_httptest2()

# shinytest2
shinytest2::use_shinytest2()

# spellcheck
usethis::use_spell_check()
cat(
  "if (requireNamespace(\"spelling\", quietly = TRUE)) {",
  "  spelling::spell_check_test(",
  "    vignettes = TRUE,",
  "    error = FALSE,",
  "    skip_on_cran = TRUE",
  "  )",
  "}",
  "",
  file = "tests/spelling.R",
  sep = "\n",
  append = FALSE
)
spelling::update_wordlist()

# run tests
devtools::test()

# linting -----------------------------------------------------------------
lintr::use_lintr()
cat(
  "linters: linters_with_defaults(",
  "    line_length_linter(120),",
  "    commented_code_linter = NULL,",
  "    trailing_blank_lines_linter = NULL",
  "  )",
  "exclusions: list()",
  "encoding: \"UTF-8\"",
  "",
  file = ".lintr",
  sep = "\n",
  append = FALSE
)
usethis::use_build_ignore(".lintr")
lintr::lint_package()

# gitignore, gitattributes, editorconfig ----------------------------------

gitattributes <- c(
  "# Global",
  "# -------------",
  "* text=auto",
  "tests/fixtures/**/* -diff", # Ignore diff for fixtures
  "",
  "# R Files",
  "# -------------",
  "*.{R,Rmd,Rd,Rproj,[Rr]environ,[Rr]profile,[Rr]history} text",
  "",
  "# Binary Files",
  "# -------------",
  "*.{R[Dd]ata,R[Dd][Ss],rda,rdb,rds,Rdx} binary", # R data files
  "*.zip binary", # Zip files
  "*.{pdf,png,jpg,jpeg,gif} binary", # Image files
  "",
  "# Linguist",
  "# -------------",
  ".[Rr]md linguist-detectable",
  "*.{md,markdown,txt} linguist-documentation",
  "docs/** linguist-documentation",
  "vignettes/** linguist-documentation",
  "*.{js,css,scss,less} linguist-vendored",
  "*.{min.js,min.css} linguist-generated",
  ""
)

writeLines(gitattributes, ".gitattributes")

editorconfig <- c(
  "[*]",
  "indent_style = space",
  "indent_size = 2",
  "end_of_line = lf",
  "charset = utf-8",
  "trim_trailing_whitespace = true",
  "insert_final_newline = true",
  "",
  "[*.R]",
  "indent_size = 2",
  "max_line_length = 120",
  "",
  "[*.md]",
  "trim_trailing_whitespace = false",
  "",
  "[*.json]",
  "insert_final_newline = ignore",
  "",
  "[Makefile]",
  "indent_style = tab",
  "",
  "[*.bat]",
  "indent_style = tab"
)

writeLines(editorconfig, ".editorconfig")

usethis::use_build_ignore(".gitattributes")
usethis::use_build_ignore(".editorconfig")

# codemeta ----------------------------------------------------------------
# codemetar::write_codemeta()
codemeta::write_codemeta()
usethis::use_build_ignore("codemeta.json")

# news --------------------------------------------------------------------
usethis::use_news_md()



# inst --------------------------------------------------------------------
c(
  "inst",

  # assets
  "inst/assets",
  "inst/assets/img",
  "inst/assets/styles",
  "inst/assets/scripts",

  # config
  "inst/config",

  # extdata
  "inst/extdata",

  # shiny
  "inst/shiny",
  "inst/shiny/auth_app",
  "inst/shiny/auth_app/www",

  # plumber
  "inst/plumber",
  "inst/plumber/auth_api",

  # templates
  "inst/templates",
  "inst/templates/email",

  # scripts
  "inst/scripts"
) |>
  purrr::walk(fs::dir_create)

# config ------------------------------------------------------------------

require(noclocksr)

noclocksr::cfg_init(cfg = cfg)
noclocksr::cfg_hooks_init()
usethis::use_build_ignore("config.yml")

usethis::use_build_ignore(".dockerignore")
usethis::use_build_ignore("Dockerfile")

# data --------------------------------------------------------------------

usethis::use_data_raw("internal")
usethis::use_data_raw("exported")


# refresh -----------------------------------------------------------------

attachment::att_amend_desc()
devtools::document()
devtools::load_all()
devtools::check()
devtools::test()
devtools::install()
devtools::build()



