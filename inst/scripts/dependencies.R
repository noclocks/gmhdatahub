
#  ------------------------------------------------------------------------
#
# Title : R Package Dependencies
#    By : Jimmy Briggs
#  Date : 2025-01-18
#
#  ------------------------------------------------------------------------


# install pak if not already installed
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages(
    "pak",
    repos = sprintf(
      "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
      .Platform$pkgType,
      R.Version()$os,
      R.Version()$arch
    )
  )
}

# install github remotes if not already installed
if (!requireNamespace("noClocksAuthR", quietly = TRUE)) {
  install.packages("noclocks/noClocksAuthR")
}

if (!requireNamespace("reactablefmtr", quietly = TRUE)) {
  pak::pak("kcuilla/reactablefmtr")
}

if (!requireNamespace("shinybg", quietly = TRUE)) {
  pak::pak("tiledb-inc/shinybg")
}

# packages
to_install <- c(
  "apexcharter",
  "bsicons",
  "bslib",
  "cli",
  "config",
  "DBI",
  "dbplyr",
  "dbx",
  "dplyr",
  "fontawesome",
  "fs",
  "ggmap",
  "glue",
  "googleway",
  "htmltools",
  "httr2",
  "jsonlite",
  "leaflet",
  "logger",
  "lubridate",
  "mime",
  "pkgload",
  "plotly",
  "pool",
  "purrr",
  "reactable",
  "readr",
  "rintrojs",
  "rlang",
  "RPostgres",
  "scales",
  "shiny",
  "shinycustomloader",
  "shinyjs",
  "shinyvalidate",
  "shinyWidgets",
  "stringr",
  "tibble",
  "tidyr",
  "waiter",
  "withr"
)

for (pkg in to_install) {
  message(paste("Looking for package: ", pkg))
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("     Installing Package: ", pkg))
    pak::pak(pkg)
  }
}
