# Remotes ----
install.packages("remotes")
remotes::install_github('rstudio/bslib')
remotes::install_github('r-lib/httr2')
remotes::install_github('rstudio/sass')
# Attachments ----
to_install <- c("bsicons", "cli", "dplyr", "fontawesome", "golem", "htmltools", "jsonlite", "logger", "pkgload", "purrr", "rlang", "shiny", "shinyjs", "shinyWidgets", "stringr", "waiter")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i, quietly = TRUE)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }

