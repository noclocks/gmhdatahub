
if (!exists("use_template")) {
  source("dev/R/use_template.R")
}

use_module <- function(name, test = FALSE) {
  use_template("dev/templates/module.template.R", paste0("R/mod_", name, ".R"), data = list(name = name), open = TRUE)
  if (test) {
    use_template("dev/templates/test-module.template.R", paste0("tests/testthat/test-mod_", name, ".R"), data = list(name = name), open = TRUE)
  }
}

use_module_test <- function(name) {
  use_template("dev/templates/test-module.template.R", paste0("tests/testthat/test-mod_", name, ".R"), data = list(name = name), open = TRUE)
}


