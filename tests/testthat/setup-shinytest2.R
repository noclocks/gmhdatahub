library(shinytest2)
library(shinybg)

# Load application support files into testing environment
shinytest2::load_app_env(app_dir = testthat::test_path("app"))
