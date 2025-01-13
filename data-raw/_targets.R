# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tibble",
    "dplyr",
    "tidyr",
    "readr",
    "readxl",
    "openxlsx2",
    "lubridate",
    "stringr",
    "purrr",
    "tibblify"
  ),
  format = "qs"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("data-raw/R/")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(

  tar_target(
    name = entrata,
    command = source("data-raw/scripts/entrata.R")
  ),

  tar_target(
    name = shiny,
    command = source("data-raw/scripts/shiny.R")
  ),

  tar_target(
    name = gmh,
    command = source("data-raw/scripts/gmh.R")
  ),

  # depends on the above three
  tar_target(
    name = internal,
    command = source("data-raw/scripts/internal.R")
  )
)
