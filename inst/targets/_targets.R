
#  ------------------------------------------------------------------------
#
# Title : Targets Data Pipeline
#    By : Jimmy Briggs
#  Date : 2024-11-08
#
#  ------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
library(targets)
library(tarchetypes)
library(qs2)

# if local development run pkgload:
if (interactive()) {
  pkgload::load_all()
}

# configure ---------------------------------------------------------------

config <- config::get()

# options ----------------------------------------------------------------
targets::tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "pool",
    "RPostgres",
    "jsonlite",
    "httr2",
    "tibblify",
    "tibble",
    "tidyr",
    "janitor",
    "readr",
    "fs",
    "here",
    "memoise",
    "glue",
    "googleCloudStorageR",
    "purrr"
  ),
  imports = c("gmhdatahub"),
  format = "qs",
  repository = "gcp",
  resources = targets::tar_resources(
    gcp = targets::tar_resources_gcp(
      bucket = "entrata-pipeline",
      prefix = "targets",
      predefined_acl = "default"
    )
  )
)

# source -----------------------------------------------------------------

# targets::tar_source()


# pipeline --------------------------------------------------------------------------------------------------------

list(
  # entrata configuration
  targets::tar_target(
    name = "entrata_config",
    command = gmhdatahub::get_entrata_config()
  ),
  # database configuration
  targets::tar_target(
    name = "db_config",
    command = gmhdatahub::get_db_config()
  ),
  # database connection pool
  targets::tar_target(
    name = "pool",
    command = gmhdatahub::db_connect(db_config = db_config)
  ),
  # database status check
  targets::tar_target(
    name = "db_status",
    command = gmhdatahub::check_db_conn(pool)
  ),
  # entrata status check
  targets::tar_target(
    name = "entrata_staus",
    command = gmhdatahub::entrata_status(entrata_config = entrata_config)
  )
)
