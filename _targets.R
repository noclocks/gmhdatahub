
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
pkgload::load_all()

# configure ---------------------------------------------------------------

config <- config::get()
gmhdatahub:::configure_gcs("entrata-pipeline")

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
    name = "db_status_check",
    command = gmhdatahub::check_db_conn(pool)
  ),
  # entrata status check
  targets::tar_target(
    name = "entrata_status_check",
    command = gmhdatahub::entrata_status(entrata_config = entrata_config)
  ),
  # entrata properties list
  targets::tar_target(
    name = "entrata_properties_lst",
    command = gmhdatahub::entrata_properties("getProperties", entrata_config = entrata_config)
  ),
  # entrata properties table
  targets::tar_target(
    name = "entrata_properties_tbl",
    command = purrr::pluck(entrata_properties_lst, "properties")
  ),
  # entrata property addresses table
  targets::tar_target(
    name = "entrata_property_addresses_tbl",
    command = purrr::pluck(entrata_properties_lst, "property_addresses")
  ),
  # entrata property post months table
  targets::tar_target(
    name = "entrata_property_post_months_tbl",
    command = purrr::pluck(entrata_properties_lst, "property_post_months")
  ),
  # entrata property hours table
  targets::tar_target(
    name = "entrata_property_hours_tbl",
    command = purrr::pluck(entrata_properties_lst, "property_hours")
  ),
  # entrata property space options table
  targets::tar_target(
    name = "entrata_property_space_options_tbl",
    command = purrr::pluck(entrata_properties_lst, "property_space_options")
  ),
  # entrata property lease terms table
  targets::tar_target(
    name = "entrata_property_lease_terms_tbl",
    command = purrr::pluck(entrata_properties_lst, "property_lease_terms")
  ),
  # entrata property lease term windows table
  targets::tar_target(
    name = "entrata_property_lease_term_windows_tbl",
    command = purrr::pluck(entrata_properties_lst, "property_lease_term_windows")
  ),
  # entrata property ids
  targets::tar_target(
    name = "entrata_property_ids",
    command = purrr::pluck(entrata_properties_tbl, "property_id")
  ),
  # entrata property floorplans
  targets::tar_target(
    name = "entrata_property_floorplans_lst",
    command = entrata_floorplans(property_ids = entrata_property_ids, entrata_config = entrata_config)
  )
)
