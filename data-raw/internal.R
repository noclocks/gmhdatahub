
#  ------------------------------------------------------------------------
#
# Title : Internal Package Data
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------


# entrata -----------------------------------------------------------------

# entrata <- list(
#   endpoints = entrata_endpoints,
#   methods = entrata_methods,
#   params = entrata_parameters,
#   defaults = list(
#     endpoint = entrata_default_endpoint,
#     methods = entrata_default_methods,
#     version = entrata_default_version,
#     req_body = entrata_default_req_body,
#     resp_body = entrata_default_resp_body,
#     req_id = 15
#   ),
#   schemas =

# app_info ----------------------------------------------------------------

app_info <- list(
  name = "GMH Data Hub",
  version = "1.0",
  logo = "www/images/shared/app/logos/app-logo.svg",
  symbol = "www/images/shared/app/icons/app-icon.webp",
  repo_url = "https://github.com/noclocks/gmhdatahub",
  docs_url = "https://docs.noclocks.dev/gmhdatahub"
)

# client information ------------------------------------------------------

client_info <- list(
  name = "GMH Communities",
  url = "https://gmhcommunities.com",
  logo = "www/images/gmh/logos/gmh-logo.svg",
  symbol = "www/images/gmh/icons/gmh-icon.png"
)

# noclocks_info -----------------------------------------------------------

developer_info <- list(
  name = "No Clocks, LLC",
  url = "https://noclocks.dev",
  logo = "www/images/noclocks/logos/noclocks-logo-black.svg",
  symbol = "www/images/noclocks/icons/noclocks-icon-circular.png"
)

# entrata_info ------------------------------------------------------------

entrata_info <- list(
  name = "Entrata",
  url = "https://gmhcommunities.entrata.com/api/v1/documentation",
  logo = "www/images/entrata/logos/entrata-logo-light.png",
  symbol = NULL
)

# input_choices -------------------------------------------------------------

portfolios <- readr::read_csv("data-raw/working/investment_partners.csv")
portfolio_choices <- portfolios$investment_partner

properties <- readr::read_csv("data-raw/working/properties_ids_names.csv")
property_choices <- properties$property_id
names(property_choices) <- properties$entrata_name

app_choices <- list(
  portfolios = portfolio_choices,
  properties = property_choices
)

usethis::use_data(
  entrata_params_tbl,
  entrata_methods_tbl,
  entrata_used_methods_tbl,
  entrata_default_methods_tbl,
  entrata_method_versions,
  entrata_endpoints,
  app_info,
  client_info,
  developer_info,
  entrata_info,
  app_choices,
  internal = TRUE,
  overwrite = TRUE
)
