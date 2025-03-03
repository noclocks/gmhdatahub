
#  ------------------------------------------------------------------------
#
# Title : Entrata Database Schema
#    By : Jimmy Briggs
#  Date : 2025-03-02
#
#  ------------------------------------------------------------------------


# properties ------------------------------------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

# build request and perform response
req <- entrata_properties_request()
resp <- entrata_req_perform_save(req)



entrata_properties_tbl <- parse_property_base_data(resp_data)
entrata_property_addresses_tbl <- parse_property_address_data(resp_data)


# resp_data_clean <- resp_data |>
#   dplyr::mutate(
#     property_id = as.integer(property_id),
#     property_name = as.character(property_name),
#     property_type = as.character(property_type),
#     property_website = as.character(property_website),
#     property_email = as.character(property_email),
#     property_phone = as.character(property_phone),
#     is_disabled = as.logical(is_disabled),
#     is_featured = as.logical(is_featured),
#     parent_property_id = as.integer(parent_property_id)
#   )
#
# resp_properties_tbl <- resp_properties_json |>
#   purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
#   purrr::map_dfr(
#     ~ {
#       list(
#         property_id = .x |> purrr::pluck("PropertyID") |> as.integer(),
#         property_name = .x |> purrr::pluck("MarketingName") |> as.character(),
#         property_type = .x |> purrr::pluck("Type") |> as.character(),
#         property_website = .x |> purrr::pluck("webSite") |> as.character(),
#         property_email = .x |> purrr::pluck("Address", "Email") |> as.character(),
#         property_phone = .x |> purrr::pluck("Phone", "PhoneNumber") |> as.character(),
#         is_disabled = .x |> purrr::pluck("IsDisabled") |> as.logical(),
#         is_featured = .x |> purrr::pluck("IsFeatured") |> as.logical(),
#         parent_property_id = .x |> purrr::pluck("ParentPropertyID") |> as.integer(),
#
#       )
#     }
#
#
#
#
# entrata_properties_lst <- entrata_properties(
#   method_name = "getProperties",
#   method_params = list(NULL),
#   request_id = 15L
# )
#
# entrata_property_tbl <- entrata_properties_lst$property_tbl_base
# entrata_property_addresses_tbl <- entrata_properties_lst$property_addresses
# entrata_property_hours <- entrata_properties_lst$property_hours
# entrata_property_space_options <- entrata_properties_lst$property_space_options
