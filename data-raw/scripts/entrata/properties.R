#  ------------------------------------------------------------------------
#
# Title : Entrata Properties
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

# build request and perform response
req <- entrata_properties_request()
resp <- entrata_req_perform_save(req)

# parse response
resp_properties_json <- resp |> httr2::resp_body_json()

# create tables
entrata_properties_tbl <- resp_properties_json |>
  purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
  purrr::map(function(x) {
    name <- purrr::pluck(x, "MarketingName")
    id <- purrr::pluck(x, "PropertyID")
    out <- list(name = name, id = id)
  }) |>
  purrr::map_dfr(~ tibble::tibble(name = .x$name, id = .x$id))

entrata_properties_lst <- as.list(entrata_properties_tbl$id) |>
  setNames(entrata_properties_tbl$name)

# cleanup
rm(req, resp, resp_properties_json)
