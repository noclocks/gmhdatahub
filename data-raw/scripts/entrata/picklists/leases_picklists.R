
#  ------------------------------------------------------------------------
#
# Title : Leases Pick Lists
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

req <- entrata_leases_picklist_request()
resp <- entrata_req_perform_save(req)
resp_json <- httr2::resp_body_json(resp)

resp_content <- purrr::pluck(
  resp_json,
  "response",
  "result"
)

# status types ------------------------------------------------------------
lease_status_types <- purrr::pluck(resp_content, "leaseStatusTypes", "leaseStatusType") |>
  purrr::map_dfr(
    function(x) {
      attribs <- purrr::pluck(x, "@attributes")
      id <- purrr::pluck(attribs, "id")
      name <- purrr::pluck(attribs, "name")
      type <- purrr::pluck(attribs, "type")
      return(
        tibble::tibble(
          id = id,
          name = name,
          type = type
        )
      )
    }
  ) |>
  janitor::clean_names()


# file types --------------------------------------------------------------

lease_file_types <- purrr::pluck(resp_content, "leaseFileTypes", "leaseFileType") |>
  purrr::map_dfr(~ tibble::as_tibble(.x[["@attributes"]])) |>
  janitor::clean_names()

charge_timings <- purrr::pluck(resp_content, "chargeTimings", "chargeTiming") |>
  purrr::map_dfr(~ tibble::as_tibble(.x[["@attributes"]])) |>
  janitor::clean_names()

charge_code_types <- purrr::pluck(resp_content, "chargeCodeTypes", "chargeCodeType") |>
  purrr::map_dfr(~ tibble::as_tibble(.x[["@attributes"]])) |>
  janitor::clean_names()

allowed_charge_timings_by_type <- charge_code_types |>
  tidyr::separate_rows(allowed_charge_timing_ids, sep = ",") |>
  dplyr::mutate(allowed_charge_timing_ids = as.integer(allowed_charge_timing_ids))

event_types <- purrr::pluck(resp_content, "eventTypes", "eventType") |>
  purrr::map_dfr(~ tibble::as_tibble(.x[["@attributes"]])) |>
  janitor::clean_names()

pet_types <- purrr::pluck(resp_content, "petTypes", "petType") |>
  purrr::map_dfr(~ tibble::as_tibble(.x[["@attributes"]])) |>
  janitor::clean_names()

refund_methods <- purrr::pluck(resp_content, "refundMethods", "refundMethod") |>
  purrr::map(~ purrr::pluck(.x, "@attributes")) |>
  purrr::map_dfr(~ tibble::as_tibble(.x)) |>
  janitor::clean_names()

event_tags <- purrr::pluck(resp_content, "eventTags", "eventTag") |>
  purrr::map(~ purrr::pluck(.x, "@attributes")) |>
  purrr::map_dfr(~ tibble::as_tibble(.x)) |>
  janitor::clean_names()

entrata_leases_pick_list_tbls <- list(
  lease_status_types = lease_status_types,
  lease_file_types = lease_file_types,
  charge_timings = charge_timings,
  charge_code_types = charge_code_types,
  allowed_charge_timings_by_type = allowed_charge_timings_by_type,
  event_types = event_types,
  pet_types = pet_types,
  refund_methods = refund_methods,
  event_tags = event_tags
)

# cleanup
rm(
  req,
  resp,
  resp_json,
  resp_content,
  lease_status_types,
  lease_file_types,
  charge_timings,
  charge_code_types,
  allowed_charge_timings_by_type,
  event_types,
  pet_types,
  refund_methods,
  event_tags
)
