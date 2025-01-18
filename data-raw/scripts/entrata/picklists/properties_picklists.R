#  ------------------------------------------------------------------------
#
# Title : Properties Pick Lists
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

req <- entrata_properties_picklist_request()
resp <- entrata_req_perform_save(req)
resp_json <- httr2::resp_body_json(resp)

resp_properties <- purrr::pluck(
  resp_json,
  "response",
  "result",
  "properties",
  "property"
)

# parse properties pick lists tables --------------------------------------

properties_tbl <- tibble::tibble(
  property_id = purrr::map_int(resp_properties, "id"),
  property_name = purrr::map_chr(resp_properties, "name")
)

unit_types_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    unit_type_id = purrr::map_chr(x$unitTypes$unitType, "id"),
    unit_type_name = purrr::map_chr(x$unitTypes$unitType, "name")
  )
})

floorplans_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    floorplan_id = purrr::map_chr(x$propertyFloorplans$propertyFloorplan, "id"),
    floorplan_name = purrr::map_chr(x$propertyFloorplans$propertyFloorplan, "name")
  )
})

rentable_items_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    rentable_item_id = purrr::map_int(x$rentableItems$rentableItem, "id"),
    rentable_item_name = purrr::map_chr(x$rentableItems$rentableItem, "name")
  )
})

add_on_categories_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    add_on_id = purrr::map_int(x$addOnCategories$addOnCategory, "id"),
    add_on_name = purrr::map_chr(x$addOnCategories$addOnCategory, "name")
  )
})

assignable_items_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    item_id = purrr::map_int(x$assignableItems$assignableItem, "id"),
    item_name = purrr::map_chr(x$assignableItems$assignableItem, "name")
  )
})

move_out_types_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    move_out_type_id = purrr::map_int(x$moveOutTypes$moveOutType, "id"),
    move_out_type_name = purrr::map_chr(x$moveOutTypes$moveOutType, "name")
  )
})

move_out_reasons_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    reason_id = purrr::map_chr(x$moveOutReasons$moveOutReason, "id"),
    reason_name = purrr::map_chr(x$moveOutReasons$moveOutReason, "name"),
    move_out_type_id = purrr::map_int(x$moveOutReasons$moveOutReason, "moveOutTypeId")
  )
})

lease_terms_tbl <- purrr::map_dfr(resp_properties, function(x) {
  tibble::tibble(
    property_id = x$id,
    lease_term_id = purrr::map_chr(x$leaseTerms$leaseTerm, "leaseTermId"),
    lease_term_name = purrr::map_chr(x$leaseTerms$leaseTerm, "leaseTermName"),
    lease_term_structure_id = purrr::map_chr(x$leaseTerms$leaseTerm, "leaseTermStructureId"),
    lease_term_structure_name = purrr::map_chr(x$leaseTerms$leaseTerm, "leaseTermStructureName"),
    web_visible = purrr::map_lgl(x$leaseTerms$leaseTerm, "webVisible")
  )
})

lease_start_windows_tbl <- purrr::map_dfr(resp_properties, function(x) {
  if (!is.null(x$leaseTerms$leaseTerm)) {
    purrr::map_dfr(x$leaseTerms$leaseTerm, function(y) {
      if (!is.null(y$leaseStartWindows$leaseStartWindow)) {
        tibble::tibble(
          property_id = x$id,
          lease_term_id = y$leaseTermId,
          lease_start_window_id = purrr::map_int(y$leaseStartWindows$leaseStartWindow, "leaseStartWindowId"),
          window_start_date = purrr::map_chr(y$leaseStartWindows$leaseStartWindow, "windowStartDate"),
          window_end_date = purrr::map_chr(y$leaseStartWindows$leaseStartWindow, "windowEndDate")
        )
      }
    })
  }
})

# combine
entrata_properties_pick_list_tbls <- list(
  properties = properties_tbl,
  unit_types = unit_types_tbl,
  floorplans = floorplans_tbl,
  rentable_items = rentable_items_tbl,
  add_on_categories = add_on_categories_tbl,
  assignable_items = assignable_items_tbl,
  move_out_types = move_out_types_tbl,
  move_out_reasons = move_out_reasons_tbl,
  lease_terms = lease_terms_tbl,
  lease_start_windows = lease_start_windows_tbl
)

# cleanup
rm(
  req,
  resp,
  resp_json,
  resp_properties,
  properties_tbl,
  unit_types_tbl,
  floorplans_tbl,
  rentable_items_tbl,
  add_on_categories_tbl,
  assignable_items_tbl,
  move_out_types_tbl,
  move_out_reasons_tbl,
  lease_terms_tbl,
  lease_start_windows_tbl
)
