parse_unit_types <- function(resp_properties) {
  purrr::map_dfr(resp_properties, function(x) {
    tibble::tibble(
      property_id = x$id,
      unit_type_id = purrr::map_chr(x$unitTypes$unitType, "id"),
      unit_type_name = purrr::map_chr(x$unitTypes$unitType, "name")
    )
  })
}

safe_unnest <- function(data, id_col, path, names) {
  data |>
    dplyr::select(
      {{ id_col }},
      {{ path }}
    ) |>
    tidyr::unnest(
      cols = {{ path }},
      names_sep = "_",
      names_repair = "minimal"
    ) |>
    dplyr::select(
      dplyr::all_of(names)
    )
}
