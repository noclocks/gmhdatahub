
#  ------------------------------------------------------------------------
#
# Title : Entrata Data Preparation
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------


# sources -----------------------------------------------------------------

entrata_path <- "data-raw/src/entrata"
entrata_scripts <- fs::dir_ls(entrata_path)
purrr::walk(entrata_scripts, source)

# data --------------------------------------------------------------------

# derive the endpoint methods table

# used methods table
entrata_used_methods_tbl <- tibble::enframe(
  entrata_used_methods,
  name = "endpoint",
  value = "method"
) |>
  tidyr::unnest(cols = c("method")) |>
  dplyr::mutate(used = TRUE)

# default methods
entrata_default_methods_tbl <- tibble::enframe(
  entrata_default_methods,
  name = "endpoint",
  value = "method"
) |>
  tidyr::unnest(cols = c("method")) |>
  dplyr::mutate(default = TRUE)

# methods table merged
entrata_methods_tbl <- tibble::enframe(
  entrata_methods,
  name = "endpoint",
  value = "method"
) |>
  tidyr::unnest(cols = c("method")) |>
  dplyr::left_join(
    y = entrata_used_methods_tbl,
    by = c("endpoint", "method")
  ) |>
  dplyr::mutate(used = ifelse(is.na(used), FALSE, TRUE)) |>
  dplyr::select(
    endpoint,
    method,
    used
  ) |>
  dplyr::left_join(
    y = entrata_default_methods_tbl,
    by = c("endpoint", "method")
  ) |>
  dplyr::mutate(default = ifelse(is.na(default), FALSE, TRUE)) |>
  dplyr::select(
    endpoint,
    method,
    used,
    default
  ) |>
  dplyr::mutate(
    version = dplyr::case_when(
      .data$method %in% entrata_r2_methods ~ "r2",
      .data$method %in% entrata_r3_methods ~ "r3",
      TRUE ~ "r1"
    )
  )


# params ------------------------------------------------------------------

entrata_params_tbl <- entrata_parameters |>
  tibble::enframe(name = "endpoint", value = "methods") |>
  tidyr::unnest_longer(methods) |>
  dplyr::mutate(
    method = names(methods)
  ) |>
  tidyr::unnest_longer(methods, values_to = "parameters") |>
  dplyr::mutate(parameter = names(parameters)) |>
  tidyr::unnest_wider(parameters, names_sep = "_") |>
  dplyr::mutate(
    multiple = dplyr::coalesce(parameters_multiple, FALSE)
  ) |>
  dplyr::select(
    endpoint,
    method,
    parameter,
    type = parameters_type,
    required = parameters_required,
    multiple,
    description = parameters_description
  ) |>
  dplyr::arrange(
    endpoint,
    method,
    dplyr::desc(required),
    parameter
  )
