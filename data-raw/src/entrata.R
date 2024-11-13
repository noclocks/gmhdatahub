
#  ------------------------------------------------------------------------
#
# Title : Entrata Data Preparation
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------


# sources -----------------------------------------------------------------

entrata_path <- "data-raw/src/entrata"
entrata_scripts <- fs::dir_ls(entrata_path, type = "file", glob = "*.R")
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


# req_validate_params <- function(req, ...) {
#
#   check_request(req)
#
#   # extract method object from request body
#   req_method <- req$body$data$method
#
#   # method name, version, and params
#   req_method_name <- req_method$name
#   req_method_version <- req_method$version
#   req_method_params <- req_method$params
#
#   # get values to validate with
#   valid_method_params <- entrata_params_tbl |>
#     dplyr::filter(
#       .data$method == .env$req_method_name
#     )
#
#   validated_params <- vector("list", length(valid_method_params))
#
#   for (i in seq_len(nrow(valid_method_params))) {
#     param_name <- valid_method_params$parameter[i]
#     param_type <- valid_method_params$type[i]
#     param_required <- valid_method_params$required[i]
#     param_multiple <- valid_method_params$multiple[i]
#
#     # check if parameter exists in the request
#     if (!param_name %in% names(req_method_params)) {
#       if (param_required) {
#         cli::cli_abort(
#           "Missing required request body parameter for {.field {param_name}}."
#         )
#       } else {
#         next
#       }
#     }
#
#     param_value <- req_method_params[[param_name]]
#     if (!is.null(param_value)) {
#
#     }
