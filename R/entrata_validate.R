
validate_entrata_endpoint <- function(
    endpoint,
    arg = rlang::caller_arg(endpoint),
    call = rlang::caller_env()
) {

  if (!(endpoint %in% .entrata_endpoints)) {
    cli::cli_abort(
      c(
        "Invalid Entrata API Endpoint Provided: {.arg {arg}}",
        "i" = "Valid Entrata API Endpoints: {.field {.entrata_endpoints}}"
      ),
      call = call
    )
  }

  return(invisible())

}

validate_entrata_method_name <- function(
  method_name,
  arg = rlang::caller_arg(method_name),
  call = rlang::caller_env()
) {

  if (!(method_name %in% .entrata_method_names)) {
    cli::cli_abort(
      c(
        "Invalid {.arg method_name} provided: {.field {method_name}}.\n",
        "Must be one of: {.field {.entrata_method_names}}."
      ),
      call = call
    )
  }

  return(invisible())

}

validate_entrata_method_version <- function(
  method_name,
  method_version,
  arg = rlang::caller_arg(method_version),
  call = rlang::caller_env()
) {

  validate_entrata_method_name(method_name, call = call)
  default_version <- entrata_method_versions_lst[[method_name]]
  valid_versions <- if (default_version == "r3") {
    c("r1", "r2", "r3")
  } else if (default_version == "r2") {
    c("r1", "r2")
  } else {
    c("r1")
  }

  if (!(method_version %in% valid_versions)) {
    cli::cli_abort(
      c(
        "Invalid {.arg method_version} provided: {.field {method_version}}.\n",
        "Must be one of: {.field {valid_versions}}."
      ),
      call = call
    )
  }

  return(invisible())

}

validate_entrata_method_params <- function(
    endpoint,
    method_name,
    method_params,
    method_version = get_default_entrata_method_version(endpoint, method_name),
    arg = rlang::caller_arg(method_params),
    call = rlang::caller_env()
) {

  stopifnot(rlang::is_list(method_params) && rlang::is_named(method_params))
  validate_entrata_method_name(endpoint, method_name)

  params_tbl <- entrata_params_tbl |>
    dplyr::filter(.data$endpoint == .env$endpoint,
                  .data$method == .env$method_name)

  valid_params <- entrata_method_params[[endpoint]][[method_name]]

  if (is.null(valid_params)) {
    cli::cli_alert_info(
      c(
        "No expected method parameters for the {.field {method_name}} method."
      ),
      call = call
    )
    return(invisible())
  }

  for (param in names(method_params)) {

    # check if param is valid
    if (!(param %in% names(valid_params))) {
      cli::cli_alert_warning(
        c(
          "Unexpected Entrata API Method Parameter Provided: {.field {param}}",
          "i" = "Valid Entrata API Method Parameters for the {.field {method_name}} Method: ",
          "{.field {valid_params}}"
        ),
        call = call
      )
      next
    }

    param_value <- method_params[[param]]
    param_info <- valid_params[[param]]
    param_type <- param_info[["type"]]
    param_required <- param_info[["required"]]

    if (param_required && is.null(param_value)) {
      cli::cli_abort(
        c(
          "Required Entrata API Method Parameter Missing: {.field {param}}",
          "i" = "The {.field {param}} parameter is required for the {.field {method_name}} method."
        ),
        call = call
      )
    }

    if (!is.null(param_value)) {

      validate_entrata_method_param_type(param, param_value, call = call)

      if (param_type == "character") {
        stopifnot(is.character(param_value))
      } else if (param_type == "numeric") {
        stopifnot(is.numeric(param_value))
      } else if (param_type == "logical") {
        stopifnot(is.logical(param_value))
      } else if (param_type == "list") {
        stopifnot(is.list(param_value))
      } else {
        cli::cli_abort(
          c(
            "Invalid Entrata API Method Parameter Type Provided: {.field {param}}",
            "i" = "Valid Entrata API Method Parameter Types: ",
            "character, numeric, logical, list"
          ),
          call = call
        )
      }

    }

  }

}

validate_entrata_report_name <- function(
    report_name,
    arg = rlang::caller_arg(report_name),
    call = rlang::caller_env()
) {

  if (!(report_name %in% .entrata_report_names)) {
    cli::cli_abort(
      c(
        "Invalid Entrata Report Name Provided: {.arg {arg}}",
        "i" = "Valid Entrata Report Names: {.field {.entrata_report_names}}"
      ),
      call = call
    )
  }

  return(invisible())

}

validate_entrata_report_version <- function(
  report_name,
  report_version,
  arg = rlang::caller_arg(report_version),
  call = rlang::caller_env()
) {

  validate_entrata_report_name(report_name, call = call)
  default_version <- entrata_report_versions_lst[[report_name]]
  valid_versions <- if (default_version == "r3") {
    c("r1", "r2", "r3")
  } else if (default_version == "r2") {
    c("r1", "r2")
  } else {
    c("r1")
  }

  if (!(report_version %in% valid_versions)) {
    cli::cli_abort(
      c(
        "Invalid {.arg report_version} provided: {.field {report_version}}.\n",
        "Must be one of: {.field {valid_versions}}."
      ),
      call = call
    )
  }

  return(invisible())

}

validate_entrata_property_id <- function(property_id) { }

validate_entrata_period_type <- function(
  period_type,
  arg = rlang::caller_arg(period_type),
  call = rlang::caller_env()
) {

  if (!period_type %in% .period_types) {
    cli::cli_abort(
      c(
        "Invalid {.arg period_type} provided: {.field {period_type}}.\n",
        "Must be one of: {.field {.period_types}}."
      ),
      call = call
    )
  }

  return(invisible())

}

.entrata_method_names <- entrata_methods_tbl$method_name

.entrata_period_types <- c(
  "date",
  "daterange",
  "today",
  "yesterday",
  "currentwk",
  "lastwk",
  "currentcm",
  "priorcm",
  "currentcq",
  "priorcq",
  "currentcyr",
  "priorcyr"
)
