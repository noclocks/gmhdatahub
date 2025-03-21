
#  ------------------------------------------------------------------------
#
# Title : Checking Utilities
#    By : Jimmy Briggs
#  Date : 2024-11-29
#
#  ------------------------------------------------------------------------


# topic -----------------------------------------------------------------------------------------------------------

#' Check Utility Functions
#'
#' @name utils_checks
#'
#' @description
#' A collection of functions for checking the validity of various R objects,
#' primarily used for argument validation in functions.
#'
#' @details
#' Functions:
#'
#' @param obj,chart,tbl,req,resp,val The object to check the class of.
#' @param class The class to check against.
#' @param arg,obj_arg,call [rlang::args_error_context] Arguments for error messaging.
#'
#' @importFrom cli cli_abort
#' @importFrom rlang caller_arg caller_env
NULL


# gmh specific ----------------------------------------------------------------------------------------------------

#' @rdname utils_checks
#' @export
check_property_id <- function(id, arg = rlang::caller_arg(id), call = rlang::caller_env()) {
  id_val <- tryCatch(as.integer(id), warning = function(w) NA)
  if (is.na(id_val)) {
    cli::cli_abort(
      "Invalid {.arg id} provided: Property IDs must be a valid integer.",
      call = call
    )
  }
  valid_ids <- entrata_properties_tbl$id |> as.integer()
  if (!id_val %in% valid_ids) {
    closest_match <- get_closest_match(id_val, valid_ids)
    closest_matching_name <- entrata_properties_tbl$name[entrata_properties_tbl$id == closest_match]
    cli::cli_abort(
      "Invalid {.arg id} provided: Did you mean {.field {closest_match}} ({.field {closest_matching_name}})?",
      call = call
    )
  }
  invisible(id_val)
}

#' @rdname utils_checks
#' @export
check_property_name <- function(name, arg = rlang::caller_arg(name), call = rlang::caller_env()) {
  name_val <- as.character(name)
  valid_names <- entrata_properties_tbl$name |> as.character()
  if (!name_val %in% valid_names) {
    closest_match <- get_closest_match(name_val, valid_names)
    closest_matching_id <- entrata_properties_tbl$id[entrata_properties_tbl$name == closest_match]
    cli::cli_abort(
      "Invalid {.arg name} provided: Did you mean {.field {closest_match}} ({.field {closest_matching_id}})?",
      call = call
    )
  }
  invisible(name_val)
}

# classes ---------------------------------------------------------------------------------------------------------

#' @rdname utils_checks
#' @export
check_inherits <- function(
  obj,
  class,
  obj_arg = rlang::caller_arg(obj),
  call = rlang::caller_env()
) {

  if (!inherits(obj, class)) {
    cli::cli_abort(
      "{.arg {obj_arg}} must inherit from class {.cls {class}}, not {.obj_type_friendly {obj}}.",
      call = call
    )
  }

  invisible(TRUE)

}

#' @rdname utils_checks
#' @export
check_chart <- function(chart, arg = rlang::caller_arg(chart), call = rlang::caller_env()) {
  check_inherits(chart, "htmlwidget", obj_arg = arg, call = call)
  check_inherits(chart, "apex", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_tibble <- function(tbl, arg = rlang::caller_arg(tbl), call = rlang::caller_env()) {
  check_inherits(tbl, "tbl_df", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_request <- function(req, arg = rlang::caller_arg(req), call = rlang::caller_env()) {
  check_inherits(req, "httr2_request", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_response <- function(resp, arg = rlang::caller_arg(resp), call = rlang::caller_env()) {
  check_inherits(resp, "httr2_response", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_reactable <- function(tbl, arg = rlang::caller_arg(obj), call = rlang::caller_env()) {
  check_inherits(chart, "htmlwidget", obj_arg = arg, call = call)
  check_inherits(chart, "reactable", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_tag <- function(obj, arg = rlang::caller_arg(obj), call = rlang::caller_env()) {
  check_inherits(obj, "shiny.tag", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_taglist <- function(obj, arg = rlang::caller_arg(obj), call = rlang::caller_env()) {
  check_inherits(obj, "shiny.tag.list", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_bslib_page <- function(obj, arg = rlang::caller_arg(obj), call = rlang::caller_env()) {
  check_taglist(obj, arg = arg, call = call)
  check_inherits(obj, "bslib_page", obj_arg = arg, call = call)
}

#' @rdname utils_checks
#' @export
check_percent <- function(val, arg = rlang::caller_arg(val), call = rlang::caller_env()) {
  if (!is.numeric(val)) cli::cli_abort("{.arg {arg}} must be numeric.", call = call)
  if (val < 0 | val > 1) cli::cli_abort("{.arg {arg}} must be between 0 and 1.", call = call)
  return(invisible(val))
}


# dates -----------------------------------------------------------------------------------------------------------

#' @rdname utils_checks
#' @export
check_date <- function(
    date,
    arg = rlang::caller_arg(date),
    call = rlang::caller_env()
) {
  if (!lubridate::is.Date(date) && !lubridate::is.POSIXt(date)) {
    cli::cli_abort(
      c(
        "Invalid Date Provided: {.arg {arg}}",
        "The provided date is not a valid date object."
      ),
      call = call
    )
  }
  return(invisible(date))
}

#' @rdname utils_checks
#' @export
check_entrata_date <- function(date, arg = rlang::caller_arg(date), call = rlang::caller_env()) {
  if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) {
    cli::cli_abort(
      c(
        "Invalid Date Format Provided: {.arg {arg}}",
        "The provided date is not in the format 'MM/DD/YYYY'."
      ),
      call = call
    )
  }
  return(TRUE)
}

#' @rdname utils_checks
#' @export
check_entrata_daterange <- function(daterange, arg = rlang::caller_arg(daterange), call = rlang::caller_env()) {
  if (!is.list(daterange) || !all(c("daterange-start", "daterange-end") %in% names(daterange))) {
    cli::cli_abort(
      c(
        "Invalid Date Range Provided: {.arg {arg}}",
        "The provided date range must be a list with 'daterange-start' and 'daterange-end' keys."
      ),
      call = call
    )
  }
  check_entrata_date(daterange[[1]], call = call)
  check_entrata_date(daterange[[2]], call = call)
  if (as.Date(daterange[[1]]) > as.Date(daterange[[2]])) {
    cli::cli_abort(
      c(
        "Invalid Date Range Provided: {.arg {arg}}",
        "The start date must be before the end date."
      ),
      call = call
    )
  }
  return(TRUE)
}

# packages --------------------------------------------------------------------------------------------------------

#' @rdname utils_checks
#' @export
#' @importFrom cli cli_abort
#' @importFrom rlang is_installed caller_arg caller_env
check_installed <- function(pkg, arg = rlang::caller_arg(pkg), call = rlang::caller_env()) {
  if (!rlang::is_installed(pkg)) {
    cli::cli_abort("Package {.pkg {arg}} is not installed.", call = call)
  }
  invisible(NULL)
}


# validation ------------------------------------------------------------------------------------------------------

#' Validate Column Names
#'
#' @description
#' This function validates the column names of a data frame. It is used
#' throughout the package to ensure that the data frame has the required
#' columns before attempting to use or process the data.
#'
#' @param data The data frame to validate against.
#' @param req_cols Character vector of required column names.
#' @param optional_cols Character vector of optional column names.
#' @inheritParams rlang::args_error_context
#'
#' @returns
#' Returns invisibly if the data frame has the required columns.
#'
#' @export
#'
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang caller_env
#'
#' @examples
#' validate_col_names(mtcars, c("mpg", "cyl"))
validate_col_names <- function(
    data,
    req_cols,
    optional_cols = NULL,
    call = rlang::caller_env()
) {

  stopifnot(
    is.data.frame(data) || tibble::is_tibble(data),
    is.vector(req_cols)
  )

  missing_req_cols <- setdiff(req_cols, colnames(data))

  if (length(missing_req_cols) > 0) {
    cli::cli_abort(
      c(
        "The following required columns are missing from the provided {.arg data}:\n",
        "{.field {missing_req_cols}}"
      ),
      call = call
    )
  }

  if (!is.null(optional_cols) && length(optional_cols) > 0) {
    stopifnot(is.vector(optional_cols))
    missing_opt_cols <- setdiff(optional_cols, colnames(data))
    if (length(missing_opt_cols) > 0) {
      cli::cli_alert_warning(
        c(
          "The following optional columns are missing from the provided {.arg data}:\n",
          "{.field {missing_opt_cols}}"
        )
      )
    }
  }

  return(invisible(data))

}
