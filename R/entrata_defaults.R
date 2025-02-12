
#  ------------------------------------------------------------------------
#
# Title : Entrata Default Values Functions
#    By : Jimmy Briggs
#  Date : 2024-12-31
#
#  ------------------------------------------------------------------------

# defaults ----------------------------------------------------------------

get_default_entrata_request_id <- function() {
  # getOption("entrata.request_id") %||%
  #   Sys.getenv("ENTRATA_REQUEST_ID", unset = NULL) %||%
  as.integer(Sys.time())
}

get_default_request_timeout <- function() {
  # getOption("entrata.request_timeout") %||%
  #   Sys.getenv("ENTRATA_REQUEST_TIMEOUT", unset = NULL) %||%
  60
}

get_default_request_retries <- function() {
  # getOption("entrata.request_retries") %||%
  #   Sys.getenv("ENTRATA_REQUEST_RETRIES", unset = NULL) %||%
  3
}

get_default_entrata_method <- function(endpoint) {
  validate_entrata_endpoint(endpoint)
  entrata_default_methods_lst[[endpoint]]
}

get_default_entrata_method_version <- function(endpoint, method) {
  validate_entrata_endpoint(endpoint)
  validate_entrata_method_name(endpoint, method_name = method)
  entrata_method_versions_lst[[method]]
}

get_default_entrata_method_params <- function(endpoint, method) {
  validate_entrata_endpoint(endpoint)
  validate_entrata_method_name(method)
  entrata_params_lst[[endpoint]][[method]]
}

get_default_entrata_report_version <- function(report_name) {

  validate_entrata_report_name(report_name)

  entrata_reports_list(latest_only = TRUE) |>
    dplyr::filter(
      .data$report_name == {{ report_name }}
    ) |>
    dplyr::pull("report_version")

}
