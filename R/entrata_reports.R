# report version ----------------------------------------------------------

#' Get Latest Report Version from Entrata
#'
#' @description
#' This function retrieves the latest version of a specified report from Entrata.
#' It's useful when you need to ensure you're working with the most up-to-date
#' version of a particular report.
#'
#' @param report_name A character string representing the name of the report.
#'
#' @return A character string representing the latest version of the report.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get the latest version of the "pre_lease" report
#' latest_version <- get_latest_report_version("pre_lease")
#' print(latest_version)
#' }
#'
#' @seealso
#' \code{\link{get_entrata_reports_list}} for getting a list of all available reports
#' \code{\link{get_entrata_report_info}} for getting detailed information about a specific report
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang .data .env
get_latest_report_version <- function(report_name) {

  validate_entrata_report_name(report_name)

  latest_report_version <- get_entrata_reports_list(latest_only = TRUE) |>
    dplyr::filter(
      .data$report_name == {{ report_name }}
    ) |>
    dplyr::pull("report_version")

  return(latest_report_version)

}

# reports list ------------------------------------------------------------

#' Get Entrata Reports List
#'
#' @description
#' This function retrieves a list of reports available in the
#' Entrata system, optionally filtering to only the latest version of each report.
#'
#' @param latest_only Logical, if TRUE (default), returns only the latest version
#' of each report.
#'
#' @return A tibble containing report information including report name, ID,
#' system name, and version.
#'
#' @export
#'
#' @importFrom dplyr filter left_join select
#' @importFrom tibblify tspec_df tib_int tib_chr tib_row tib_df tib_lgl
#' @importFrom purrr pluck set_names list_rbind
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom rlang .data .env
get_entrata_reports_list <- function(latest_only = TRUE) {
  req <- entrata(endpoint = "reports", method = "getReportList")
  res <- httr2::req_perform(req)
  res_content <- res |>
    httr2::resp_body_json()

  reports_specs <- list(
    "getReportList" = list(
      main = tibblify::tspec_df(
        tibblify::tib_int("id"),
        tibblify::tib_chr("reportName"),
        tibblify::tib_chr("systemName"),
        tibblify::tib_row(
          "reportVersions",
          tibblify::tib_df(
            "reportVersion",
            tibblify::tib_chr("version"),
            tibblify::tib_lgl("isLatest"),
            tibblify::tib_chr("titleAddendum", required = FALSE),
            tibblify::tib_chr("expiryDate", required = FALSE),
          )
        )
      ),
      versions = tibblify::tspec_row(
        tibblify::tib_chr("version"),
        tibblify::tib_lgl("isLatest"),
        tibblify::tib_chr("titleAddendum", required = FALSE),
        tibblify::tib_chr("expiryDate", required = FALSE)
      )
    )
  )

  res_data <- res_content |>
    purrr::pluck("response", "result", "reports", "report") |>
    tibblify::tibblify(reports_specs$getReportList$main)

  report_names <- res_data$reportName

  res_data_report_versions <- purrr::pluck(
    res_data,
    "reportVersions",
    "reportVersion"
  ) |>
    rlang::set_names(report_names) |>
    purrr::list_rbind(names_to = "report_name")

  res_data_merged <- dplyr::select(
    res_data,
    "report_id" = "id",
    "report_name" = "reportName",
    "system_name" = "systemName",
    -c("reportVersions")
  ) |>
    dplyr::left_join(
      res_data_report_versions,
      by = "report_name"
    ) |>
    dplyr::select(
      "report_id",
      "report_name",
      "system_name",
      "report_version" = "version",
      "is_latest" = "isLatest",
      -c("titleAddendum", "expiryDate")
    )

  if (!latest_only) {
    return(res_data_merged)
  }

  res_data_merged |>
    dplyr::filter(
      .data$is_latest == TRUE
    ) |>
    dplyr::select(
      -c("is_latest")
    )
}

# report info -------------------------------------------------------------

#' Get Entrata Report Info
#'
#' @description
#' This function retrieves detailed information for a specified
#' report from Entrata, including filters and descriptions.
#'
#' @param report_name A character string representing the name of the report.
#' @param report_version A character string representing the version of the report.
#'   Defaults to "latest".
#'
#' @return A list containing report name, description, and filters.
#'
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang .data .env
#' @importFrom purrr pluck map list_rbind
#' @importFrom httr2 req_perform resp_body_json
get_entrata_report_info <- function(report_name, report_version = "latest") {

  validate_entrata_report_name(report_name)

  if (report_version == "latest") {
    latest_report_version <- mem_get_entrata_reports_list(latest_only = TRUE) |>
      dplyr::filter(
        "report_name" == {{ report_name }}
      ) |>
      dplyr::pull("report_version")
  }

  req <- entrata(
    endpoint = "reports",
    method = "getReportInfo",
    method_params = list(
      reportName = report_name,
      reportVersion = latest_report_version
    )
  )

  res <- httr2::req_perform(req)

  res_content <- res |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "reports", "report", 1)

  res_report_name <- res_content |> purrr::pluck("name")
  res_report_description <- res_content |> purrr::pluck("description")
  res_report_filters <- res_content |>
    purrr::pluck("filters", "filter") |>
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind()

  res_report_info <- list(
    report_name = res_report_name,
    report_description = res_report_description,
    report_filters = res_report_filters
  )

  return(res_report_info)

}
