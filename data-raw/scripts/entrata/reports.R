#  ------------------------------------------------------------------------
#
# Title : Entrata Reports
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

source("data-raw/R/utils_entrata.R")

entrata_config <- get_entrata_config()

resp_report_list <- entrata_base_request() |>
  httr2::req_url_path_append("reports") |>
  httr2::req_body_json(
    list(
      auth = list(type = "basic"),
      request_id = 15L,
      method = list(
        name = "getReportList",
        version = "r1",
        params = list()
      )
    )
  ) |>
  entrata_req_perform()

resp_report_list_json <- resp_report_list |> httr2::resp_body_json()

resp_report_list_content <- resp_report_list_json |>
  purrr::pluck("response", "result", "reports", "report")

entrata_report_versions_tbl <- purrr::map(
  resp_report_list_content,
  function(x) {
    report_id <- x$id
    report_name <- x$reportName
    report_versions <- purrr::map_dfr(
      x$reportVersions$reportVersion,
      function(y) {
        tibble::tibble(
          version = y$version,
          is_latest = y$isLatest
        )
      }
    )
    dplyr::mutate(
      report_versions,
      report_id = report_id,
      report_name = report_name
    )
  }
) |>
  dplyr::bind_rows() |>
  dplyr::select(
    report_id,
    report_name,
    report_version = version,
    is_latest
  )

entrata_report_latest_versions_tbl <- entrata_report_versions_tbl |>
  dplyr::filter(is_latest)

entrata_report_names_lst <- entrata_report_latest_versions_tbl$report_name |>
  unique()

# cleanup
rm(
  resp_report_list,
  resp_report_list_json,
  resp_report_list_content
)
