get_weekly_period_start_date = function(end_date = lubridate::today()) {
  hold <- lubridate::today() - lubridate::days(7)
  out <- format(hold, "%m/%d/%Y") |> as.character()
  out
}

entrata_reports_pre_lease_weekly <- function(
  config = config::get()$entrata,
  req_id_reports = 15,
  req_id_queue = 15,
  report_name = "lease_execution_(applicant)",
  report_version = "1.8",
  property_ids = NULL,
  ...
) {

  property_group_ids <- property_ids

  if (is.null(property_group_ids)) {
    property_group_ids <- c(
      "1161867",
      "641240",
      "739084",
      "1197886",
      "1143679",
      "739079",
      "518044",
      "676055",
      "952515",
      "518041",
      "518046",
      "1197887",
      "1132027",
      "577897",
      "1161871",
      "739085",
      "518042",
      "833617",
      "1311849",
      "739080",
      "739076",
      "1115679"
    )
  }

  default_req_body <- list(
    auth = list(type = "basic"),
    requestId = 15,
    method = list(
      name = "getReportData",
      version = "r3",
      params = list(
        reportName = "lease_execution_(applicant)",
        reportVersion = "1.8", # get_latest_report_version("lease_execution_(applicant)"),
        filters = list(
          property_group_ids = property_group_ids,
          period = list(
            daterange = list(
              "daterange-start" = as.Date(get_weekly_period_start_date(), format = "%m/%d/%Y"),
              "daterange-end" = as.Date(format(lubridate::today(), "%m/%d/%Y"), format = '%m/%d/%Y')
            ),
            "period_type" = "daterange"
          ),
          results_based_on = "activity",
          lease_type = c("1", "3"),
          summarize_by = "lease_type",
          group_by = "property",
          consolidate_by = "no_consolidation",
          arrange_by_property = "0",
          subtotals = "0"
        )
      )
    )
  )

  req <- httr2::request(paste0(config$base_url)) |>
    httr2::req_auth_basic(config$username, config$password) |>
    httr2::req_body_json(default_req_body) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("reports")

  req

  resp <- httr2::req_perform(req)

  queue_id <- httr2::resp_body_json(resp) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "The report data has been successfully queued for processing.",
      "The queue ID is: {.field {queue_id}}"
    )
  )

  req_body_queue <- list(
    auth = list(type = "basic"),
    requestId = 15,
    method = list(
      name = "getResponse",
      version = "r1",
      params = list(queueId = queue_id, serviceName = "getReportData")
    )
  )

  req_queue <- httr2::request(config$base_url) |>
    httr2::req_auth_basic(config$username, config$password) |>
    httr2::req_body_json(req_body_queue) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("queue") |>
    httr2::req_retry(
      max_tries = 10,
      backoff = ~ lubridate::seconds(2 ^ (.x - 1)),
      is_transient = \(resp) {
        !is.null(httr2::resp_body_json(resp)$response$error)
      }
    )

  req_queue

  resp_queue <- httr2::req_perform(req_queue)

  resp_data_queue <- httr2::resp_body_json(resp_queue) |>
    purrr::pluck("response", "result", "reportData")

  resp_data_queue_parsed <- resp_data_queue |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select("property_name", "lease_type", "signed") |>
    tidyr::pivot_wider(names_from = lease_type, values_from = signed) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0))) |>
    dplyr::rename(new_leases = `New Lease`, new_renewals = Renewal) |>
    dplyr::mutate(new_total = new_leases + new_renewals)

  return(resp_data_queue_parsed)

}

  # results_based_on <- c("activity", "moved_in", "current_status")[[1]]
  # period <- list(
  #   daterange = list(`daterange-start` = "m/d/Y", `daterange-end` = "m/d/Y"),
  #   date = "m/d/Y",
  #   # "today,yesterday,currentwk,lastwk,currentcm,priorcm,currentcq,priorcq,currentcyr,priorcyr,daterange,date"
  #   period_type = c(
  #     "today",
  #     "yesterday",
  #     "currentwk",
  #     "lastwk",
  #     "currentcm",
  #     "priorcm",
  #     "currentcq",
  #     "priorcq",
  #     "currentcyr",
  #     "priorcyr",
  #     "daterange",
  #     "date"
  #   )
  # )
  #
  # # not being used by pat?
  # applicant <- c(1, 4, 2)
  #
  # # pat uses c('1', '3')?
  # lease_type <- c(1, 3, 5)
  #
  # # pat using 'lease_type'?
  # summarize_by <- c("property", "lease_type", "do_not_summarize")
  #
  # # pat using 'property'?
  # group_by <- c("property", "lease_type", "building_units", "do_not_group")
  #
  # # pat not using?
  # lease_terms <- list(dependencies = c("property_group_ids", "lease_occupancy_types"))
  #
  # consolidate_by <- c(
  #   "no_consolidation",
  #   "consolidate_all_properties",
  #   "consolidate_by_property_groups"
  # )
  # arrange_by_property <- c("0", "1")
  # subtotals <- c("0", "1")

  # {
  #   "response": {
  #     "requestId": "15",
  #     "code": 200,
  #     "result": {
  #       "reports": {
  #         "report": [
  #           {
  #             "name": "lease_execution_(applicant)",
  #             "description": "The purpose of the Lease Execution (Applicant) report is to be able to track lease signatures based on applicant types.",
  #             "filters": {
  #               "lease_occupancy_types": [],
  #               "property_group_ids": [
  #                 1161867,
  #                 641240,
  #                 739084,
  #                 1197886,
  #                 1143679,
  #                 739079,
  #                 518044,
  #                 676055,
  #                 952515,
  #                 518041,
  #                 518046,
  #                 1197887,
  #                 1132027,
  #                 577897,
  #                 1161871,
  #                 739085,
  #                 518042,
  #                 833617,
  #                 1311849,
  #                 739080,
  #                 739076,
  #                 1115679
  #               ],
  #               "results_based_on": "activity,moved_in,current_status",
  #               "period": {
  #                 "daterange": {
  #                   "daterange-start": "m/d/Y",
  #                   "daterange-end": "m/d/Y"
  #                 },
  #                 "date": "m/d/Y",
  #                 "period_type": "today,yesterday,currentwk,lastwk,currentcm,priorcm,currentcq,priorcq,currentcyr,priorcyr,daterange,date"
  #               },
  #               "applicant": [
  #                 1,
  #                 4,
  #                 2
  #               ],
  #               "lease_type": [
  #                 1,
  #                 3,
  #                 5
  #               ],
  #               "summarize_by": "property,lease_type,do_not_summarize",
  #               "group_by": "property,lease_type,building_units,do_not_group",
  #               "lease_terms": {
  #                 "dependencies": "property_group_ids,lease_occupancy_types"
  #               },
  #               "consolidate_by": "no_consolidation,consolidate_all_properties,consolidate_by_property_groups",
  #               "arrange_by_property": "0,1",
  #               "subtotals": "0,1"
  #             }
  #           }
  #         ]
  #       }
  #     }
  #   }
  # }


