
#  ------------------------------------------------------------------------
#
# Title : Entrata API Data Pipeline
#    By : Jimmy Briggs
#  Date : 2024-11-12
#
#  ------------------------------------------------------------------------


# config ------------------------------------------------------------------

load_entrata_config <- function(path = pkg_sys("config/config.yml")) {
  config::get("entrata", file = path)
}

# get initial properties --------------------------------------------------

get_entrata_properties <- function(entrata_config = load_entrata_config()) {
  httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("properties") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getProperties",
          version = "r1",
          params = list(NULL)
        )
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    purrr::map_dfr(
      function(x) {
        tibble::tibble(
          property_id = x$PropertyID,
          property_name = x$MarketingName,
          property_type = x$Type
        )
      }
    )
}

# leasing report summary and details --------------------------------------

request_leasing_report <- function(
  entrata_config = NULL,
  property_ids = NULL,
  report_date = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- load_entrata_config() }
  if (is.null(property_ids)) { property_ids <- get_entrata_properties()$property_id }
  if (is.null(report_date)) {
    now <- lubridate::today()
    current_month <- lubridate::month(now)
    if (current_month < 9) {
      report_date_year <- lubridate::year(now)
    } else {
      report_date_year <- lubridate::year(now) + 1
    }
    report_date <- paste0("09/01/", report_date_year)
  }

  req_reports <- httr2::request(entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("reports") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getReportData",
          version = "r3",
          params = list(
            reportName = "pre_lease",
            reportVersion = "3.2",
            filters = list(
              property_group_ids = as.list(as.character(property_ids)),
              period = list(date = report_date, period_type = "date"),
              summarize_by = "property",
              group_by = "do_not_group",
              consider_pre_leased_on = "332",
              charge_code_detail = 1,
              space_options = 'do_not_show',
              additional_units_shown = 'available',
              combine_unit_spaces_with_same_lease = 0,
              consolidate_by = 'no_consolidation',
              arrange_by_property = 0,
              subtotals = list("summary", "details"),
              yoy = 1
            )
          )
        )
      )
    )

  resp_reports <- httr2::req_perform(req_reports)
  resp_queue_id <- httr2::resp_body_json(resp_reports) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "The report data has been successfully queued for processing.",
      "The queue ID is: {.field {resp_queue_id}}"
    )
  )

  return(resp_queue_id)

}


# weekly leasing report ---------------------------------------------------

request_weekly_prelease_data <- function(
  entrata_config = NULL,
  property_ids = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- load_entrata_config() }
  if (is.null(property_ids)) { property_ids <- get_entrata_properties()$property_id }

  req_reports <- httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("reports") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = 'basic'),
        method = list(
          name = "getReportData",
          version = "1.8",
          params = list(
            reportName = 'lease_execution_(applicant)',
            filters = list(
              property_group_ids = as.list(as.character(property_ids)),
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
    )

  resp_reports <- httr2::req_perform(req_reports)
  resp_queue_id <- httr2::resp_body_json(resp_reports) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "The report data has been successfully queued for processing.",
      "The queue ID is: {.field {resp_queue_id}}"
    )
  )

  return(resp_queue_id)

}


# retrieve report data ----------------------------------------------------

retrieve_report_data <- function(
  queue_id,
  api_config = NULL
) {

  if (is.null(api_config)) { api_config <- load_entrata_config() }

  req_queue <- httr2::request(base_url = api_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("queue") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = api_config$username,
      password = api_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getResponse",
          version = "r1",
          params = list(
            queueId = queue_id,
            serviceName = "getReportData"
          )
        )
      )
    ) |>
    httr2::req_retry(
      max_tries = 10,
      backoff = ~ lubridate::seconds(2 ^ (.x - 1)),
      is_transient = \(resp) {
        !is.null(httr2::resp_body_json(resp)$response$error)
      }
    )

  resp_queue <- httr2::req_perform(req_queue)

  resp_data <- httr2::resp_body_json(resp_queue) |>
    purrr::pluck("response", "result", "reportData") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select("property_name", "lease_type", "signed") |>
    tidyr::pivot_wider(names_from = lease_type, values_from = signed) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0))) |>
    dplyr::rename(new_leases = `New Lease`, new_renewals = Renewal) |>
    dplyr::mutate(new_total = new_leases + new_renewals)

  return(resp_data)

}


# parse, process, merge, transform data  ----------------------------------

prepare_summary_data <- function(report_data, weekly_report_data, ...) {

  report_data <- report_data |>
    dplyr::mutate(
      new_leases = as.numeric(new_leases),
      new_renewals = as.numeric(new_renewals),
      new_total = as.numeric(new_total)
    )

  weekly_report_data <- weekly_report_data |>
    dplyr::mutate(
      new_leases = as.numeric(new_leases),
      new_renewals = as.numeric(new_renewals),
      new_total = as.numeric(new_total)
    )

  summary_data <- dplyr::full_join(report_data, weekly_report_data, by = "property_name") |>
    dplyr::mutate(
      new_leases = coalesce(new_leases.x, new_leases.y),
      new_renewals = coalesce(new_renewals.x, new_renewals.y),
      new_total = coalesce(new_total.x, new_total.y)
    ) |>
    dplyr::select(-dplyr::starts_with("new_"))

  return(summary_data)


}




# investment partner/portfolio mapping ------------------------------------

# investment_partners_mapping <- readr::read_csv("data-raw/investment_partners.csv")

map_property_to_investment_partner <- function(property_name) {
  investment_partners_mapping |>
    dplyr::filter(.data$property_name == .env$property_name) |>
    dplyr::pull(.data$investment_partner)
}


# manual bed counts -------------------------------------------------------

# model_beds_tbl <- investment_partners_mapping |>
#   dplyr::mutate(model_beds = 0) |>
#   dplyr::arrange(.data$property_name)
#
# model_beds_tbl$model_beds[1] <- 4

# get main summary table --------------------------------------------------

# pre_lease_summary_data <- entrata_pre_lease_report()

