
#  ------------------------------------------------------------------------
#
# Title : Entrata Database Functions
#    By : Jimmy Briggs
#  Date : 2025-03-04
#
#  ------------------------------------------------------------------------

db_read_entrata_pre_lease_details_tbl <- function(pool) {

  check_db_conn(pool)

  excl_cols <- c(
    "lease_started_on_date",
    "lease_partially_completed_on_date",
    "lease_approved_on_date",
    "ledger_name",
    "deposit_held",
    "advertised_rate",
    "scheduled_rent_total"
  )

  tryCatch({

    hold <- db_read_tbl(pool, "entrata.pre_lease_report_details_w_charge_codes", collect = FALSE) |>
      dplyr::filter(
        .data$report_date == max(.data$report_date, na.rm = TRUE)
      ) |>
      dplyr::select(-tidyselect::all_of(excl_cols)) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.numeric),
          function(x) {
            as.numeric(x) |> dplyr::coalesce(0.00)
          }
        ),
        dplyr::across(
          tidyselect::where(is.integer),
          function(x) {
            as.integer(x) |> dplyr::coalesce(0L)
          }
        )
      ) |>
      dplyr::arrange(
        .data$property_name,
        .data$unit_name
      )

  }, error = function(e) {

    cli::cli_alert_danger(
      c(
        "Failed to read the Pre-Lease Details table from the database.\n",
        "Error: {.error {e$message}}"
      )
    )

    return(NULL)

  })

}

db_upsert_entrata_pre_lease_summary_by_property <- function(pool, summary_data) {

  check_db_conn(pool)

  validate_col_names(
    summary_data,
    req_cols = c(
      "report_date",
      "property_id",
      "property_name",
      "total_unit_count",
      "excluded_unit_count",
      "rentable_unit_count",
      "occupied_unit_count",
      "available_unit_count"
    )
  )

  tryCatch({

    pool::poolWithTransaction(
      pool,
      function(conn) {

        dbx::dbxUpsert(
          conn,
          DBI::SQL("entrata.report_pre_lease_summary_by_property"),
          records = summary_data,
          where_cols = c("report_date", "property_id"),
          skip_existing = FALSE
        )

        n_records <- nrow(summary_data)

        cli::cli_alert_success(
          c(
            "Successfully upserted {.field {n_records}} from {.field summary_data} into {.field entrata.report_pre_lease_summary_by_property}."
          )
        )

      }
    )

  }, error = function(e) {

    cli::cli_alert_danger(
      c(
        "Failed to upsert records from {.field summary_data} into {.field entrata.report_pre_lease_summary_by_property}.\n",
        "Error: {.error {e$message}}."
      )
    )

    return(NULL)

  })

}

db_upsert_entrata_pre_lease_details_by_property <- function(pool, details_data) {

  check_db_conn(pool)

  validate_col_names(
    details_data,
    req_cols = c(
      "report_date",
      "property_id",
      "property_name",
      "bldg_unit",
      "unit_type",
      "charge_code",
      "lease_id",
      "deposit_charged",
      "deposit_held",
      "market_rent",
      "budgeted_rent",
      "scheduled_rent",
      "advertised_rate",
      "actual_charges",
      "scheduled_rent_total"
    )
  )

  tryCatch({

    pool::poolWithTransaction(
      pool,
      function(conn) {

        dbx::dbxUpsert(
          conn,
          DBI::SQL("entrata.report_pre_lease_details_by_property"),
          records = details_data,
          where_cols = c("report_date", "property_id", "bldg_unit", "unit_type", "lease_id", "charge_code"),
          skip_existing = FALSE
        )

        n_records <- nrow(details_data)

        cli::cli_alert_success(
          c(
            "Successfully upserted {.field {n_records}} from {.field details_data} into {.field entrata.report_pre_lease_details_by_property}."
          )
        )

      }
    )


  }, error = function(e) {

    cli::cli_alert_danger(
      c(
        "Failed to upsert records from {.field details_data} into {.field entrata.report_pre_lease_details_by_property}.\n",
        "Error: {.error {e$message}}."
      )
    )

    return(NULL)

  })

}



db_upsert_entrata_pre_lease_summary_by_property_unit <- function(
    pool,
    summary_data,
    report_req_json = NULL,
    report_resp_json = NULL,
    queue_req_json = NULL,
    queue_resp_json = NULL,
    report_params_json = NULL
) {

  check_db_conn(pool)

  validate_col_names(
    summary_data,
    c(
      "report_date",
      "property_id",
      "property_name",
      "unit_type"
    )
  )

  tryCatch({

    pool::poolWithTransaction(
      pool,
      function(conn) {

        dbx::dbxUpsert(
          conn,
          DBI::SQL("entrata.pre_lease_summary_by_property_unit"),
          records = summary_data,
          where_cols = c("report_date", "property_id", "unit_type"),
          skip_existing = FALSE
        )

        cli::cli_alert_success(
          c(
            "Successfully upserted {.field summary_data} to {.field entrata.pre_lease_summary_by_property_unit}"
          )
        )

        if (
          !is.null(report_req_json) &&
          !is.null(report_resp_json) &&
          !is.null(queue_req_json) &&
          !is.null(queue_resp_json) &&
          !is.null(report_params_json)
        ) {

          report_req_json_lst <- jsonlite::fromJSON(report_req_json)
          report_resp_json_lst <- jsonlite::fromJSON(report_resp_json)
          queue_req_json_lst <- jsonlite::fromJSON(queue_req_json)
          queue_resp_json_lst <- jsonlite::fromJSON(queue_resp_json)
          report_params_json_lst <- jsonlite::fromJSON(report_params_json)

          # get queue id and start/end
          queue_id <- purrr::pluck(report_resp_json_lst, "response", "result", "queueId")
          queue_start <- purrr::pluck(queue_resp_json_lst, "response", "result", "queueStartedOn") |>
            lubridate::mdy_hms()
          queue_end <- purrr::pluck(queue_resp_json_lst, "response", "result", "queueCompletedOn") |>
            lubridate::mdy_hms()
          queue_duration <- as.integer(difftime(queue_end, queue_start, units = "secs"))

          # get request id
          request_id <- purrr::pluck(report_req_json_lst, "request_id")

          logs_data_report <- tibble::tibble(
            log_date = Sys.Date(),
            entrata_endpoint = "/reports",
            entrata_operation = "getReportData",
            entrata_request_id = request_id,
            entrata_request_start = as.POSIXct(NA),
            entrata_request_end = as.POSIXct(NA),
            entrata_request_duration = as.integer(NA),
            entrata_request_json = as.character(report_req_json),
            entrata_response_json = as.character(report_resp_json),
            entrata_report_params_json = as.character(report_params_json),
            entrata_report_name = "pre_lease",
            entrata_report_queue_id = queue_id,
            log_comments = "Pre-Lease Report by Unit Type Report Request/Response"
          )

          logs_data_queue <- tibble::tibble(
            log_date = Sys.Date(),
            entrata_endpoint = "/queue",
            entrata_operation = "getResponse",
            entrata_request_id = request_id,
            entrata_request_start = queue_start,
            entrata_request_end = queue_end,
            entrata_request_duration = queue_duration,
            entrata_request_json = as.character(queue_req_json),
            entrata_response_json = as.character(queue_resp_json),
            entrata_report_params_json = as.character(report_params_json),
            entrata_report_name = "pre_lease",
            entrata_report_queue_id = queue_id,
            log_comments = "Pre-Lease Report by Unit Type Queue Request/Response"
          )

          logs_data <- dplyr::bind_rows(logs_data_report, logs_data_queue)

          dbx::dbxInsert(
            conn,
            table = DBI::SQL("entrata.api_logs"),
            records = logs_data
          )

          cli::cli_alert_success(
            c(
              "Successfully inserted API request/response logs into {.field entrata.api_logs}."
            )
          )


        }

      }
    )

  }, error = function(e) {

    cli::cli_alert_danger(
      c(
        "Failed to updated database with the Pre-Lease Summary by Property Unit Data\n",
        "Error: {.error {e$message}}."
      )
    )

    return(NULL)

  })

}

db_upsert_entrata_pre_lease_by_property <- function(pool, resp_lst) {

  check_db_conn(pool)

  req_names <- c("summary_data", "details_data")
  if (!all(req_names %in% names(resp_lst))) {
    missing_names <- setdiff(req_names, names(resp_lst))
    cli::cli_abort("Missing required response data in {.arg resp_lst}: {.field {missing_names}}.")
  }

  tryCatch({

    pool::poolWithTransaction(
      pool,
      function(conn) {

        summary_data <- purrr::pluck(resp_lst, "summary_data")
        details_data <- purrr::pluck(resp_lst, "details_data")
        # report_req_json <- purrr::pluck(resp_lst, "report_req_json")
        # report_resp_json <- purrr::pluck(resp_lst, "report_resp_json")
        # queue_req_json <- purrr::pluck(resp_lst, "queue_req_json")
        # queue_resp_json <- purrr::pluck(resp_lst, "queue_resp_json")
        # report_params_json <- purrr::pluck(resp_lst, "report_params_json")
        # queue_id <- purrr::pluck(resp_lst, "queue_id")

        dbx::dbxUpsert(
          conn,
          DBI::SQL("entrata.pre_lease_summary_by_property"),
          records = summary_data,
          where_cols = c("report_date", "property_id"),
          skip_existing = FALSE
        )

        cli::cli_alert_success(
          c(
            "Successfully upserted {.field summary_data} to {.field entrata.pre_lease_summary_by_property}."
          )
        )

        dbx::dbxUpsert(
          conn,
          DBI::SQL("entrata.pre_lease_details_by_property"),
          records = details_data,
          where_cols = c("report_date", "property_id", "unit_type", "lease_id", "charge_code"),
          skip_existing = FALSE
        )

        cli::cli_alert_success(
          c(
            "Successfully upserted {.field details_data} to {.field entrata.pre_lease_details_by_property}."
          )
        )

        # if (
        #   !is.null(report_req_json) &&
        #   !is.null(report_resp_json) &&
        #   !is.null(queue_req_json) &&
        #   !is.null(queue_resp_json) &&
        #   !is.null(report_params_json) &&
        #   !is.null(queue_id)
        # ) {
        #
        #   queue_start <- jsonlite::fromJSON(queue_resp_json) |>
        #     purrr::pluck("response", "result", "queueStartedOn") |>
        #     lubridate::mdy_hms()
        #
        #   queue_end <- jsonlite::fromJSON(queue_resp_json) |>
        #     purrr::pluck("response", "result", "queueCompletedOn") |>
        #     lubridate::mdy_hms()
        #
        #   queue_duration <- as.integer(difftime(queue_end, queue_start, units = "secs"))
        #
        #   request_id <- jsonlite::fromJSON(report_req_json) |>
        #     purrr::pluck("request_id")
        #
        #   # minify json
        #   report_req_json <- jsonlite::toJSON(jsonlite::fromJSON(report_req_json), auto_unbox = TRUE)
        #   report_resp_json <- jsonlite::toJSON(jsonlite::fromJSON(report_resp_json), auto_unbox = TRUE)
        #   queue_req_json <- jsonlite::toJSON(jsonlite::fromJSON(queue_req_json), auto_unbox = TRUE)
        #   queue_resp_json <- jsonlite::toJSON(jsonlite::fromJSON(queue_resp_json), auto_unbox = TRUE)
        #   report_params_json <- jsonlite::toJSON(jsonlite::fromJSON(report_params_json), auto_unbox = TRUE)
        #
        #   logs_data_report <- tibble::tibble(
        #     log_date = Sys.Date(),
        #     entrata_endpoint = "/reports",
        #     entrata_operation = "getReportData",
        #     entrata_request_id = request_id,
        #     entrata_request_start = as.POSIXct(NA),
        #     entrata_request_end = as.POSIXct(NA),
        #     entrata_request_duration = as.integer(NA),
        #     entrata_request_json = as.character(report_req_json),
        #     entrata_response_json = as.character(report_resp_json),
        #     entrata_report_params_json = as.character(report_params_json),
        #     entrata_report_name = "pre_lease",
        #     entrata_report_queue_id = queue_id,
        #     log_comments = "Pre-Lease Report by Unit Type Report Request/Response"
        #   )
        #
        #   logs_data_queue <- tibble::tibble(
        #     log_date = Sys.Date(),
        #     entrata_endpoint = "/queue",
        #     entrata_operation = "getResponse",
        #     entrata_request_id = request_id,
        #     entrata_request_start = queue_start,
        #     entrata_request_end = queue_end,
        #     entrata_request_duration = queue_duration,
        #     entrata_request_json = as.character(queue_req_json),
        #     entrata_response_json = as.character(queue_resp_json),
        #     entrata_report_params_json = as.character(report_params_json),
        #     entrata_report_name = "pre_lease",
        #     entrata_report_queue_id = queue_id,
        #     log_comments = "Pre-Lease Report by Unit Type Queue Request/Response"
        #   )
        #
        #   logs_data <- dplyr::bind_rows(logs_data_report, logs_data_queue)
        #
        #   dbx::dbxInsert(
        #     conn,
        #     table = DBI::SQL("entrata.api_logs"),
        #     records = logs_data
        #   )
        #
        #   cli::cli_alert_success(
        #     c(
        #       "Successfully inserted API request/response logs into {.field entrata.api_logs}."
        #     )
        #   )
        # }
      }
    )
  }, error = function(e) {
    cli::cli_alert_danger(
      c(
        "Failed to upsert data into the database.\n",
        "Error: {.error {e$mesage}}"
      )
    )
  })

  return(invisible(NULL))

}


