#  ------------------------------------------------------------------------
#
# Title : Shiny App Healthcheck
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

app_healthcheck <- function(req) {

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")

  parsed_request <- parse_request(req)

  http_method <- parsed_request$method
  path_info <- parsed_request$path_info
  protocol <- parsed_request$protocol

  entrata_status <- tryCatch(
    entrata_healthcheck(),
    error = function(e) list(status = "error", message = conditionMessage(e))
  )

  entrata_status_code <- entrata_status$response$code

  db_status <- tryCatch(
    db_healthcheck(),
    error = function(e) list(status = "error", message = conditionMessage(e))
  )

  global_status <- if (entrata_status_code == 200L && db_status$status == "ok") {
    "HEALTHY"
  } else {
    "UNHEALTHY"
  }

  resp_data <- list(
    status = global_status,
    timestamp = now,
    request = list(
      method = http_method,
      path = path_info,
      protocol = protocol
    ),
    services = list(
      entrata = entrata_status,
      database = db_status
    )
  )

  status <- if (global_status == "HEALTHY") 200L else 503L

  resp_data_json <- jsonlite::toJSON(resp_data, auto_unbox = TRUE, pretty = TRUE)

  shiny::httpResponse(
    status = status,
    content_type = "application/json",
    content = resp_data_json,
    headers = list("Cache-Control" = "no-cache", "Pragma" = "no-cache")
  )

}

parse_req <- function(req) {
  force(req)
  http_method <- req$REQUEST_METHOD
  path_info <- req$PATH_INFO
  protocol <- req$HTTP_X_FORWARDED_PROTO
  return(
    list(
      http_method = http_method,
      path_info = path_info,
      protocol = protocol
    )
  )
}

add_healthcheck <- function(app) {
  app$ui <- function(req) {
    force(req)
    if (req$PATH_INFO == "/health") {
      return(
        app_healthcheck(req)
      )
    }
    app$ui(req)
  }
  return(app)
}

healthcheck_ui <- function(ui) {
  initial_ui <- ui
  app_ui <- function(req) {
    force(req)
    if (req$PATH_INFO == "/health") {
      return(
        app_healthcheck(req)
      )
    }
    initial_ui(req)
  }
}

entrata_healthcheck <- function(entrata_config = NULL) {

  if (is.null(entrata_config)) {
    entrata_config <- config::get(value = "entrata")
  }

  req <- httr2::request(entrata_config$base_url) |>
    httr2::req_url_path_append("status") |>
    httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15L,
        method = list(
          name = "getStatus",
          version = "r1"
        )
      )
    )

  resp <- httr2::req_perform(req)

  resp |>
    httr2::resp_body_json()

}

db_healthcheck <- function(db_config = NULL) {

  if (is.null(db_config)) {
    db_config <- config::get(value = "db")
  }

  conn <- db_connect()
  on.exit(pool::poolClose(conn))

  tryCatch(
    {
      pool::dbExecute(conn, "SELECT 1;")
      cli::cli_alert_info("Database connection successful.")
      return(
        list(
          status = "ok",
          db = db_config$dbname
        )
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Failed to connect to the database.",
          "Details: {conditionMessage(e)}"
        )
      )
      return(
        list(
          status = "error",
          db = db_config$dbname
        )
      )
    }
  )

}
