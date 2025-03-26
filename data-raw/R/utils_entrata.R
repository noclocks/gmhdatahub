
source("data-raw/R/utils_caching.R")

get_entrata_config <- function() {
  config::get("entrata")
}

entrata_base_request <- function() {
  # get config
  entrata_config <- get_entrata_config()

  # base request
  httr2::request(entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(
      entrata_config$username,
      entrata_config$password
    ) |>
    httr2::req_headers(
      `Content-Type` = "application/json; charset=utf-8",
      `Accept` = "application/json"
    )
}

entrata_properties_request <- function() {
  # base request
  entrata_base_request() |>
    httr2::req_url_path_append("properties") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getProperties",
          version = "r1",
          params = list(NULL)
        )
      )
    )
}

entrata_arcodes_request <- function() {
  entrata_base_request() |>
    httr2::req_url_path_append("arcodes") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getArCodes",
          version = "r1",
          params = list()
        )
      )
    )
}

entrata_properties_picklist_request <- function(property_ids = NULL) {
  if (is.null(property_ids)) {
    property_ids <- get_entrata_properties()$id
  }

  entrata_base_request() |>
    httr2::req_url_path_append("properties") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getPropertyPickLists",
          version = "r2",
          params = list(
            propertyId = stringr::str_c(property_ids, collapse = ",")
          )
        )
      )
    )
}

entrata_leases_picklist_request <- function() {
  entrata_base_request() |>
    httr2::req_url_path_append("leases") |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        request_id = 15L,
        method = list(
          name = "getLeasePickList",
          version = "r1",
          params = list()
        )
      )
    )
}


entrata_req_perform <- function(req) {
  req |> httr2::req_perform()
}

entrata_req_perform_save <- function(req, path = "data-raw/cache/entrata/responses", force = FALSE) {

  fs::dir_create(path)
  request_dir <- fs::path(dirname(path), "requests")
  fs::dir_create(request_dir)

  endpoint <- basename(req$url)
  method_name <- req$body$data$method$name
  method_version <- req$body$data$method$version
  file_name <- glue::glue("{endpoint}.{method_name}.{method_version}")
  response_file <- fs::path(path, paste0(file_name, ".response.json"))
  response_file_qs <- fs::path(path, paste0(file_name, ".response.qs"))
  request_file <- fs::path(request_dir, paste0(file_name, ".request.json"))

  req_json <- jsonlite::toJSON(req$body$data, auto_unbox = TRUE, pretty = TRUE)
  perform <- force || TRUE

  if (!force && fs::file_exists(request_file)) {
    existing_req <- jsonlite::read_json(request_file)
    if (identical(existing_req, req$body$data)) {
      perform <- FALSE
    }
  }

  if (perform) {
    jsonlite::write_json(req$body$data, request_file, auto_unbox = TRUE, pretty = TRUE)
  }

  if (perform || !fs::file_exists(response_file_qs)) {
    tryCatch({
      logger::log_info("Performing request for {endpoint}.{method_name}.{method_version}")
      resp <- req |> httr2::req_perform(path = response_file)
      qs2::qs_save(resp, response_file_qs)
      logger::log_info("Response saved to file: {response_file}")
      return(resp)
    },
    error = function(e) {
      logger::log_error("Error performing request: {e$message}")
      stop(e)
    })
  } else {
    tryCatch({
      logger::log_info("Reading cached response for {endpoint}.{method_name}.{method_version}")
      resp <- qs2::qs_read(response_file_qs)
      logger::log_info("Response read from file: {response_file}")
      return(resp)
    },
    error = function(e) {
      logger::log_error("Error reading cached response: {e$message}")
      stop(e)
    })
  }
}

entrata_req_perform_save <- memoise::memoise(
  entrata_req_perform_save,
  cache = setup_cache("entrata", "memoised")
)

assign_versions <- function(methods) {
  purrr::map(methods, ~ if (.x %in% r3_methods) {
    "r3"
  } else if (.x %in% r2_methods) {
    "r2"
  } else {
    "r1"
  })
}



