require(httr2)
library(reticulate)

fs::dir_create(".venv")
reticulate::use_virtualenv(fs::path(getwd(), ".venv"))
reticulate::py_install(c("mitmproxy", "mitmproxy2swagger"))

mitmproxy <- reticulate::import("mitmproxy")
mitmdump <- reticulate::import("mitmproxy.tools.dump")
mitmproxy2swagger <- reticulate::import("mitmproxy2swagger")

# Function to start mitmdump
start_mitmdump <- function(outfile) {

  options <- list(
    listen_host = "127.0.0.1",
    listen_port = 8080,
    save_stream_file = outfile
  )

  master <- mitmdump$DumpMaster(options)
  master$run()
}

# Function to convert mitmdump output to OpenAPI spec
mitmdump_to_swagger <- function(input_file, output_file, api_prefix) {
  # mitmproxy2swagger -i <input_path> -o <output_path> -p <api_prefix>
  mitmproxy2swagger$mitmproxy2swagger(
    "-i", input_file,
    "-o", output_file,
    "-p", api_prefix
  )
}

# Function to set up a proxy for httr2 requests
# setup_mitmproxy <- function(host = "127.0.0.1", port = 8080, config) {
#
# }

Sys.setenv(CURL_CA_BUNDLE = "/path/to/mitmproxy-ca-cert.pem")

proxy_url <- "http://localhost:8080"

entrata_config <- config::get("entrata")

base_req <- httr2::request(entrata_config$base_url) |>
  httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
  httr2::req_method("POST") |>
  httr2::req_headers(`Content-Type` = "application/json") |>
  httr2::req_user_agent("gmhdatahub/0.1.0 (https://github.com/noclocks/gmhdatahub)") |>
  httr2::req_verbose(
    header_req = TRUE,
    header_resp = TRUE,
    body_req = TRUE,
    body_resp = TRUE,
    info = TRUE,
    redact_headers = TRUE
  ) |>
  httr2::req_progress() |>
  httr2::req_body_json(
    list(
      auth = list(
        type = "basic"
      ),
      requestId = 15,
      method = list(
        name = NULL,
        version = "r1",
        params = list(NULL)
      )
    )
  ) |>
  httr2::req_proxy(
    url = "127.0.0.1",
    port = 8080,
    username = config$username,
    password = config$password,
    auth = "basic"
  )

entrata_req_proxy_perform <- function(endpoint, body) {

  req <- base_req |> httr2::req_url_path_append(endpoint) |>
    httr2::req_body_json(body)

  entrata_req_log(req)
  req |> httr2::req_perform()
  entrata_resp_log(resp)
  resp
}

entrata_req_log <- function(req) {

  logger::log_info("Request URL: {req$url}")
  logger::log_info("Request Method: {req$method}")
  logger::log_info("Request Headers: {req$headers}")
  logger::log_info("Request Body: {req$body}")

}

entrata_resp_log <- function(resp) {

  logger::log_info("Response Status: {resp$status_code}")
  logger::log_info("Response Headers: {resp$headers}")
  logger::log_info("Response Body:\n{httr2::resp_body_string(resp)}")

}



out_file <- "data-raw/entrata_captured_traffic.flow"
future::future(start_mitmdump(out_file))

req_status <-  httr2::request(entrata_config$base_url) |>
  httr2::req_url_path_append("status") |>
  httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
  httr2::req_method("POST") |>
  httr2::req_headers(`Content-Type` = "application/json") |>
  httr2::req_user_agent("gmhdatahub/0.1.0 (https://github.com/noclocks/gmhdatahub)") |>
  httr2::req_verbose(
    header_req = TRUE,
    header_resp = TRUE,
    body_req = TRUE,
    body_resp = TRUE,
    info = TRUE,
    redact_headers = TRUE
  ) |>
  httr2::req_progress() |>
  httr2::req_body_json(
    list(
      auth = list(
        type = "basic"
      ),
      requestId = 15,
      method = list(
        name = "getStatus",
        version = "r1",
        params = list(NULL)
      )
    )
  ) |>
  httr2::req_proxy(
    url = "127.0.0.1",
    port = 8080,
    username = entrata_config$username,
    password = entrata_config$password,
    auth = "basic"
  )

req

resp_status <- req_status |> httr2::req_perform()

mitmdump_to_swagger(
  input_file = out_file,
  output_file = "data-raw/entrata_apispec.yml",
  api_prefix = entrata_config$base_url
)


req_properties <-  httr2::request(entrata_config$base_url) |>
  httr2::req_url_path_append("properties") |>
  httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
  httr2::req_method("POST") |>
  httr2::req_headers(`Content-Type` = "application/json") |>
  httr2::req_user_agent("gmhdatahub/0.1.0 (https://github.com/noclocks/gmhdatahub)") |>
  httr2::req_verbose(
    header_req = TRUE,
    header_resp = TRUE,
    body_req = TRUE,
    body_resp = TRUE,
    info = TRUE,
    redact_headers = TRUE
  ) |>
  httr2::req_progress() |>
  httr2::req_body_json(
    list(
      auth = list(
        type = "basic"
      ),
      requestId = 15,
      method = list(
        name = "getProperties",
        version = "r1",
        params = list(
          propertyId = ""
        )
      )
    )
  ) |>
  httr2::req_proxy(
    url = "http://127.0.0.1",
    port = 8080,
    username = entrata_config$username,
    password = entrata_config$password,
    auth = "basic"
  )

req

resp_properties <- req_properties |> httr2::req_perform()
resp_properties_content <- resp_properties |> httr2::resp_body_json()
resp_properties_data <- purrr::pluck(
  resp_properties_content, "response", "result", "PhysicalProperty", "Property"
) |>
  parse_properties_response()

# queue
req_queue |> httr2::req_proxy(
  url = "http://127.0.0.1",
  port = 8080,
  username = entrata_config$username,
  password = entrata_config$password,
  auth = "basic"
) |>
  httr2::req_perform()

# reports
req |>  httr2::req_proxy(
  url = "http://127.0.0.1",
  port = 8080,
  username = entrata_config$username,
  password = entrata_config$password,
  auth = "basic"
) |>
  httr2::req_perform()

# arcodes
httr2::request(entrata_config$base_url) |>
  httr2::req_url_path_append("arcodes") |>
  httr2::req_auth_basic(entrata_config$username, entrata_config$password) |>
  httr2::req_method("POST") |>
  httr2::req_headers(`Content-Type` = "application/json") |>
  httr2::req_proxy(
    url = "http://127.0.0.1",
    port = 8080,
    username = entrata_config$username,
    password = entrata_config$password,
    auth = "basic"
  ) |>
  httr2::req_perform()
