
#  ------------------------------------------------------------------------
#
# Title : Entrata API Client
#    By : Jimmy Briggs
#  Date : 2024-11-13
#
#  ------------------------------------------------------------------------

exponential_backoff <- function(num_requests) {
  backoff <- 2^num_requests
  jitter <- runif(1, 0, 1)
  backoff <- backoff + jitter
  return(min(backoff, 60))
}

retry_after <- function(resp) {
  time <- as.numeric(httr2::resp_headers(resp, "X-RateLimit-Reset"))
}

entrata_resp_parse_rate_limit_headers <- function(resp) {

  headers <- httr2::resp_headers(
    resp,
    filter = "x-ratelimit-limit|x-ratelimit-remaining|x-ratelimit-reset"
  )




}

entrata_api <- function(entrata_config) {

  base_url <- entrata_config$base_url
  username <- entrata_config$username
  password <- entrata_config$password

  user_agent <- entrata_config$user_agent %||% "gmhdatahub/0.0.1"

  retry_policy <- list(
    max_tries = 10,
    max_seconds = 60,
    retry_on_failure = TRUE,
    is_transient = function(resp) {
      status_transient <- httr2::resp_status(resp) %in% c(408, 429, 500:599)
      headers_transient <- "x-ratelimit-remaining" %in% tolower(names(httr2::resp_headers(resp))) &&
        tolower(httr2::resp_headers(resp))$`x-ratelimit-remaining` == "0"
      status_transient || headers_transient
    },
    backoff = exponential_backoff
  )

  req <- httr2::request(base_url) |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_retry(!!!retry_policy) |>
    httr2::req_throttle(rate = 25 / 60) |>
    httr2::req_error(is_error = entrata_resp_is_error, body = entrata_resp_body_error)

  structure(
    list(
      req = req,
      config = entrata_config
    ),
    class = c("entrata_request", "httr2_request")
  )

}

set_entrata_config <- function(api, ...) {
  api$config <- modifyList(api$config, list(...))
  api
}

entrata_error <- function(resp) {
  cond <- structure(
    list(
      message = paste("Entrata API Request failed: ", resp$status),
      resposne = resp
    ),
    class = c("entrata_error", "error", "condition")
  )
  stop(cond)
}

entrata_resp_error_body <- function(resp) {
  entrata_error(resp)
}

entrata_req_error <- function(req) {
  req |>
    httr2::req_error(is_error = entrata_resp_is_error, body = entrata_resp_error_body)
}

# entrata_req_retry <- function(req, max_tries = 10, backoff = )

library(ratelimitr)

entrata_rate_limited <- limit_rate(
  entrata_request,
  rate(n = 10, period = 60)
)

library(memoise)

cached_get_customers <- memoise(get_customers)

library(logger)

log_request <- function(req) {
  log_info("Making request to {req$url}")
}

req_before(\(req) log_request(req))

library(jsonvalidate)

validate_customer_response <- function(json) {
  schema <- '{
    "type": "object",
    "properties": {
      "id": {"type": "integer"},
      "name": {"type": "string"}
    },
    "required": ["id", "name"]
  }'

  json_validate(json, schema)
}

entrata_status <- function(api, ...) {
  api$req |>
    httr2::req_url_path_append("status") |>
    httr2::req_body_json(
      list(
        auth = list(
          type = "basic"
        ),
        method = list(
          name = "getStatus",
          version = "r1",
          params = list(NULL)
        )
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result")
}

# access-control-allow-origin: *
# access-control-expose-headers: Link, X-Total-Count, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset, Content-Language

entrata_req_retry <- function(req) {
  req |>
    httr2::req_retry(
      max_tries = 5,
      is_transient = function(resp) {
        rate_info <- entrata_resp_parse_rate_limit_headers(resp)
        httr2::resp_status(resp) == 429 ||
          all(sapply(rate_info$remaining, function(x) x == 0))
      },
      backoff = function(resp) {
        rate_info <- entrata_resp_parse_rate_limit_headers(resp)
        max(rate_info$reset$minute, rate_info$reset$hour, rate_info$reset$day, 1)
      }
    )
}

entrata_req_throttle <- function(req) {
  req |>
    httr2::req_throttle(rate = 25 / 60)  # 25 requests per minute
}

entrata_req_cache <- function(req) {
  req |>
    httr2::req_cache(path = "entrata_cache")
}
