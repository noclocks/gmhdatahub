.get_rate_limit_headers <- function(resp) {
  httr2::resp_headers(
    resp,
    filter = "x-ratelimit-limit|x-ratelimit-remaining|x-ratelimit-reset|expires"
  )
}

.parse_limits_remaining <- function(header) {
  limits <- strsplit(header, ";")[[1]]
  lapply(limits, function(limit) {
    parts <- strsplit(limit, "/")[[1]]
    list(value = as.integer(parts[1]), period = ifelse(length(parts) > 1, parts[2], NA))
  })
}

entrata_resp_parse_rate_limit_headers <- function(resp) {

  headers <- .get_rate_limit_headers(resp)

  limit <- .parse_limits_remaining(headers$`x-ratelimit-limit`)
  remaining <- .parse_limits_remaining(headers$`x-ratelimit-remaining`)

  reset <- as.integer(strsplit(headers$`x-ratelimit-reset`, ";")[[1]])

  expires <- if (!is.null(headers$expires)) {
    as.POSIXct(headers$expires, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  } else {
    NA
  }

  list(
    limit = list(
      day = limit[[1]]$value,
      hour = limit[[2]]$value,
      minute = limit[[3]]$value
    ),
    remaining = list(
      day = remaining[[1]]$value,
      hour = remaining[[2]]$value,
      minute = remaining[[3]]$value
    ),
    reset = list(
      day = reset[1],
      hour = reset[2],
      minute = reset[3]
    ),
    expires = expires
  )
}

# Assuming 'resp' is your httr2 response object
rate_limit_info <- entrata_resp_parse_rate_limit_headers(resp)

# You can then access the parsed information like this:
print(rate_limit_info$limit$day)  # Prints the daily limit
print(rate_limit_info$remaining$hour)  # Prints the remaining hourly limit
print(rate_limit_info$reset$minute)  # Prints the reset time for the minute limit
print(rate_limit_info$expires)  # Prints the expiration time

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

build_entrata_request <- function(base_url) {
  httr2::request(base_url) |>
    entrata_req_retry() |>
    entrata_req_throttle() |>
    entrata_req_cache() |>
    httr2::req_headers(
      Accept = "application/json",
      `User-Agent` = "Your App Name (your@email.com)"
    )
}

entrata_api_call <- function(endpoint, query = list()) {
  build_entrata_request("https://api.entrata.com") |>
    req_url_path(endpoint) |>
    req_url_query(!!!query) |>
    req_perform()
}

# Example usage
response <- entrata_api_call("/properties", list(limit = 10))

req_get_endpoint <- function(req) {
  check_request(req)
  req_url <- purrr::pluck(req, "url")
  gsub(paste0("^", "https://gmhcommunities.entrata.com/api/v1/"), "", req_url)
}

entrata_req_hash <- function(req) {
  endpoint <- req_get_endpoint(req)
  body <- req$body$data
  cache_key <- paste0(endpoint, digest::digest(body, algo = "md5"), sep = "_")
  return(cache_key)
}

library(digest)

generate_cache_key <- function(endpoint, body) {
  key <- paste(endpoint, digest::digest(body, algo = "md5"), sep = "_")
  return(key)
}

library(R6)

EntrataCache <- R6::R6Class(
  "EntrataCache",
  public = list(
    cache = list(),
    set = function(key, value, expires) {
      self$cache[[key]] <- list(
        value = value,
        expires = expires
      )
    },
    get = function(key) {
      entry <- self$cache[[key]]
      if (!is.null(entry) && Sys.time() < entry$expires) {
        return(entry$value)
      }
      return(NULL)
    },
    clear = function(key) {
      self$cache[[key]] <- NULL
    }
  )
)

entrata_cache <- EntrataCache$new()


entrata_api_call <- function(endpoint, body = list(), use_cache = TRUE) {

  cache_key <- generate_cache_key(endpoint, body)

  if (use_cache) {
    cached_response <- entrata_cache$get(cache_key)
    if (!is.null(cached_response)) {
      return(cached_response)
    }
  }

  response <- build_entrata_request("https://api.entrata.com") |>
    req_url_path(endpoint) |>
    req_body_json(body) |>
    req_method("POST") |>
    req_perform()

  rate_limit_info <- entrata_resp_parse_rate_limit_headers(response)

  if (use_cache && !is.null(rate_limit_info$cache_control)) {
    max_age <- as.numeric(sub(".*max-age=(\\d+).*", "\\1", rate_limit_info$cache_control))
    if (!is.na(max_age)) {
      expires <- Sys.time() + max_age
      entrata_cache$set(cache_key, response, expires)
    }
  }

  return(response)

}

response <- entrata_api_call("/properties", list(auth = list(type = "basic"), requestId = 15, method = list(version = "r1", name = "getProperties", params = list(NULL))))
