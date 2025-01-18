
#  ------------------------------------------------------------------------
#
# Title : Shiny App Healthcheck
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

app_healthcheck <- function() {
  shiny::httpResponse(
    status = 200L,
    content_type = "application/json",
    content = jsonlite::toJSON(list("status" = "OK"), auto_unbox = TRUE, pretty = TRUE),
    headers = list("Cache-Control" = "no-cache")
  )
}

healthcheck_handler <- function(req) {
  force(req)
  ts <- Sys.time()
  status_ok <- TRUE
  req_parsed <- parse_req(req)

  if (req_parsed$http_method == "GET" && req_parsed$path_info == "/health") {
    return(
      shiny::httpResponse(
        status = 200L,
        content_type = "application/json",
        content = jsonlite::toJSON(list(status = "ok")),
        headers = list("Cache-Control" = "no-cache")
      )
    )
  }
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

with_healthcheck <- function(app) {

  app$ui <- function(req) {
    force(req)
    if (req$PATH_INFO == "/health") {
      return(healthcheck_handler(req))
    }
    app$ui(req)
  }

  return(app)

}
