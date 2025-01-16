
#  ------------------------------------------------------------------------
#
# Title : httptest2 Redaction
#    By : Jimmy Briggs
#  Date : 2024-11-14
#
#  ------------------------------------------------------------------------

# This file is used in conjunction with the httptest2 package to redact any
# sensitive information from the response object and replace it with a mock
# response. This is useful for testing and debugging purposes.

function(resp) {

  # headers -----------------------------------------------------------------
  resp <- httptest2::redact_headers(
    response = resp,
    headers = c(

      # General
      "Date",
      "Alt-Svc",

      # Cloudflare
      "CF-Cache-Status",
      "CF-RAY",

      # Cookies
      "Set-Cookie",

      # Server
      "Server",

      # Security
      "Strict-Transport-Security",
      "Vary",
      "Access-Control-Allow-Origin",
      "Access-Control-Expose-Headers"

    )
  )

  # Cookies -----------------------------------------------------------------

  resp <- httptest2::redact_cookies(response = resp)

  # URLs --------------------------------------------------------------------

  resp_url <- purrr::pluck(resp, "url")
  resp_endpoint <- resp_url |> basename()
  resp_req_id <- purrr::pluck(resp, "request", "body", "data", "requestId")
  resp_endpoint_method <- purrr::pluck(resp, "request", "body", "data", "method", "name")

  mock_dir <- glue::glue("mocks/gmhcommunities.entrata.com/{resp_endpoint}/")
  mock_file <- glue::glue("{resp_endpoint_method}-{resp_req_id}.json")

  # GMH Entrata
  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://gmhcommunities.entrata.com/api/v1/",
    replacement = mock_dir
  )

  # noclocks auth
  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://auth-api-dev.noclocks.co/v1/",
    replacement = "mocks/auth-api-dev.noclocks.co/"
  )

  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://gmh-noclocks-auth.noclocks.co/v1/",
    replacement = "mocks/gmh-noclocks-auth.noclocks.co/"
  )

  # gmh hosted auth
  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://auth-api-dev.gmhcommunities.com/v1/",
    replacement = "mocks/auth-api-dev.gmhcommunities.com/"
  )

  # resend
  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://api.resend.com/",
    replacement = "mocks/api.resend.com/"
  )

  # httpbin
  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://httpbin.org/",
    replacement = "mocks/httpbin.org/"
  )

  return(resp)

}
