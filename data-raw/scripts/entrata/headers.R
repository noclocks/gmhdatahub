
#  ------------------------------------------------------------------------
#
# Title : Entrata Headers
#    By : Jimmy Briggs
#  Date : 2024-11-18
#
#  ------------------------------------------------------------------------


# request -----------------------------------------------------------------

entrata_request_headers <- list(
  "Content-Type" = "APPLICATION/JSON; CHARSET=UTF-8",
  "Accept" = "application/json",
  "Authorization" = "Basic REDACTED"
)


# response ----------------------------------------------------------------

entrata_response_headers <- list(
  "Access-Control-Allow-Origin" = "*",
  "Access-Control-Expose-Headers" = "Link, X-Total-Count, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset, Content-Language",
  "Alt-Svc" = "H3=\":443\"; MA=86400",
  "CF-Cache-Status" = "DYNAMIC",
  "CF-Ray" = "8e4ad26f9d87bca7-ATL",
  "Content-Encoding" = "BR",
  "Content-Language" = "EN-US",
  "Content-Type" = "APPLICATION/JSON; CHARSET=UTF-8",
  "Date" = "Mon, 18 Nov 2024 20:48:33 GMT",
  "Server" = "CLOUDFLARE",
  "Vary" = "Authorization",
  "X-RateLimit-Limit" = "15000/DAY;1000/HOUR;150/MINUTE",
  "X-RateLimit-Remaining" = "14996/DAY;999/HOUR;149/MINUTE",
  "X-RateLimit-Reset" = "36686;686;26",
  "X-Read-Only" = "000"
)
