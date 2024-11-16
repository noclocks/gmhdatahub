
#  ------------------------------------------------------------------------
#
# Title : httptest2 Redaction
#    By : Jimmy Briggs
#  Date : 2024-11-14
#
#  ------------------------------------------------------------------------

function(resp) {

  # mocked responses path ---------------------------------------------------
  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://gmhcommunities.entrata.com/api/v1/",
    replacement = "mocks/gmhcommunities.entrata.com/"
  )

  resp <- httptest2::gsub_response(
    resp,
    pattern = "https://httpbin.org/",
    replacement = "mocks/httpbin.org/"
  )





}
