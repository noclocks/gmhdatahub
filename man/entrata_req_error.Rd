% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrata_request.R
\name{entrata_req_error}
\alias{entrata_req_error}
\title{Set Error Handling for Entrata Request}
\usage{
entrata_req_error(
  req,
  is_error = entrata_resp_is_error,
  error_body = entrata_resp_body
)
}
\arguments{
\item{req}{An httr2 request object}

\item{is_error}{Function to check if response contains an error}

\item{error_body}{Function to parse error body in case of an error}
}
\value{
Modified request with error handling
}
\description{
Configures error handling for the request, using custom error functions.
}
