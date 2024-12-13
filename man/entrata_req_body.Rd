% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrata_request.R
\name{entrata_req_body}
\alias{entrata_req_body}
\title{Set Request Body for Entrata API Request}
\usage{
entrata_req_body(
  req,
  id = NULL,
  method = NULL,
  version = "r1",
  params = list()
)
}
\arguments{
\item{req}{An httr2 request object}

\item{id}{Request ID}

\item{method}{The endpoint method name}

\item{version}{The endpoint method version}

\item{params}{List of parameters for the method}
}
\value{
Modified request object with the configured body
}
\description{
Configures the request body with specific values for the request.
}
