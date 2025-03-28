% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrata_request.R
\name{entrata_req_auth}
\alias{entrata_req_auth}
\title{Set Basic Authentication for Entrata Request}
\usage{
entrata_req_auth(req, username = NULL, password = NULL)
}
\arguments{
\item{req}{An httr2 request object}

\item{username}{Username for authentication. If left \code{NULL}, will attempt
to retrieve systematically.}

\item{password}{Password for authentication. If left \code{NULL}, will attempt
to retrieve systematically.}
}
\value{
The modified request object with authentication headers.
}
\description{
Adds basic authentication (username and password) to the Entrata request.
}
