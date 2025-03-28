% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrata_request.R
\name{entrata_req_init}
\alias{entrata_req_init}
\title{Initialize Entrata API Request}
\usage{
entrata_req_init(
  base_url = NULL,
  username = NULL,
  password = NULL,
  user_agent = NULL,
  entrata_config = get_entrata_config()
)
}
\arguments{
\item{base_url}{The base URL of the Entrata API. If left \code{NULL}, will attempt
to retrieve from \code{get_entrata_config("base_url")}.}

\item{username}{The username for the Entrata API. If left \code{NULL}, will attempt
to retrieve from \code{get_entrata_config("username")}.}

\item{password}{The password for the Entrata API. If left \code{NULL}, will attempt
to retrieve from \code{get_entrata_config("password")}.}

\item{user_agent}{The user agent for the request. If left \code{NULL}, will attempt
to retrieve from \code{get_entrata_config("user_agent")}.}

\item{entrata_config}{Entrata API Configuration values as a list. The configuration
values should include the following keys: \code{username}, \code{password}, and \code{base_url}
at a minimum. If left \code{NULL}, will retrieve the configuration via the
\code{\link[=get_entrata_config]{get_entrata_config()}} function.}
}
\value{
\code{\link[httr2:request]{httr2::request()}} object
}
\description{
Initialize a request to the Entrata API. By default this function sets up the
request with the following configured settings:
\itemize{
\item Sets the base URL of the Entrata API.
\item Sets up basic authentication with the username and password.
\item Sets the content type and accept headers to \code{application/json}.
\item Sets the user agent.
\item Sets up error handling for Entrata API specific errors.
}
}
