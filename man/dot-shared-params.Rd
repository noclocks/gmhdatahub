% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R
\name{.shared-params}
\alias{.shared-params}
\title{Shared Parameters}
\arguments{
\item{as_of_date}{Date to use as the reference point for deriving other dates from.
Defaults to the current system date.}

\item{conn}{A database connection object.}

\item{pool}{A database connection pool object.}

\item{key}{Character string representing the configuration key to retrieve.
Defaults to \code{NULL} which returns the full configuration list.}

\item{file}{A character string representing the path to the configuration file.}

\item{config}{A character string representing the configuration to use from the
configuration file.}

\item{cfg}{List containing the configuration values or a string to the path
of a configuration file. If \code{NULL}, the default configuration file path will be used.}

\item{entrata_config}{Entrata API Configuration values as a list. The configuration
values should include the following keys: \code{username}, \code{password}, and \code{base_url}
at a minimum. If left \code{NULL}, will retrieve the configuration via the
\code{\link[=get_entrata_config]{get_entrata_config()}} function.}

\item{db_config}{Database configuration values as a list. The configuration
values should include the following keys: \code{host}, \code{port}, \code{dbname}, \code{user},
and \code{password} at a minimum. If left \code{NULL}, will retrieve the configuration
via the \code{\link[=get_db_config]{get_db_config()}} function.}

\item{gmaps_config}{Google Maps API configuration values as a list. The configuration
values should include the following keys: \code{api_key} at a minimum. If left \code{NULL},
will retrieve the configuration via the \code{\link[=get_gmaps_config]{get_gmaps_config()}} function.}

\item{request_id}{The unique identifier for the request.
This is used to track the request and response.  If left \code{NULL}, a
unique identifier will be generated via \code{as.integer(Sys.time())}.}

\item{endpoint}{The Entrata API endpoint to send the request to.
This must be a valid Entrata API endpoint path. If left \code{NULL},
no endpoint will be appended to the request's URL. See
details for how to get the available Entrata API endpoints.}

\item{method_name}{The name of the Entrata API "method" (operation) to use
in the request. Not to be confused with the \code{HTTP} request method
(i.e. \code{GET}, \code{POST}, etc.), this method must be a method available for
the specified \code{endpoint} argument, making it conditional on the supplied
\code{endpoint}. If left \code{NULL}, a pre-defined default method will be used
depending on the supplied \code{endpoint}. For example, the default method
for the \verb{/properties} endpoint is \code{getProperties}. See details for how
to get the available Entrata API methods by their endpoint.}

\item{method_version}{The version of the Entrata API method to use in the
request. This is conditional on the supplied \code{method_name} argument
and must be a valid version for the given method name. If left \code{NULL},
a pre-defined default version will be used depending on the supplied
\code{method_name}. Note that all Entrata API method versions are one of
\code{"r1"}, \code{"r2"}, or \code{"r3"}.}

\item{method_params}{A named, potentially nested list of request method parameters
to pass to the Entrata API. These parameters will be added to the request
body's \code{method.params} object. Depending on the supplied \code{endpoint},
\code{method_name}, and \code{method_version}, the required parameters will vary
and some will have required, optional, or default values. If left \code{NULL},
any required method parameters without default values will throw an error.}
}
\description{
These parameters are used in multiple functions throughout the package.
They are defined here to make it easier to update them in one place.

To use these parameters in a function, simply include the following
in the function's documentation:

\if{html}{\out{<div class="sourceCode">}}\preformatted{#' @inheritParams .shared-params
}\if{html}{\out{</div>}}
}
\keyword{internal}
