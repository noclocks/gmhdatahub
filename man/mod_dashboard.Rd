% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_dashboard.R
\name{mod_dashboard}
\alias{mod_dashboard}
\alias{mod_dashboard_ui}
\alias{mod_dashboard_server}
\alias{mod_dashboard_demo}
\title{Dashboard Shiny Module}
\usage{
mod_dashboard_ui(id)

mod_dashboard_server(id, pool = NULL)

mod_dashboard_demo()
}
\arguments{
\item{id}{Module's namespace ID.}

\item{pool}{Database connection pool.}
}
\value{
\itemize{
\item \code{mod_dashboard_ui()}: UI output
\item \code{mod_dashboard_server()}: List of reactive expressions.
\item \code{mod_dashboard_demo()}: \code{NULL}, runs the demo application.
}
}
\description{
A Shiny Module for the GMH Data Hub's Dashboard page.

Includes the following functions:
\itemize{
\item \code{mod_dashboard_ui()}: User Interface (UI) for the module.
\item \code{mod_dashboard_server()}: Server logic for the module.
\item \code{mod_dashboard_demo()}: Demo application for the module.
}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
mod_dashboard_demo()
\dontshow{\}) # examplesIf}
}
