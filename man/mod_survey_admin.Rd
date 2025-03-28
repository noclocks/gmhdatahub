% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_survey_admin.R
\name{mod_survey_admin}
\alias{mod_survey_admin}
\alias{mod_survey_admin_ui}
\alias{mod_survey_admin_server}
\alias{mod_survey_admin_demo}
\title{Survey Admin Shiny Module}
\usage{
mod_survey_admin_ui(id)

mod_survey_admin_server(
  id,
  pool = NULL,
  global_filters = NULL,
  navigate = NULL
)

mod_survey_admin_demo()
}
\arguments{
\item{id}{Module's namespace ID.}

\item{pool}{Database connection pool.}
}
\value{
\itemize{
\item \code{mod_survey_admin_ui()}: UI output
\item \code{mod_survey_admin_server()}: List of reactive expressions.
\item \code{mod_survey_admin_demo()}: \code{NULL}, runs the demo application.
}
}
\description{
A Shiny Module for the GMH Data Hub's Survey Admin page.

Includes the following functions:
\itemize{
\item \code{mod_survey_admin_ui()}: User Interface (UI) for the module.
\item \code{mod_survey_admin_server()}: Server logic for the module.
\item \code{mod_survey_admin_demo()}: Demo application for the module.
}
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
mod_survey_admin_demo()
\dontshow{\}) # examplesIf}
}
