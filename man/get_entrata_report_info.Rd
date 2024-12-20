% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrata_reports.R
\name{get_entrata_report_info}
\alias{get_entrata_report_info}
\title{Get Entrata Report Info}
\usage{
get_entrata_report_info(report_name, report_version = "latest")
}
\arguments{
\item{report_name}{A character string representing the name of the report.}

\item{report_version}{A character string representing the version of the report.
Defaults to "latest".}
}
\value{
A list containing report name, description, and filters.
}
\description{
This function retrieves detailed information for a specified
report from Entrata, including filters and descriptions.
}
