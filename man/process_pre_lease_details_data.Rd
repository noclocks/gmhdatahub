% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entrata_pipeline.R
\name{process_pre_lease_details_data}
\alias{process_pre_lease_details_data}
\title{Process Pre-Lease Details Data}
\usage{
process_pre_lease_details_data(details_data, report_date = Sys.Date())
}
\arguments{
\item{details_data}{The original Pre-Lease Details Data.}

\item{report_date}{The report date. Default is the current system date.}
}
\value{
A tibble containing the processed Pre-Lease Details Data.
}
\description{
This function processes the Pre-Lease Details Data returned from the \code{pre_lease} Entrata API report details
dataset. It performs data type conversion, column renaming, and reordering.
}
