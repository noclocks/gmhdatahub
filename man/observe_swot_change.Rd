% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_survey_insights_swot.R
\name{observe_swot_change}
\alias{observe_swot_change}
\title{Observe SWOT Change}
\usage{
observe_swot_change(input_field, field_name)
}
\arguments{
\item{input_field}{The input field to observe.}

\item{field_name}{The name of the field to log.}
}
\description{
Observe changes to a SWOT analysis field and log the change.
}
\examples{
if (interactive()) {
  # in a shiny app's server:
  observe_swot_change("strengths", "Strengths")
  observe_swot_change("weaknesses", "Weaknesses")
  observe_swot_change("opportunities", "Opportunities")
  observe_swot_change("threats", "Threats")
}
}
