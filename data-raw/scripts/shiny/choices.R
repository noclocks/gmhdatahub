if (!exists("entrata_properties_lst")) source("data-raw/scripts/entrata/properties.R")
if (!exists("gmh_portfolios_lst")) source("data-raw/scripts/gmh/portfolios.R")
if (!exists("gmh_competitors_lst")) source("data-raw/scripts/gmh/competitors.R")
if (!exists("gmh_partners_lst")) source("data-raw/scripts/gmh/partners.R")
if (!exists("gmh_segments_lst")) source("data-raw/scripts/gmh/segments.R")

app_choices_lst <- list(
  "properties" = entrata_properties_lst,
  "portfolios" = gmh_portfolios_lst,
  "competitors" = gmh_competitors_lst,
  "partners" = gmh_partners_lst,
  "segments" = gmh_segments_lst
)
