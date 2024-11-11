
#  ------------------------------------------------------------------------
#
# Title : Metadata
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------

source("data-raw/src/properties.R")

# dictionary --------------------------------------------------------------

dictionary <- list()

# gmh_info ----------------------------------------------------------------

gmh_info <- list()

# noclocks_info -----------------------------------------------------------

noclocks_info <- list()

# entrata_info ------------------------------------------------------------

entrata_info <- list()

# app_info ----------------------------------------------------------------

app_info <- list()

# input_choices -------------------------------------------------------------

input_choices <- list(
  portfolios = c('GMH', '', 'Entrata'),
  properties = tibble::deframe(properties), # names = ids, values = property names

)


# app_defaults ------------------------------------------------------------

app_defaults <- list(
  picker_options = list(
    `actions-box` = TRUE,
    `live-search` = TRUE,
    `live-search-normalize` = TRUE,
    `live-search-placeholder` = 'Search...',
    `selected-text-format` = 'count > 3',
    `count-selected-text` = 'Properties Selected'
  )
)


# output ------------------------------------------------------------------

metadata <- list(
  dictionary = dictionary,
  gmh_info = gmh_info,
  noclocks_info = noclocks_info,
  entrata_info = entrata_info,
  app_info = app_info,
  app_choices = app_choices,
  app_defaults = app_defaults
)

usethis::use_data(metadata, internal = TRUE, overwrite = TRUE)
