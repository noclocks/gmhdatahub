
#  ------------------------------------------------------------------------
#
# Title : Metadata
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------

metadata <- list(
  labels = list(
    title = "GMH Data Hub",
    subtitle = "Data Analysis and Reporting"
  ),
  validation = list(
    required_fields = c("properties", "reports"),
    numeric_ranges = list(
      age = c(0, 120),
      weight = c(0, 1000),
      score = c(0, 100)
    )
  ),
  defaults = list(
    page_size = 10,
    chart_height = 400,
    table_height = 400
  )
)

app_metadata <- list(
  # App configuration
  config = list(
    app_name = "My Shiny App",
    version = "1.0.0",
    description = "App description",
    authors = c("Author 1", "Author 2")
  ),

  # UI elements metadata
  ui_elements = list(
    labels = list(
      title = "App Title",
      subtitle = "Subtitle"
    ),
    input_choices = list(
      colors = c("Red", "Blue", "Green"),
      sizes = c("Small", "Medium", "Large")
    )
  ),

  # Validation rules
  validation = list(
    required_fields = c("name", "email"),
    numeric_ranges = list(
      age = c(0, 120),
      score = c(0, 100)
    )
  )
)

source("data-raw/src/properties.R")

# dictionary --------------------------------------------------------------

dictionary <- list()

# gmh_info ----------------------------------------------------------------

client_info <- list(
  name = "GMH Communities",
  url = "https://gmhcommunities.com",
  logo = "www/img/logos/gmh-logo.svg",
  symbol = "www/img/icons/gmh-icon.png"
)

# noclocks_info -----------------------------------------------------------

developer_info <- list(
  name = "No Clocks, LLC",
  url = "https://noclocks.dev",
  logo = "www/img/logos/noclocks-logo.svg",
  symbol = "www/img/icons/noclocks-icon-circular.png"
)

# entrata_info ------------------------------------------------------------

entrata_info <- list(
  name = "Entrata",
  url = "https://gmhcommunities.entrata.com/api/v1/documentation",
  logo = "www/img/logos/entrata-logo.png",
  symbol = NULL
)

# app_info ----------------------------------------------------------------

app_info <- list(
  name = "GMH Data Hub",
  version = "1.0",
  logo = "www/img/logos/app-logo.svg",
  symbol = "www/img/icons/app-icon.webp",
  repo_url = "https://github.com/noclocks/gmhdatahub",
  docs_url = "https://docs.noclocks.dev/gmhdatahub"
)

# input_choices -------------------------------------------------------------

app_choices <- list(
  portfolios = c(),
  properties = c() #tibble::deframe(properties) # names = ids, values = property names
)


# app_defaults ------------------------------------------------------------

app_defaults <- list(
  picker_options = list(
    "actions-box" = TRUE,
    "live-search" = TRUE,
    "live-search-normalize" = TRUE,
    "live-search-placeholder" = "Search...",
    "selected-text-format" = "count > 3",
    "count-selected-text" = "Properties Selected"
  )
)


# output ------------------------------------------------------------------

metadata <- list(
  # dictionary = dictionary,
  client_info = client_info,
  developer_info = developer_info,
  entrata_info = entrata_info,
  app_info = app_info,
  app_choices = app_choices,
  app_defaults = app_defaults
)

# fs::file_delete("R/sysdata.rda")
# save(metadata, file = "R/sysdata.rda")
usethis::use_data(
  client_info,
  developer_info,
  entrata_info,
  app_info,
  app_choices,
  app_defaults,
  internal = TRUE,
  overwrite = TRUE,
  compress = "bzip2",
  version = 3,
  ascii = FALSE
)

