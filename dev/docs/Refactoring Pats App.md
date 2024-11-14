# Refactor `gmhLeasingDashboard` App

>   [!NOTE]
>
>   *In order to create a more developer-friendly, modular, and maintainable codebase for the **home module** in the `gmhLeasingDashboard` Shiny app, this document will focus on organizing and re-structuring the summary report data pipeline, encapsulating technical operations (database handling, authentication, etc.), and structuring the UI logic. Below is a comprehensive plan to achieve this:*

## Contents

[TOC]

## Separate Code into Functional Layers

-   **Data Access Layer (DAL)**: Handles Entrata API interactions, including request setup, retry logic, and error handling.
-   **Service Layer**: Manages the data pipeline logic (transformations, calculations) and data enrichment (e.g., adding manual data like "Model Beds").
-   **UI Layer**: Implements the Shiny UI and server logic, where the data is displayed and interacted with by the user.

This layering approach allows each part to be developed and tested independently, making it easier for developers to understand specific functionalities without needing to dive into the entire codebase.

## Data Access Layer (DAL)

Separate all Entrata API and database interaction functions into a dedicated R script (`R/data_access.R`). This layer will handle all network requests, authentication, retry logic, and error handling, abstracting these details away from the main application logic.

This file will provide a suite of DAL functions representing each necessary step along the pipeline to get to the final summary table. See [Data Pipeline for the Leasing Summary Table](./Data Pipeline for the Leasing Summary Table.md) for details.

Also this document includes an [Appendix](#appendix) to review

## Service Layer for Data Pipeline Logic

Create a service layer to encapsulate the data processing logic in `R/data_service.R`. This layer will handle all data transformations, enrichments, and business rules.

## Home Module Interface

Use `Shiny modules` to encapsulate the home module logic. This allows you to manage the module in a self-contained structure with input/output bindings.

For example,

```R
# In `R/mod_home.R`

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Home Module"),
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("property"), "Select Property", choices = NULL),
          dateRangeInput(ns("daterange"), "Date Range"),
          actionButton(ns("refresh"), "Refresh Data", icon = icon("refresh"))
        ),
        mainPanel(
          DTOutput(ns("summary_table"))
        )
      )
    )
  )
}
```

## Home Module Server

In the server function of `mod_home_server`, integrate calls to the DAL and service functions to create a cohesive data pipeline, enabling data retrieval and processing.

```R
mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Values
    entrata_config <- reactive(load_entrata_config())
    
    properties_data <- reactive({
      req(input$refresh)
      get_entrata_properties(entrata_config())
    })
    
    # Data pipeline using the Service Layer
    summary_data <- reactive({
      properties_data <- properties_data()
      weekly_data <- request_weekly_prelease_data(entrata_config())
      processed_data <- process_entrata_data(properties_data, weekly_data)
      return(processed_data)
    })
    
    # Render the Summary Table
    output$summary_table <- renderDT({
      datatable(
        summary_data(),
        options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
      )
    })
  })
}
```

## Connect Module to Main App

Now you can add the home module to the main UI and server logic of your app.

```R
ui <- fluidPage(
  mod_home_ui("home_module")
)

# In `app_server.R`
server <- function(input, output, session) {
  mod_home_server("home_module")
}
```

## Documentation & Developer Experience

1.  **Code Comments and Documentation**: Use `roxygen2` comments for each function to document its purpose, parameters, and return values. This will make it easier for future developers to understand each part of the codebase.

2.  **Modular Tests**: Use `testthat` to add unit tests for each function in `data_access.R` and `data_service.R`, ensuring the data pipeline is robust and maintains functionality over time.

3.  **Developer Notes**: Include a `README` file and in-line comments to explain the general structure of the module, including the purpose of each layer and where to modify specific parts of the pipeline (e.g., updating API authentication or adding a new data transformation).

This modular approach not only clarifies the structure but also makes it easy to test, extend, and document, resulting in a codebase that's maintainable, scalable, and developer-friendly.

## Appendix

## Data Access Layer Functions

Below are functions for the DAL pipeline:

### Load Entrata Configuration

The `load_entrata_config()` function reads configuration details from `config/config.yml` using `config::get()`. This function makes it easy to load configurations across different functions.

`load_entrata_config`:

```R
# In `R/data_access.R`

# config ------------------------------------------------------------------

load_entrata_config <- function(path = pkg_sys("config/config.yml")) {
  config::get("entrata", file = path)
}

# Add more API functions as needed (e.g., request_leasing_report, request_weekly_prelease_data, etc.)
```

### Get Entrata Properties

The function `get_entrata_properties()` correctly retrieves property data, maps the response into a data frame, and pulls essential fields like `PropertyID`, `MarketingName`, and `Type`. Ensure that the configuration passed matches the Entrata base URL and authentication requirements.

`get_entrata_properties`:

```R
# get initial properties --------------------------------------------------

get_entrata_properties <- function(entrata_config = load_entrata_config()) {
  httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("properties") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getProperties",
          version = "r1",
          params = list(NULL)
        )
      )
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::pluck("response", "result", "PhysicalProperty", "Property") |>
    purrr::map_dfr(
      function(x) {
        tibble::tibble(
          property_id = x$PropertyID,
          property_name = x$MarketingName,
          property_type = x$Type
        )
      }
    )
}
```

### Request Entrata Leasing Report Data

`request_leasing_report()` function performs the request to `/reports` `getReportData` to retrieve the `queue_id` for the `“pre_lease”` report which in turn efficiently queues the report request. Be sure to handle missing `property_ids` and derive the correct `period` filter parameter using a `report_date` with sensible defaults based off the current date/month/year.

`request_entrata_leasing_report`: 

```R
# leasing report summary and details --------------------------------------

request_entrata_leasing_report <- function(
  entrata_config = NULL,
  property_ids = NULL,
  report_date = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- load_entrata_config() }
  if (is.null(property_ids)) { property_ids <- get_entrata_properties()$property_id }
  if (is.null(report_date)) {
    now <- lubridate::today()
    current_month <- lubridate::month(now)
    if (current_month < 9) {
      report_date_year <- lubridate::year(now)
    } else {
      report_date_year <- lubridate::year(now) + 1
    }
    report_date <- paste0("09/01/", report_date_year)
  }

  req_reports <- httr2::request(entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("reports") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getReportData",
          version = "r3",
          params = list(
            reportName = "pre_lease",
            reportVersion = "3.2",
            filters = list(
              property_group_ids = as.list(as.character(property_ids)),
              period = list(date = report_date, period_type = "date"),
              summarize_by = "property",
              group_by = "do_not_group",
              consider_pre_leased_on = "332",
              charge_code_detail = 1,
              space_options = 'do_not_show',
              additional_units_shown = 'available',
              combine_unit_spaces_with_same_lease = 0,
              consolidate_by = 'no_consolidation',
              arrange_by_property = 0,
              subtotals = list("summary", "details"),
              yoy = 1
            )
          )
        )
      )
    )

  resp_reports <- httr2::req_perform(req_reports)
  resp_queue_id <- httr2::resp_body_json(resp_reports) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "The report data has been successfully queued for processing.",
      "The queue ID is: {.field {resp_queue_id}}"
    )
  )

  return(resp_queue_id)

}
```

### Request Entrata Weekly Leasing Report Data

The `request_entrata_weekly_leasing_report()` function follows a similar pattern but requests data over the last week by defining a daterange. Specifically, the function will perform the request to `/reports` `getReportData` to retrieve the `queue_id` for the `“leasae_execution_(applicant)”` report. This function returns the queueId to retrieve the weekly data.

`request_entrata_weekly_leasing_report`: 

```R
request_entrata_weekly_leasing_report <- function(
  entrata_config = NULL,
  property_ids = NULL
) {

  if (is.null(entrata_config)) { entrata_config <- load_entrata_config() }
  if (is.null(property_ids)) { property_ids <- get_entrata_properties()$property_id }

  req_reports <- httr2::request(base_url = entrata_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("reports") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = entrata_config$username,
      password = entrata_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = 'basic'),
        method = list(
          name = "getReportData",
          version = "1.8",
          params = list(
            reportName = 'lease_execution_(applicant)',
            filters = list(
              property_group_ids = as.list(as.character(property_ids)),
              period = list(
                daterange = list(
                  "daterange-start" = as.Date(get_weekly_period_start_date(), format = "%m/%d/%Y"),
                  "daterange-end" = as.Date(format(lubridate::today(), "%m/%d/%Y"), format = '%m/%d/%Y')
                ),
                "period_type" = "daterange"
              ),
              results_based_on = "activity",
              lease_type = c("1", "3"),
              summarize_by = "lease_type",
              group_by = "property",
              consolidate_by = "no_consolidation",
              arrange_by_property = "0",
              subtotals = "0"
            )
          )
        )
      )
    )

  resp_reports <- httr2::req_perform(req_reports)
  resp_queue_id <- httr2::resp_body_json(resp_reports) |>
    purrr::pluck("response", "result", "queueId")

  cli::cli_alert_info(
    c(
      "The report data has been successfully queued for processing.",
      "The queue ID is: {.field {resp_queue_id}}"
    )
  )

  return(resp_queue_id)

}
```

### Retrieve Queued Report Data via Queue ID

The next function, `retrieve_report_data()` checks the `/queue` endpoint's status providing the `queue_id` and when ready, will retrieve the report data. The function includes necessary retry logic and handles responses to return a structured dataset.

`retrieve_report_data()`:

```R
# Retrieve Report Data
retrieve_report_data <- function(
  queue_id,
  api_config = NULL
) {
  if (is.null(api_config)) { api_config <- load_entrata_config() }
  
  req_queue <- httr2::request(base_url = api_config$base_url) |>
    httr2::req_method("POST") |>
    httr2::req_url_path_append("queue") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_auth_basic(
      user = api_config$username,
      password = api_config$password
    ) |>
    httr2::req_body_json(
      list(
        auth = list(type = "basic"),
        requestId = 15,
        method = list(
          name = "getResponse",
          version = "r1",
          params = list(
            queueId = queue_id,
            serviceName = "getReportData"
          )
        )
      )
    ) |>
    httr2::req_retry(
      max_tries = 10,
      backoff = ~ lubridate::seconds(2 ^ (.x - 1)),
      is_transient = \(resp) {
        !is.null(httr2::resp_body_json(resp)$response$error)
      }
    )
  
  resp_queue <- httr2::req_perform(req_queue)
  
  resp_data <- httr2::resp_body_json(resp_queue) |>
    purrr::pluck("response", "result", "reportData") |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE) |>
    jsonlite::fromJSON(flatten = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select("property_name", "lease_type", "signed") |>
    tidyr::pivot_wider(names_from = lease_type, values_from = signed) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0))) |>
    dplyr::rename(new_leases = `New Lease`, new_renewals = Renewal) |>
    dplyr::mutate(new_total = new_leases + new_renewals)
  
  return(resp_data)
}
```

## Data Service Layer Functions

To finalize this pipeline, we will need to add a variety of additional functions such as loading of additional data sources (database, other endpoints, etc.), including the `model beds` manual metric for the "Investment Partners", and managing the rate limits/access layer:

- `load_model_beds(conn)`
- `load_investment_partners(conn)`
- `calculate_metrics(report_data, weekly_data)`
- `assemble_summary_table(report_data, weekly_data, model_beds, investment_partners)`
