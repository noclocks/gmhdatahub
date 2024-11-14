library(shiny)
library(bslib)
library(bsicons)
library(DT)

ui <- page_navbar(
  title = htmltools::tags$div(
    class = "navbar-brand",
    htmltools::tags$a(
      href = "#",
      htmltools::tags$img(
        src = "https://cdn.brandfetch.io/gmhcommunities.com/logo",
        height = 50,
        width = "auto",
        alt = "GMH Logo"
      )
    )
  ),
  window_title = "GMH Leasing Dashboard",
  theme = bs_theme(
    version = 5,
    primary = "#0054A6",  # GMH blue
    "navbar-bg" = "#ffffff"
  ),
  sidebar = sidebar(
    title = "Controls",
    selectInput("property", "Select Property", choices = c("Property A", "Property B", "Property C")),
    dateRangeInput("daterange", "Date Range"),
    bslib::input_task_button("entrata_refresh", label = "Refresh Data", icon = shiny::icon("refresh"))
  ),
  nav_spacer(),
  nav_panel("Dashboard", icon = bs_icon("speedometer2"),
            layout_columns(
              fill = FALSE,
              gap = "1rem",
              value_box(
                title = "Occupancy Rate",
                value = textOutput("occupancy", inline = TRUE),
                showcase = bs_icon("house-check-fill"),
                theme_color = "primary",
                p("2% increase from last month", class = "small text-muted"),
                full_screen = TRUE
              ),
              value_box(
                title = "New Leases",
                value = textOutput("leases", inline = TRUE),
                showcase = bs_icon("file-earmark-text-fill"),
                theme_color = "success",
                p("5 more than last month", class = "small text-muted"),
                full_screen = TRUE
              ),
              value_box(
                title = "Revenue",
                value = textOutput("revenue", inline = TRUE),
                showcase = bs_icon("currency-dollar"),
                theme_color = "info",
                p("15% increase YoY", class = "small text-muted"),
                full_screen = TRUE
              )
            ),
            layout_columns(
              fill = FALSE,
              gap = "1rem",
              card(
                card_header(bs_icon("graph-up"), "Key Metrics"),
                card_body(
                  "This is where you would display additional metrics or charts."
                ),
                full_screen = TRUE
              ),
              card(
                card_header(bs_icon("clock-history"), "Recent Activity"),
                card_body(
                  "This section could show recent updates or activities."
                ),
                full_screen = TRUE
              )
            )
  ),
  nav_panel("Summary", icon = bs_icon("bar-chart-line"),
            card(
              card_header(bs_icon("clipboard-data"), "Data Summary"),
              card_body(
                "Here you can display summary statistics or visualizations.",
                DT::DTOutput("summary_table")
              )
            )
  ),
  nav_panel("Reports", icon = bs_icon("file-earmark-text"),
            card(
              card_header(bs_icon("folder"), "Available Reports"),
              card_body(
                "List of available reports or report generation options would go here."
              )
            )
  ),
  nav_spacer(),
  # nav_menu(
  #
  #   nav_item("Email Support", icon = bs_icon("envelope"), href = "mailto:support@example.com"),
  #   nav_item("Phone Support", icon = bs_icon("telephone"), href = "tel:+1234567890"),
  #   nav_item("Live Chat", icon = bs_icon("chat-dots"), href = "#")
  # ),
  # nav_menu(
  #
  #   align = "right",
  #   nav_item("Profile", icon = bs_icon("person"), href = "#"),
  #   nav_item("Settings", icon = bs_icon("gear"), href = "#"),
  #   nav_item("Logout", icon = bs_icon("box-arrow-right"), href = "#")
  # )
  nav_menu(
    title = "Links",
    icon = bsicons::bs_icon("link-45deg"),
    align = "right",
    nav_item(
      htmltools::tags$a(
        shiny::icon("github"),
        "GitHub",
        href = "https://github.com/noclocks/gmhdatahub",
        target = "_blank"
      )
    ),
    nav_item(
      htmltools::tags$a(
        shiny::icon("book"),
        "Documentation",
        href = "https://noclocks.github.io/gmhdatahub/",
        target = "_blank"
      )
    )
  ),
  nav_menu(
    title = "Contact",
    icon = bsicons::bs_icon("envelope"),
    align = "right",
    nav_item(
      htmltools::tags$a(
        shiny::icon("envelope"),
        "Email Support",
        href = "mailto:support@noclocks.dev",
        target = "_blank"
      )
    )
  ),
  nav_menu(
    title = "User",
    icon = bs_icon("person-circle"),
    align = "right",
    nav_item(
      htmltools::tags$a(
        shiny::icon("person"),
        "John Doe",
        href = "#"
      ),
      nav_item(
        htmltools::tags$a(
          shiny::icon("gear"),
          "Settings",
          href = "#"
        )
      ),
      nav_item(
        htmltools::tags$a(
          shiny::icon("box-arrow-right"),
          "Logout",
          href = "#"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$occupancy <- renderText("95%")
  output$leases <- renderText("23")
  output$revenue <- renderText("$127,500")


  summary_data <- reactive({
    data = data.frame(
      property_name = c("Property A", "Property B", "Property C"),
      occupancy_rate = c(0.95, 0.87, 0.92),
      new_leases = c(23, 15, 18),
      revenue = c(127500, 98000, 110000)
    )
    data <- data[data$property_name %in% input$property, ]
    data
  })

  output$summary_table <- renderDT({
    datatable(
      data = summary_data(),
      class = 'cell-border stripe hover compact table-responsive',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    ) |>
      formatPercentage(columns = "occupancy_rate", digits = 1) |>
      formatCurrency(columns = "revenue", currency = "$", digits = 0)
  })
}

shinyApp(ui, server)
