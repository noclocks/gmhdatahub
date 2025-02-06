library(shiny)
library(bslib)
library(reactable)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(apexcharter)
library(dplyr)
library(tidyr)
library(DT)
library(bsicons)

prep_velocity_chart_data <- function(data, metrics) {

  if (length(metrics) == 0) return(NULL)

  data |>
    dplyr::select(
      property_name,
      tidyselect::all_of(metrics)
    ) |>
    tidyr::pivot_longer(
      cols = -property_name,
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == "occupancy" ~ "Occupancy %",
        metric == "vacancy" ~ "Vacancy %",
        metric == "prelease" ~ "Pre-lease %",
        metric == "total_beds" ~ "Total Beds",
        metric == "total_units" ~ "Total Units",
        metric == "total_renewals" ~ "Total Renewals",
        metric == "available_units" ~ "Available Units",
        metric == "weekly_new_leases" ~ "Weekly New Leases",
        metric == "weekly_renewals" ~ "Weekly Renewals",
        TRUE ~ metric
      )
    )
}

prep_rates_chart_data <- function(data, rent_type, metrics) {

  rent_cols <- if(rent_type == "bed") {
    c("market_rent_per_bed", "effective_rent_per_bed", "bundled_rent_per_bed")[metrics %in% c("market", "effective", "bundled")]
  } else {
    c("market_rent_per_unit", "effective_rent_per_unit", "bundled_rent_per_unit")[metrics %in% c("market", "effective", "bundled")]
  }

  if(length(rent_cols) == 0) return(NULL)

  data |>
    dplyr::select(
      property_name,
      tidyselect::all_of(rent_cols)
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(rent_cols),
      names_to = "rent_type",
      values_to = "amount"
    )

}

chart_velocity <- function(data) {

  if (is.null(data)) return(NULL)

  apexcharter::apex(
    data,
    type = "line",
    mapping = apexcharter::aes(
      x = property_name,
      y = value,
      group = metric
    )
  ) |>
    apexcharter::ax_series(
      list(
        name = unique(data$metric)
      )
    ) |>
    apexcharter::ax_yaxis(
      decimalsInFloat = 1,
      labels = list(
        formatter = apexcharter::JS("function(val) {
          if (this.seriesName && this.seriesName.includes('%')) {
            return val.toFixed(1) + '%';
          }
          return Math.round(val);
        }")
      )
    ) |>
    apexcharter::ax_xaxis(
      type = "category"
    ) |>
    apexcharter::ax_tooltip(
      shared = TRUE,
      y = list(
        formatter = apexcharter::JS("function(val, { seriesIndex, w }) {
          if (w.globals.seriesNames[seriesIndex].includes('%')) {
            return val.toFixed(1) + '%';
          }
          return Math.round(val);
        }")
      )
    ) |>
    apexcharter::ax_stroke(
      width = 2,
      curve = "smooth"
    ) |>
    apexcharter::ax_markers(
      size = 4
    ) |>
    apexcharter::ax_grid(
      show = TRUE,
      borderColor = "#e0e0e0",
      strokeDashArray = 0,
      position = "back"
    ) |>
    apexcharter::ax_title(
      text = "Leasing Velocity Metrics",
      align = "center",
      style = list(
        fontSize = "16px"
      )
    ) |>
    apexcharter::ax_legend(
      position = "top",
      horizontalAlign = "center"
    )

}

chart_rates_comparison <- function(data, rent_type) {

  if(is.null(data)) return(NULL)

  apexcharter::apex(
    data = data,
    type = "bar",
    mapping = apexcharter::aes(x = property_name, y = amount, fill = rent_type)
  ) |>
    apexcharter::ax_title(
      text = paste("Rent Analysis by Property -",
                   if (rent_type == "bed") "Per Bed" else "Per Unit"),
      align = "center",
      style = list(fontSize = "16px")
    ) |>
    apexcharter::ax_yaxis(
      labels = list(
        formatter = apexcharter::JS("function(val) { return '$' + val.toFixed(0) }")
      )
    ) |>
    apexcharter::ax_tooltip(
      shared = TRUE,
      y = list(
        formatter = apexcharter::JS("function(val) { return '$' + val.toFixed(0) }")
      )
    ) |>
    apexcharter::ax_legend(
      position = "top",
      horizontalAlign = "center"
    )
}

# Sample initial data
properties_data <- data.frame(
  property_id = 1:3,
  property_name = c("The Hub", "University Village", "Campus Corner"),
  address = c("123 College St", "456 University Ave", "789 Campus Dr"),
  lat = c(40.7128, 40.7150, 40.7110),
  lng = c(-74.0060, -74.0080, -74.0040),
  type = c("Property", "Competitor", "Competitor"),
  total_units = c(200, 180, 150),
  total_beds = c(600, 540, 450),
  market_rent_bed = c(899, 850, 825),
  effective_rent_bed = c(875, 825, 800),
  bundled_rent_bed = c(925, 875, 850),
  market_rent_unit = c(2697, 2550, 2475),
  effective_rent_unit = c(2625, 2475, 2400),
  bundled_rent_unit = c(2775, 2625, 2550),
  occupancy = c(95, 92, 90),
  vacancy = c(5, 8, 10),
  prelease = c(65, 60, 58),
  total_renewals = c(120, 100, 90),
  available_units = c(10, 14, 15),
  weekly_new_leases = c(8, 6, 5),
  weekly_renewals = c(4, 3, 2)
)

# Sample university data
universities_data <- data.frame(
  university_name = c("State University", "Tech Institute", "Liberal Arts College"),
  address = c("1000 University Dr", "2000 Tech Blvd", "3000 Liberal Arts Way"),
  lat = c(40.7140, 40.7160, 40.7130),
  lng = c(-74.0070, -74.0090, -74.0050),
  enrollment = c(25000, 15000, 5000)
)

ui <- page_sidebar(
  title = "Student Housing Market Analysis",
  theme = bs_theme(version = 5),

  # Add Google Places JavaScript
  tags$head(
    tags$script(src = sprintf("https://maps.googleapis.com/maps/api/js?key=%s&libraries=places", "AIzaSyA9iicnOPVX6NmifeCQNYtEhIciwdSgaOc"))
  ),

  sidebar = sidebar(
    selectInput("selected_property",
                tags$span(bs_icon("building"), "Select Property"),
                choices = unique(properties_data$property_name[properties_data$type == "Property"])),
    selectInput("selected_competitors",
                tags$span(bs_icon("buildings"), "Select Competitors"),
                choices = unique(properties_data$property_name[properties_data$type == "Competitor"]),
                multiple = TRUE,
                selected = unique(properties_data$property_name[properties_data$type == "Competitor"])),
    dateInput("leasing_week",
              tags$span(bs_icon("calendar"), "Leasing Week Start Date"),
              value = Sys.Date()),
    radioButtons("rent_type",
                 tags$span(bs_icon("cash-stack"), "Rent Display"),
                 choices = c("Per Bed" = "bed", "Per Unit" = "unit")),
    checkboxGroupInput("rent_metrics",
                       tags$span(bs_icon("list-check"), "Rent Metrics"),
                       choices = c("Market Rent" = "market",
                                   "Effective Rent" = "effective",
                                   "Bundled Rent" = "bundled"),
                       selected = c("market", "effective", "bundled")),
    checkboxGroupInput("performance_metrics",
                       tags$span(bs_icon("speedometer2"), "Performance Metrics"),
                       choices = c("Occupancy %" = "occupancy",
                                   "Vacancy %" = "vacancy",
                                   "Pre-Lease %" = "prelease",
                                   "Total Beds" = "total_beds",
                                   "Total Units" = "total_units",
                                   "Total Renewals" = "total_renewals",
                                   "Available Units" = "available_units",
                                   "Weekly New Leases" = "weekly_new_leases",
                                   "Weekly Renewals" = "weekly_renewals"),
                       selected = c("occupancy", "prelease", "weekly_new_leases"))
  ),

  bslib::page_fluid(
    navset_card_tab(
      nav_panel(
        "Dashboard",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          gap = "1rem",
          value_box(
            title = "Average Occupancy %",
            value = textOutput("avg_occupancy"),
            showcase = bsicons::bs_icon("percent")
          ),
          value_box(
            title = "Average Pre-Lease %",
            value = textOutput("avg_prelease"),
            showcase = bsicons::bs_icon("graph-up-arrow")
          ),
          value_box(
            title = "Total Weekly Leases",
            value = textOutput("total_weekly_leases"),
            showcase = bsicons::bs_icon("file-earmark-text")
          ),
          value_box(
            title = "Available Units",
            value = textOutput("total_available"),
            showcase = bsicons::bs_icon("buildings")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          card(
            full_screen = TRUE,
            class = "p-0",
            card_header(
              tags$span(
                bsicons::bs_icon("geo-alt"),
                "Property Map"
              )
            ),
            leafletOutput("map", height = 400)
          ),
          card(
            full_screen = TRUE,
            card_header(
              tags$span(
                bs_icon("table"),
                "Property Details"
              )
            ),
            reactableOutput("property_table")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          card(
            full_screen = TRUE,
            height = "400px",
            card_header(
              tags$span(
                bs_icon("graph-up"),
                "Weekly Velocity"
              )
            ),
            apexchartOutput("velocity_chart", height = 350)
          ),
          card(
            full_screen = TRUE,
            height = "400px",
            card_header(
              tags$span(
                bs_icon("currency-dollar"),
                "Rate Analysis"
              )
            ),
            apexchartOutput("rates_chart", height = 350)
          )
        )
      ),

      nav_panel(
        "Market Survey",
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",
          card(
            full_screen = TRUE,
            card_header(
              tags$span(
                bs_icon("pencil-square"),
                "Add/Update Property Data"
              )
            ),
            # Action type selection
            radioButtons("action_type", "Action",
                         choices = c("Add New" = "new", "Edit Existing" = "edit"),
                         selected = "new",
                         inline = TRUE),

            # Edit existing property selector (shown only when editing)
            conditionalPanel(
              condition = "input.action_type == 'edit'",
              selectInput("edit_property", "Select Property to Edit",
                          choices = c("Select a property" = ""))
            ),

            # New property address input (shown only when adding new)
            conditionalPanel(
              condition = "input.action_type == 'new'",
              textInput("address", "Property Address",
                        placeholder = "Start typing an address..."),
              tags$script(HTML("
          $(document).ready(function() {
            var addressInput = document.getElementById('address');
            var autocomplete = new google.maps.places.Autocomplete(addressInput);

            autocomplete.addListener('place_changed', function() {
              var place = autocomplete.getPlace();
              if (place.geometry) {
                // Send place details to Shiny
                Shiny.setInputValue('selected_place', {
                  address: place.formatted_address,
                  lat: place.geometry.location.lat(),
                  lng: place.geometry.location.lng(),
                  name: place.name,
                  types: place.types,
                  nearby: place.address_components
                });
              }
            });
          });
        "))
            ),

            # Property details form
            uiOutput("property_form")
          ),
          card(
            full_screen = TRUE,
            card_header(
              tags$span(
                bs_icon("clipboard-data"),
                "Submitted Data"
              )
            ),
            DTOutput("submitted_data")
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  # Reactive values for storing property data
  rv <- reactiveVal(properties_data)

  # Filtered data based on selected property and competitors
  filtered_data <- reactive({
    data <- rv()
    data %>%
      filter(property_name %in% c(input$selected_property, input$selected_competitors))
  })

  # Value box calculations
  output$avg_occupancy <- renderText({
    paste0(round(mean(filtered_data()$occupancy), 1), "%")
  })

  output$avg_prelease <- renderText({
    paste0(round(mean(filtered_data()$prelease), 1), "%")
  })

  output$total_weekly_leases <- renderText({
    sum(filtered_data()$weekly_new_leases)
  })

  output$total_available <- renderText({
    sum(filtered_data()$available_units)
  })

  # Update competitors when property changes
  observeEvent(input$selected_property, {
    competitors <- properties_data$property_name[properties_data$type == "Competitor"]
    updateSelectInput(session, "selected_competitors",
                      choices = competitors,
                      selected = competitors)
  })

  # Map
  output$map <- renderLeaflet({
    # Create color-coded icons for different types
    icons <- awesomeIconList(
      property = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "blue"),
      competitor = makeAwesomeIcon(icon = "building", library = "fa", markerColor = "red"),
      university = makeAwesomeIcon(icon = "graduation-cap", library = "fa", markerColor = "green")
    )

    # Initialize base map with tile layers
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basic") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")

    # Add property markers
    if (nrow(filtered_data() %>% filter(type == "Property")) > 0) {
      map <- map %>%
        addAwesomeMarkers(
          data = filtered_data() %>% filter(type == "Property"),
          lng = ~lng,
          lat = ~lat,
          icon = ~icons[["property"]],
          group = "Properties",
          popup = ~paste(
            "<b>", property_name, "</b><br>",
            "Type: Property<br>",
            "Units: ", total_units, "<br>",
            "Beds: ", total_beds, "<br>",
            "Occupancy: ", occupancy, "%<br>",
            "Pre-lease: ", prelease, "%"
          ),
          label = ~property_name
        )
    }

    # Add competitor markers
    if (nrow(filtered_data() %>% filter(type == "Competitor")) > 0) {
      map <- map %>%
        addAwesomeMarkers(
          data = filtered_data() %>% filter(type == "Competitor"),
          lng = ~lng,
          lat = ~lat,
          icon = ~icons[["competitor"]],
          group = "Competitors",
          popup = ~paste(
            "<b>", property_name, "</b><br>",
            "Type: Competitor<br>",
            "Units: ", total_units, "<br>",
            "Beds: ", total_beds, "<br>",
            "Occupancy: ", occupancy, "%<br>",
            "Pre-lease: ", prelease, "%"
          ),
          label = ~property_name
        )
    }

    # Add university markers
    if (nrow(universities_data) > 0) {
      map <- map %>%
        addAwesomeMarkers(
          data = universities_data,
          lng = ~lng,
          lat = ~lat,
          icon = ~icons[["university"]],
          group = "Universities",
          popup = ~paste(
            "<b>", university_name, "</b><br>",
            "Enrollment: ", enrollment, "<br>",
            "Address: ", address
          ),
          label = ~university_name
        )
    }

    # Add controls
    map <- map %>%
      addLayersControl(
        baseGroups = c("Basic", "Street", "Satellite"),
        overlayGroups = c("Properties", "Competitors", "Universities"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs",
        title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")
      )) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )

    # Set initial view to encompass all markers
    all_data <- bind_rows(
      filtered_data() %>% select(lat, lng),
      universities_data %>% select(lat, lng)
    )

    if (nrow(all_data) > 0) {
      map %>% fitBounds(
        min(all_data$lng),
        min(all_data$lat),
        max(all_data$lng),
        max(all_data$lat)
      )
    } else {
      map %>% setView(lng = -74.0060, lat = 40.7128, zoom = 12)
    }
  })

  # Property table
  output$property_table <- renderReactable({
    display_cols <- c(
      "property_name",
      if(input$rent_type == "bed") {
        c("market_rent_bed", "effective_rent_bed", "bundled_rent_bed")[input$rent_metrics %in% c("market", "effective", "bundled")]
      } else {
        c("market_rent_unit", "effective_rent_unit", "bundled_rent_unit")[input$rent_metrics %in% c("market", "effective", "bundled")]
      },
      intersect(input$performance_metrics, names(filtered_data()))
    )

    display_data <- filtered_data() %>% select(all_of(display_cols))

    col_defs <- list(
      property_name = colDef(name = "Property"),
      market_rent_bed = colDef(name = "Market Rent/Bed", format = colFormat(prefix = "$")),
      effective_rent_bed = colDef(name = "Effective Rent/Bed", format = colFormat(prefix = "$")),
      bundled_rent_bed = colDef(name = "Bundled Rent/Bed", format = colFormat(prefix = "$")),
      market_rent_unit = colDef(name = "Market Rent/Unit", format = colFormat(prefix = "$")),
      effective_rent_unit = colDef(name = "Effective Rent/Unit", format = colFormat(prefix = "$")),
      bundled_rent_unit = colDef(name = "Bundled Rent/Unit", format = colFormat(prefix = "$")),
      occupancy = colDef(name = "Occupancy %", format = colFormat(suffix = "%")),
      vacancy = colDef(name = "Vacancy %", format = colFormat(suffix = "%")),
      prelease = colDef(name = "Pre-lease %", format = colFormat(suffix = "%")),
      total_beds = colDef(name = "Total Beds"),
      total_units = colDef(name = "Total Units"),
      total_renewals = colDef(name = "Total Renewals"),
      available_units = colDef(name = "Available Units"),
      weekly_new_leases = colDef(name = "Weekly New Leases"),
      weekly_renewals = colDef(name = "Weekly Renewals")
    )

    # Only keep column definitions for columns that are actually in the display data
    col_defs <- col_defs[names(col_defs) %in% names(display_data)]

    reactable(
      display_data,
      columns = col_defs
    )
  })

  # Velocity chart
  output$velocity_chart <- renderApexchart({
    # Get selected metrics
    metrics <- input$performance_metrics

    # Skip if no metrics selected
    if(length(metrics) == 0) {
      return(NULL)
    }

    # Prepare data
    data_long <- filtered_data() %>%
      select(property_name, all_of(metrics)) %>%
      pivot_longer(
        cols = -property_name,
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = case_when(
          metric == "occupancy" ~ "Occupancy %",
          metric == "vacancy" ~ "Vacancy %",
          metric == "prelease" ~ "Pre-lease %",
          metric == "total_beds" ~ "Total Beds",
          metric == "total_units" ~ "Total Units",
          metric == "total_renewals" ~ "Total Renewals",
          metric == "available_units" ~ "Available Units",
          metric == "weekly_new_leases" ~ "Weekly New Leases",
          metric == "weekly_renewals" ~ "Weekly Renewals",
          TRUE ~ metric
        )
      )

    apex(data_long,
         type = "line",
         mapping = aes(x = property_name, y = value, group = metric)) %>%
      ax_series(list(
        name = unique(data_long$metric)
      )) %>%
      ax_yaxis(
        decimalsInFloat = 1,
        labels = list(
          formatter = JS("function(val) {
          if (this.seriesName && this.seriesName.includes('%')) {
            return val.toFixed(1) + '%';
          }
          return Math.round(val);
        }")
        )
      ) %>%
      ax_xaxis(
        type = "category"
      ) %>%
      ax_tooltip(
        shared = TRUE,
        y = list(
          formatter = JS("function(val, { seriesIndex, w }) {
          if (w.globals.seriesNames[seriesIndex].includes('%')) {
            return val.toFixed(1) + '%';
          }
          return Math.round(val);
        }")
        )
      ) %>%
      ax_stroke(width = 2, curve = "smooth") %>%
      ax_markers(size = 4) %>%
      ax_grid(
        show = TRUE,
        borderColor = "#e0e0e0",
        strokeDashArray = 0,
        position = "back"
      ) %>%
      ax_title(
        text = "Performance Metrics",
        align = "center",
        style = list(fontSize = "16px")
      ) %>%
      ax_legend(
        position = "top",
        horizontalAlign = "center"
      )
  })

  # Rates chart
  output$rates_chart <- renderApexchart({
    rent_cols <- if(input$rent_type == "bed") {
      c("market_rent_bed", "effective_rent_bed", "bundled_rent_bed")[input$rent_metrics %in% c("market", "effective", "bundled")]
    } else {
      c("market_rent_unit", "effective_rent_unit", "bundled_rent_unit")[input$rent_metrics %in% c("market", "effective", "bundled")]
    }

    data_long <- filtered_data() %>%
      select(property_name, all_of(rent_cols)) %>%
      pivot_longer(
        cols = all_of(rent_cols),
        names_to = "rent_type",
        values_to = "amount"
      )

    apex(
      data = data_long,
      type = "bar",
      mapping = aes(x = property_name, y = amount, fill = rent_type)
    ) %>%
      ax_title(text = paste("Rent Analysis by Property -", if(input$rent_type == "bed") "Per Bed" else "Per Unit")) %>%
      ax_yaxis(labels = list(formatter = JS("function(val) { return '$' + val }")))
  })

  output$property_form <- renderUI({
    if (input$action_type == "new" && !is.null(input$selected_place)) {
      # For new properties, show form with pre-populated data from address
      place_data <- input$selected_place

      # Detect if it's likely student housing based on proximity to university
      is_student_housing <- any(grepl("university|college", tolower(place_data$nearby)))
      suggested_type <- if(is_student_housing) "Property" else "Competitor"

      # Extract suggested name from place data
      suggested_name <- if(!is.null(place_data$name) && place_data$name != "") {
        place_data$name
      } else {
        # Create name from address components
        paste("Property at", strsplit(place_data$address, ",")[[1]][1])
      }

      tagList(
        textInput("property_name", tags$span(bs_icon("building"), "Property Name"), value = suggested_name),
        selectInput("type", tags$span(bs_icon("tag"), "Property Type"),
                    choices = c("Property", "Competitor"),
                    selected = suggested_type),
        numericInput("total_units", tags$span(bs_icon("houses"), "Total Units"), value = 0),
        numericInput("total_beds", tags$span(bs_icon("person-workspace"), "Total Beds"), value = 0),
        numericInput("market_rent_bed", tags$span(bs_icon("tags"), "Market Rent per Bed"), value = 0),
        numericInput("effective_rent_bed", tags$span(bs_icon("tag-fill"), "Effective Rent per Bed"), value = 0),
        numericInput("bundled_rent_bed", tags$span(bs_icon("tags-fill"), "Bundled Rent per Bed"), value = 0),
        numericInput("occupancy", tags$span(bs_icon("percent"), "Occupancy %"), value = 0),
        numericInput("prelease", tags$span(bs_icon("graph-up-arrow"), "Pre-lease %"), value = 0),
        actionButton("submit", "Submit Data", class = "btn-primary")
      )
    } else if (input$action_type == "edit" && input$edit_property != "") {
      # For editing, show form with current property data
      tagList(
        textInput("property_name", tags$span(bs_icon("building"), "Property Name"), value = suggested_name),
        selectInput("type", tags$span(bs_icon("tag"), "Property Type"),
                    choices = c("Property", "Competitor"),
                    selected = suggested_type),
        numericInput("total_units", tags$span(bs_icon("houses"), "Total Units"), value = 0),
        numericInput("total_beds", tags$span(bs_icon("person-workspace"), "Total Beds"), value = 0),
        numericInput("market_rent_bed", tags$span(bs_icon("tags"), "Market Rent per Bed"), value = 0),
        numericInput("effective_rent_bed", tags$span(bs_icon("tag-fill"), "Effective Rent per Bed"), value = 0),
        numericInput("bundled_rent_bed", tags$span(bs_icon("tags-fill"), "Bundled Rent per Bed"), value = 0),
        numericInput("occupancy", tags$span(bs_icon("percent"), "Occupancy %"), value = 0),
        numericInput("prelease", tags$span(bs_icon("graph-up-arrow"), "Pre-lease %"), value = 0),
        actionButton("submit", "Update Data", class = "btn-primary")
      )
    } else if (input$action_type == "new") {
      # Show message when no address is selected
      tags$p("Please enter and select an address to continue...")
    }
  })

  # Handle place selection for new properties
  observeEvent(input$selected_place, {
    req(input$selected_place)
    place_data <- input$selected_place

    # Find nearest university
    nearest_univ <- universities_data %>%
      mutate(
        dist = sqrt((lat - place_data$lat)^2 + (lng - place_data$lng)^2)
      ) %>%
      arrange(dist) %>%
      slice(1)

    # Store university data for use in submission
    if(nrow(nearest_univ) > 0) {
      session$userData$nearest_university <- nearest_univ
    }
  })

  # Submitted data table
  output$submitted_data <- renderDT({
    datatable(rv(), options = list(pageLength = 5))
  })

  # Update edit_property choices based on filtered data
  observe({
    selected_properties <- c(input$selected_property, input$selected_competitors)
    updateSelectInput(session, "edit_property",
                      choices = c("Add New Property" = "", selected_properties))
  })

  # Pre-populate form when a property is selected to edit
  observeEvent(input$edit_property, {
    if (input$edit_property != "") {
      property_data <- filtered_data() %>%
        filter(property_name == input$edit_property) %>%
        as.list()

      updateTextInput(session, "property_name", value = property_data$property_name)
      updateSelectInput(session, "type", selected = property_data$type)
      updateNumericInput(session, "total_units", value = property_data$total_units)
      updateNumericInput(session, "total_beds", value = property_data$total_beds)
      updateNumericInput(session, "market_rent_bed", value = property_data$market_rent_bed)
      updateNumericInput(session, "effective_rent_bed", value = property_data$effective_rent_bed)
      updateNumericInput(session, "bundled_rent_bed", value = property_data$bundled_rent_bed)
      updateNumericInput(session, "occupancy", value = property_data$occupancy)
      updateNumericInput(session, "prelease", value = property_data$prelease)
    }
  })

  # Handle form submission
  observeEvent(input$submit, {
    if (input$action_type == "new") {
      req(input$selected_place)
      place_data <- input$selected_place

      new_row <- data.frame(
        property_id = nrow(rv()) + 1,
        property_name = input$property_name,
        address = place_data$address,
        lat = place_data$lat,
        lng = place_data$lng,
        type = input$type,
        nearest_university = if(!is.null(session$userData$nearest_university))
          session$userData$nearest_university$university_name else NA,
        total_units = input$total_units,
        total_beds = input$total_beds,
        market_rent_bed = input$market_rent_bed,
        effective_rent_bed = input$effective_rent_bed,
        bundled_rent_bed = input$bundled_rent_bed,
        market_rent_unit = input$market_rent_bed * (input$total_beds/input$total_units),
        effective_rent_unit = input$effective_rent_bed * (input$total_beds/input$total_units),
        bundled_rent_unit = input$bundled_rent_bed * (input$total_beds/input$total_units),
        occupancy = input$occupancy,
        vacancy = 100 - input$occupancy,
        prelease = input$prelease,
        total_renewals = 0,
        available_units = input$total_units * (100 - input$occupancy) / 100,
        weekly_new_leases = 0,
        weekly_renewals = 0
      )

      # Add new record
      rv(rbind(rv(), new_row))

    } else if (input$action_type == "edit") {
      req(input$edit_property)

      # Get existing record for the property being edited
      existing_record <- filtered_data() %>%
        filter(property_name == input$edit_property)

      # Update the record
      updated_row <- data.frame(
        property_id = existing_record$property_id,
        property_name = input$property_name,
        address = existing_record$address,  # Keep existing address
        lat = existing_record$lat,         # Keep existing coordinates
        lng = existing_record$lng,
        type = input$type,
        nearest_university = existing_record$nearest_university,
        total_units = input$total_units,
        total_beds = input$total_beds,
        market_rent_bed = input$market_rent_bed,
        effective_rent_bed = input$effective_rent_bed,
        bundled_rent_bed = input$bundled_rent_bed,
        market_rent_unit = input$market_rent_bed * (input$total_beds/input$total_units),
        effective_rent_unit = input$effective_rent_bed * (input$total_beds/input$total_units),
        bundled_rent_unit = input$bundled_rent_bed * (input$total_beds/input$total_units),
        occupancy = input$occupancy,
        vacancy = 100 - input$occupancy,
        prelease = input$prelease,
        total_renewals = existing_record$total_renewals,      # Preserve existing values
        available_units = input$total_units * (100 - input$occupancy) / 100,
        weekly_new_leases = existing_record$weekly_new_leases, # Preserve existing values
        weekly_renewals = existing_record$weekly_renewals      # Preserve existing values
      )

      # Update existing record
      current_data <- rv()
      current_data[current_data$property_name == input$edit_property,] <- updated_row
      rv(current_data)
    }

    # Reset form
    updateRadioButtons(session, "action_type", selected = "new")
    updateSelectInput(session, "edit_property", selected = "")
    updateTextInput(session, "property_name", value = "")
    updateNumericInput(session, "total_units", value = 0)
    updateNumericInput(session, "total_beds", value = 0)
    updateNumericInput(session, "market_rent_bed", value = 0)
    updateNumericInput(session, "effective_rent_bed", value = 0)
    updateNumericInput(session, "bundled_rent_bed", value = 0)
    updateNumericInput(session, "occupancy", value = 0)
    updateNumericInput(session, "prelease", value = 0)

    # Show success message
    msg <- if(input$action_type == "new") "New property added successfully" else "Property updated successfully"
    showNotification(msg)
  })
}

shinyApp(ui, server)
