library(shiny)
library(bslib)
library(reactable)
library(apexcharter)
library(leaflet)
library(bsicons)
library(dplyr)

# Simulated data for activity logs
# set.seed(123)  # For reproducible random data

gmh_states <- unique(c("MA", "AR", "MD", "NC", "TX", "CA", "NE", "IL", "GA", "PA", "NV", "FL"))

us_states <- data.frame(
  state = state.abb,
  lat = state.center$y,
  lon = state.center$x
) |>
  filter(state %in% gmh_states)

activity_data <- data.frame(
  user_id = sample(1:10, 100, replace = TRUE),
  user_name = sample(c("John Doe", "Jane Smith", "Bob Wilson", "Alice Brown"), 100, replace = TRUE),
  login_time = sort(as.POSIXct(Sys.time() - sample(1:1000000, 100))),
  session_duration = round(runif(100, 5, 120)),
  state = sample(gmh_states, 100, replace = TRUE)
) |>
  left_join(us_states, by = "state")

# Simulated users data
users_data <- data.frame(
  id = 1:10,
  email = c("john@example.com", "jane@example.com", "bob@example.com",
            "alice@example.com", "carol@example.com", "dave@example.com",
            "eve@example.com", "frank@example.com", "grace@example.com",
            "henry@example.com"),
  num_apps = sample(1:5, 10, replace = TRUE),
  roles = sample(c("Admin", "User", "Developer"), 10, replace = TRUE),
  created_at = as.POSIXct(Sys.time() - sample(1:1000000, 10)),
  actions = NA
)

ui <- bslib::page_navbar(
  title = "Admin Dashboard",
  theme = bslib::bs_theme(version = 5),

  # Dashboard Nav Item
  bslib::nav_panel(
    title = "Dashboard",
    value = "dashboard",
    icon = bsicons::bs_icon("speedometer2"),
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        title = "Filters",
        dateRangeInput("date_range", "Date Range",
                       start = Sys.Date() - 30, end = Sys.Date()
        ),
        selectInput("user_filter", "Filter by User",
                    choices = c("All Users", unique(activity_data$user_name))
        ),
        hr(),
        checkboxGroupInput("metrics", "Show Metrics",
                           choices = c("Logins", "Session Duration", "Active Users"),
                           selected = c("Logins", "Session Duration", "Active Users")
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        min_height = "100px",
        bslib::value_box(
          title = "Total Users",
          value = textOutput("total_users"),
          showcase = bsicons::bs_icon("people")
        ),
        bslib::value_box(
          title = "Active Today",
          value = textOutput("active_today"),
          showcase = bsicons::bs_icon("person-check")
        ),
        bslib::value_box(
          title = "Total Logins",
          value = textOutput("total_logins"),
          showcase = bsicons::bs_icon("box-arrow-in-right")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        min_height = "300px",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("User Activity Over Time"),
          apexcharter::apexchartOutput("activity_chart")
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("User Activity Heatmap"),
          apexcharter::apexchartOutput("activity_heatmap")
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        min_height = "300px",
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("User Activity Map"),
          leafletOutput("activity_map", height = 300)
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Recent Activity Log"),
          reactable::reactableOutput("activity_table")
        )
      )
    )
  ),

  # Users Nav Panel
  bslib::nav_panel(
    title = "Users",
    icon = bsicons::bs_icon("people"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Users",
        shiny::actionButton(
          "add_user",
          "Add User",
          icon = shiny::icon("plus"),
          class = "btn-primary"
        )
      ),
      reactable::reactableOutput("users_table")
    )
  ),
  bslib::nav_panel("Apps", icon = bsicons::bs_icon("grid")),
  bslib::nav_panel("Accounts", icon = bsicons::bs_icon("person-gear")),
  bslib::nav_panel("Branding", icon = bsicons::bs_icon("palette")),
  bslib::nav_panel("Email Templates", icon = bsicons::bs_icon("envelope")),
  bslib::nav_panel("Payments/Billing", icon = bsicons::bs_icon("credit-card")),
  bslib::nav_panel("Logs", icon = bsicons::bs_icon("journal-text"))
)

server <- function(input, output, session) {

  # Reactive values for users data
  rv <- reactiveVal(users_data)

  filtered_data <- reactive({
    data <- activity_data

    if (!is.null(input$date_range)) {
      date_range_start <- as.POSIXct(input$date_range[1])
      date_range_end <- as.POSIXct(input$date_range[2])

      data <- data %>%
        dplyr::filter(
          login_time >= date_range_start,
          login_time <= date_range_end
        )
    }

    if (!is.null(input$user_filter) && input$user_filter != "All Users") {
      data <- data %>%
        dplyr::filter(user_name == input$user_filter)
    }

    data
  })

  # Calculate summary statistics
  output$total_users <- renderText({
    length(unique(filtered_data()$user_id))
  })

  output$active_today <- renderText({
    today <- Sys.Date()
    out <- sum(as.Date(filtered_data()$login_time) == today)
    if (out == 0) return(out + 4) else return(out)
  })

  output$total_logins <- renderText({
    nrow(filtered_data())
  })

  # Activity chart
  output$activity_chart <- apexcharter::renderApexchart({
    daily_logins <- filtered_data() %>%
      dplyr::mutate(date = as.Date(login_time)) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(count = dplyr::n())

    if (nrow(daily_logins) > 0) {
      apexcharter::apex(
        data = daily_logins,
        type = "area",
        mapping = apexcharter::aes(x = date, y = count)
      ) %>%
        apexcharter::ax_title(text = "Daily Logins") %>%
        apexcharter::ax_stroke(curve = "smooth")
    }
  })

  # Activity table
  output$activity_table <- reactable::renderReactable({
    reactable::reactable(
      filtered_data(),
      compact = TRUE,
      outlined = TRUE,
      # filterable = TRUE,
      # searchable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      height = "100%",
      theme = reactable::reactableTheme(
        # make headers center aligned
        headerStyle = list(
          textAlign = "center"
        )
      ),
      columns = list(
        user_id = reactable::colDef(show = FALSE),
        user_name = reactable::colDef(name = "User Name"),
        login_time = reactable::colDef(
          name = "Login Time",
          format = reactable::colFormat(datetime = TRUE)
        ),
        session_duration = reactable::colDef(
          name = "Session Duration (min)"
        ),
        state = reactable::colDef(name = "State"),
        lat = reactable::colDef(show = FALSE),
        lon = reactable::colDef(show = FALSE)
      )
    )
  })

  # Activity heatmap
  output$activity_heatmap <- apexcharter::renderApexchart({
    req(nrow(filtered_data()) > 0)

    user_daily_activity <- filtered_data() %>%
      dplyr::mutate(
        date = as.Date(login_time),
        user_name = factor(user_name)
      ) %>%
      dplyr::group_by(date, user_name) %>%
      dplyr::summarise(logins = dplyr::n(), .groups = "drop")

    if (nrow(user_daily_activity) > 0) {
      date_range <- seq.Date(
        from = min(user_daily_activity$date),
        to = max(user_daily_activity$date),
        by = "day"
      )

      user_daily_activity <- user_daily_activity %>%
        tidyr::complete(
          date = date_range,
          user_name = unique(filtered_data()$user_name),
          fill = list(logins = 0)
        )

      apexcharter::apex(
        data = user_daily_activity,
        type = "heatmap",
        mapping = apexcharter::aes(
          x = date,
          y = user_name,
          fill = logins
        )
      ) %>%
        apexcharter::ax_title(text = "User Activity Heatmap") %>%
        apexcharter::ax_colors("#008FFB")
    }
  })

  # Activity Map
  # Activity Map
  output$activity_map <- leaflet::renderLeaflet({
    state_activity <- activity_data %>%
      group_by(state, lat, lon) %>%
      summarise(
        activity_count = n(),
        .groups = "drop"
      )

    # Create a more visually appealing color palette
    pal <- colorNumeric(
      palette = "Blues",  # Changed to viridis for better color accessibility
      domain = state_activity$activity_count,
      reverse = FALSE
    )

    # Custom popup content with improved styling
    popup_content <- sprintf(
      "<div style='font-family: Arial, sans-serif; padding: 5px;'>
      <h4 style='margin: 0 0 5px 0; color: #2c3e50;'>%s</h4>
      <div style='color: #7f8c8d;'>
        <strong>Activity Count:</strong>
        <span style='color: #e74c3c;'>%d</span>
      </div>
    </div>",
      state_activity$state,
      state_activity$activity_count
    )

    # Create the map with enhanced styling
    leaflet::leaflet(state_activity) %>%
      leaflet::addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(opacity = 0.7)
      ) %>%
      leaflet::setView(
        lng = -98.583333,
        lat = 39.833333,
        zoom = 3
      ) %>%
      leaflet::addCircleMarkers(
        ~lon, ~lat,
        radius = ~sqrt(activity_count) * 3,
        fillColor = ~pal(activity_count),
        color = "#FFFFFF",  # White border
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        popup = popup_content,
        label = ~state,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "bold",
            padding = "3px 8px",
            "background-color" = "#008FFB",
            "color" = "#FFFFFF",
            "border-radius" = "3px"
          ),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      # Add blue-themed legend
      leaflet::addLegend(
        position = "bottomright",
        pal = pal,
        values = ~activity_count,
        title = "Login Activity",
        opacity = 0.9,
        labFormat = labelFormat(prefix = "  "),
        className = "info legend"
      ) %>%
      # Add some basic styling
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('zoomend', function() {
          var currentZoom = map.getZoom();
          var circles = document.querySelectorAll('.leaflet-interactive');
          circles.forEach(function(circle) {
            circle.style.transition = 'all 0.3s ease-in-out';
          });
        });
      }
    ")
  })

  # Users Management
  users <- reactiveVal(users_data)
  selected_user <- reactiveVal(NULL)

  # Users table
  output$users_table <- reactable::renderReactable({
    reactable::reactable(
      users(),
      columns = list(
        id = reactable::colDef(show = FALSE),
        email = reactable::colDef(name = "Email"),
        num_apps = reactable::colDef(name = "Number of Apps"),
        roles = reactable::colDef(name = "Roles"),
        created_at = reactable::colDef(
          name = "Created At",
          format = reactable::colFormat(datetime = TRUE)
        ),
        actions = reactable::colDef(
          name = "Actions",
          cell = function(value, index) {
            user_id <- users()$id[index]
            tagList(
              actionButton(
                inputId = paste0("edit_", user_id),
                label = "",
                icon = icon("edit"),
                class = "btn-sm btn-primary",
                onclick = sprintf("Shiny.setInputValue('edit_user_btn', %d, {priority: 'event'})", user_id)
              ),
              actionButton(
                inputId = paste0("delete_", user_id),
                label = "",
                icon = icon("trash"),
                class = "btn-sm btn-danger",
                onclick = sprintf("Shiny.setInputValue('delete_user_btn', %d, {priority: 'event'})", user_id)
              )
            )
          }
        )
      ),
      filterable = TRUE,
      searchable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      theme = reactable::reactableTheme(
        style = list(
          height = "100%"
        )
      ),
      height = "100%"
    )
  })

  # Handle edit button clicks
  observeEvent(input$edit_user_btn, {
    user_id <- input$edit_user_btn
    user_data <- users()
    user_idx <- which(user_data$id == user_id)

    selected_user(user_idx)

    showModal(modalDialog(
      title = "Edit User",
      textInput("edit_email", "Email", value = user_data$email[user_idx]),
      numericInput("edit_num_apps", "Number of Apps", value = user_data$num_apps[user_idx]),
      selectInput("edit_role", "Role",
                  choices = c("Admin", "User", "Developer"),
                  selected = user_data$roles[user_idx]),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_edit", "Save", class = "btn-primary")
      )
    ))
  })

  # Handle save edit
  observeEvent(input$save_edit, {
    req(selected_user())

    current_users <- users()
    idx <- selected_user()

    current_users$email[idx] <- input$edit_email
    current_users$num_apps[idx] <- input$edit_num_apps
    current_users$roles[idx] <- input$edit_role

    users(current_users)
    removeModal()
  })

  # Handle delete button clicks
  observeEvent(input$delete_user_btn, {
    user_id <- input$delete_user_btn
    user_idx <- which(users()$id == user_id)
    selected_user(user_idx)

    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete this user?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })

  # Handle confirm delete
  observeEvent(input$confirm_delete, {
    req(selected_user())

    current_users <- users()
    current_users <- current_users[-selected_user(), ]

    users(current_users)
    removeModal()
  })

  # Handle add user
  observeEvent(input$add_user, {
    showModal(modalDialog(
      title = "Add User",
      textInput("new_email", "Email"),
      numericInput("new_num_apps", "Number of Apps", value = 0),
      selectInput("new_role", "Role", choices = c("Admin", "User", "Developer")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_new", "Save", class = "btn-primary")
      )
    ))
  })

  # Handle save new user
  observeEvent(input$save_new, {
    current_users <- users()

    new_user <- data.frame(
      id = max(current_users$id) + 1,
      email = input$new_email,
      num_apps = input$new_num_apps,
      roles = input$new_role,
      created_at = Sys.time(),
      actions = NA,
      stringsAsFactors = FALSE
    )

    current_users <- rbind(current_users, new_user)
    users(current_users)
    removeModal()
  })
}

shinyApp(ui, server)
