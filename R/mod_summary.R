mod_summary_ui <- function(id) {

}

mod_summary_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      summary_data <- shiny::reactive({
        #empty_summary_table()
        demo_summary_tbl()
      })

      output$summary_table <- DT::renderDT({
        shiny::req(summary_data())

        dat <- split_summary_table(summary_data())

        DT::datatable(
          data = dat$tbl_1,
          options = list(
            dom = 't',                  # Hide pagination/search bar
            paging = FALSE,             # Display full table without pagination
            ordering = FALSE,           # Disable column sorting
            scrollX = TRUE,             # Enable horizontal scrolling
            autoWidth = TRUE,           # Adjust column widths
            columnDefs = list(
              list(targets = 0, visible = FALSE),  # Hide `property_id`
              list(className = "highlight", targets = "_all")  # Apply highlight to all cells
            )
          ),
          rownames = FALSE
        ) |>
          # Custom cell formatting for specific columns
          DT::formatStyle(
            columns = c("prelease_percent", "prior_prelease_percent"),
            backgroundColor = DT::styleInterval(90, c("lightcoral", "lightgreen")),
            fontWeight = 'bold'
          ) |>
          DT::formatStyle(
            columns = "yoy_variance_2",
            color = DT::styleInterval(0, c("red", "green")),
            fontWeight = 'bold'
          )
      })
    })
}


# utils -------------------------------------------------------------------

empty_summary_table <- function() {
  tibble::tibble(
    property_id = integer(),
    property_name = character(),
    leases_count = integer(),
    total_beds = integer(),
    model_beds = integer(),
    current_occupied = integer(),
    current_occupency = double(),
    total_new = integer(),
    total_renewals = integer(),
    total_leases = integer(),
    prelease_percent = double(),
    prior_total_new = integer(),
    prior_total_renewals = integer(),
    prior_total_leases = integer(),
    prior_prelease_percent = double(),
    yoy_variance_1 = integer(),
    yoy_variance_2 = double(),
    weekly_new_leases = integer(),
    weekly_new_renewals = integer(),
    weekly_new_total = integer(),
    weekly_new_pct_gained = double(),
    beds_left_to_lease = integer(),
    beds_leased_this_week = integer(),
    weekly_velocity_needed_90 = double(),
    weekly_velocity_needed_95 = double(),
    weekly_velocity_needed_100 = double()
  )
}

demo_summary_tbl <- function() {

  readr::read_csv(
    pkg_sys("extdata", "csv", "summary.csv")
  )

}

prep_summary_table_data <- function(tbl) {
  tbl |>
    dplyr::transmute(
      property_id = as.integer(property_id),
      property_name = as.character(property_name),
      # Leasing summary section
      leases_count = as.integer(units),                # Assuming 'units' is total leased units
      total_beds = as.integer(rentable_unit_count),    # Total beds
      model_beds = as.integer(rentable_unit_count),    # Model beds, assuming similar to rentable units
      current_occupied = as.integer(preleased_count),  # Occupied units count
      current_occupency = as.double(preleased_percent),# Occupancy percentage
      # Prelease activity
      total_new = as.integer(preleased_new_count),     # New preleases count
      total_renewals = as.integer(preleased_renewal_count),  # Renewal preleases count
      total_leases = as.integer(preleased_new_count + preleased_renewal_count), # Calculated
      prelease_percent = as.double(preleased_percent), # Prelease percent
      # Prior year same store
      prior_total_new = as.integer(preleased_count_prior),  # Prior new preleases
      prior_total_renewals = as.integer(preleased_renewal_count_prior), # Prior renewals
      prior_total_leases = as.integer(preleased_count_prior + preleased_renewal_count_prior), # Calculated
      prior_prelease_percent = as.double(preleased_percent_prior), # Prior prelease percent
      # Year-over-Year variance
      yoy_variance_1 = as.integer(variance),  # Count change
      yoy_variance_2 = as.double(preleased_percent - preleased_percent_prior), # Percent change calculated
      # Preleasing activity - prior seven days
      weekly_new_leases = as.integer(0),      # Placeholder, as no direct data was available
      weekly_new_renewals = as.integer(0),    # Placeholder
      weekly_new_total = as.integer(0),       # Placeholder
      weekly_new_pct_gained = as.double(0),   # Placeholder
      # Weekly velocity needed
      beds_left_to_lease = as.integer(available_count), # Beds available for leasing
      beds_leased_this_week = as.integer(0),            # Placeholder for leased this week
      # Calculated weekly velocities
      weekly_velocity_needed_90 = (total_beds * 0.90 - current_occupied) / weeks_remaining,
      weekly_velocity_needed_95 = (total_beds * 0.95 - current_occupied) / weeks_remaining,
      weekly_velocity_needed_100 = (total_beds * 1.00 - current_occupied) / weeks_remaining
    ) |>
    dplyr::mutate(
      weekly_velocity_needed_90 = ifelse(weekly_velocity_needed_90 < 0, 0, weekly_velocity_needed_90),
      weekly_velocity_needed_95 = ifelse(weekly_velocity_needed_95 < 0, 0, weekly_velocity_needed_95),
      weekly_velocity_needed_100 = ifelse(weekly_velocity_needed_100 < 0, 0, weekly_velocity_needed_100)
    )
}

split_summary_table <- function(tbl) {

  main_1 <- dplyr::select(
    tbl,
    property_name,
    leases_count,
    total_beds,
    model_beds,
    current_occupied,
    current_occupency
  )

  main_2 <- dplyr::select(
    tbl,
    total_new,
    total_renewals,
    total_leases,
    prelease_percent,
    prior_total_new,
    prior_total_renewals
  )

  prior_year <- dplyr::select(
    tbl,
    prior_new,
    prior_total_leases,
    prior_prelease_percent
  )

  yoy_variance <- dplyr::select(
    tbl,
    prior_total_leases,
    prior_prelease_percent,
    yoy_variance_1,
    yoy_variance_2
  )

  prior_seven_days <- dplyr::select(
    tbl,
    weekly_new_leases,
    weekly_new_renewals,
    weekly_new_total,
    weekly_new_pct_gained
  )

  weekly_velocity <- dplyr::select(
    tbl,
    beds_left_to_lease,
    beds_leased_this_week,
    weekly_velocity_needed_90,
    weekly_velocity_needed_95,
    weekly_velocity_needed_100
  )

  return(list(tbl_1 = tbl_1, tbl_2 = tbl_2, tbl_3 = tbl_3))

}

get_pre_lease_period_start_date <- function() {
  today <- lubridate::today()
  if (month(today) >= 9) {
    make_date(year(today) + 1, 9, 1)
  } else {
    make_date(year(today), 9, 1)
  } %>%
    format("%m/%d/%Y")
}

get_leasing_season_end_date <- function(as_of = lubridate::today()) {

  requireNamespace("lubridate")

  dplyr::case_when(
    lubridate::month(as_of) >= 9 ~ lubridate::make_date(lubridate::year(as_of) + 1, 8, 1),
    lubridate::month(as_of) < 9 ~ lubridate::make_date(lubridate::year(as_of), 8, 1)
  )

}

get_weeks_left_to_lease <- function(leasing_season_end_date = get_leasing_season_end_date()) {

  leasing_season_end_date %--% lubridate::today() |>
    lubridate::as.duration() |>
    as.numeric("weeks") |>
    floor() * -1

}

