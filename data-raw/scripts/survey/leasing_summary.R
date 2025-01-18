
#  ------------------------------------------------------------------------
#
# Title : Leasing Summary
#    By : Jimmy Briggs
#  Date : 2025-01-14
#
#  ------------------------------------------------------------------------

leasing_summary_inputs_tbl <- leasing_summary_inputs <- tibble::tibble(
  order = c(1:14),
  name = c(
    "Reporting Cycle",
    "Lease Launch Date",
    "Renewal Launch Date",
    "Current Occupancy",
    "Prior Year Occupancy",
    "Current Pre-Lease",
    "Prior Year Pre-Lease",
    "Total Renewals",
    "Total New Leases",
    "Total Weekly Leases",
    "Total Weekly Traffic",
    "Current Incentive",
    "Incentive Amount",
    "Data Last Updated"
  ),
  id = c(
    "reporting_cycle",
    "lease_launch_date",
    "renewal_launch_date",
    "current_occupancy",
    "prior_year_occupancy",
    "current_pre_lease",
    "prior_year_pre_lease",
    "total_renewals",
    "total_new_leases",
    "total_weekly_leases",
    "total_weekly_traffic",
    "current_incentive",
    "incentive_amount",
    "data_last_updated"
  ),
  type = c(
    "mc",      # Reporting Cycle
    "date",    # Lease Launch Date
    "date",    # Renewal Launch Date
    "numeric", # Current Occupancy
    "numeric", # Prior Year Occupancy
    "numeric", # Current Pre-Lease
    "numeric", # Prior Year Pre-Lease
    "numeric", # Total Renewals
    "numeric", # Total New Leases
    "numeric", # Total Weekly Leases
    "numeric", # Total Weekly Traffic
    "mc",      # Current Incentive
    "numeric", # Incentive Amount
    "date"     # Data Last Updated
  ),
  label = c(
    "Reporting Cycle",
    "Lease Launch Date",
    "Renewal Launch Date",
    "Current Occupancy",
    "Prior Year Occupancy",
    "Current Pre-Lease",
    "Prior Year Pre-Lease",
    "Total Renewals",
    "Total New Leases",
    "Total Weekly Leases",
    "Total Weekly Traffic",
    "Current Incentive",
    "Incentive Amount",
    "Data Last Updated"
  ),
  required = c(
    TRUE,  # Reporting Cycle
    TRUE,  # Lease Launch Date
    TRUE,  # Renewal Launch Date
    TRUE,  # Current Occupancy
    TRUE,  # Prior Year Occupancy
    TRUE,  # Current Pre-Lease
    TRUE,  # Prior Year Pre-Lease
    TRUE,  # Total Renewals
    TRUE,  # Total New Leases
    TRUE,  # Total Weekly Leases
    TRUE,  # Total Weekly Traffic
    TRUE,  # Current Incentive
    TRUE,  # Incentive Amount
    TRUE   # Data Last Updated
  ),
  help = c(
    "The reporting cycle for the current leasing summary.",
    "The date the current leasing summary was launched.",
    "The date the current leasing summary was renewed.",
    "The current occupancy rate for the property.",
    "The prior year occupancy rate for the property.",
    "The current pre-lease rate for the property.",
    "The prior year pre-lease rate for the property.",
    "The total number of renewals for the property.",
    "The total number of new leases for the property.",
    "The total number of weekly leases for the property.",
    "The total number of weekly traffic for the property.",
    "The current incentive for the property.",
    "The amount of the current incentive for the property.",
    "The date the data was last updated."
  ),
  choices = NULL,
  default = c(
    "Monday-Sunday", # Reporting Cycle
    NA,              # Lease Launch Date
    NA,              # Renewal Launch Date
    NA,              # Current Occupancy
    NA,              # Prior Year Occupancy
    NA,              # Current Pre-Lease
    NA,              # Prior Year Pre-Lease
    NA,              # Total Renewals
    NA,              # Total New Leases
    NA,              # Total Weekly Leases
    NA,              # Total Weekly Traffic
    "None",          # Current Incentive
    "0",                # Incentive Amount,
    NA               # Data Last Updated
  ),
  icon = c(
    "calendar", # Reporting Cycle
    "calendar", # Lease Launch Date
    "calendar", # Renewal Launch Date
    "chart-line", # Current Occupancy
    "chart-line", # Prior Year Occupancy
    "chart-line", # Current Pre-Lease
    "chart-line", # Prior Year Pre-Lease
    "chart-line", # Total Renewals
    "chart-line", # Total New Leases
    "chart-line", # Total Weekly Leases
    "chart-line", # Total Weekly Traffic
    "chart-line", # Current Incentive
    "chart-line", # Incentive Amount
    "calendar"   # Data Last Updated
  ),
  placeholder = c(
    "Select a reporting cycle...",
    "Select a lease launch date...",
    "Select a renewal launch date...",
    "Enter the current occupancy rate...",
    "Enter the prior year occupancy rate...",
    "Enter the current pre-lease rate...",
    "Enter the prior year pre-lease rate...",
    "Enter the total number of renewals...",
    "Enter the total number of new leases...",
    "Enter the total number of weekly leases...",
    "Enter the total number of weekly traffic...",
    "Select a current incentive...",
    "Enter the amount of the current incentive...",
    "Select the date the data was last updated..."
  )
) |>
  dplyr::mutate(
    choices = dplyr::case_when(
      id == "current_incentive" ~ 'c("None", "Free Rent", "Reduced Rent", "Other")',
      id == "reporting_cycle" ~ 'c("Saturday-Friday" "Sunday-Monday"   "Monday-Sunday")',
      TRUE ~ NA_character_
    )
  )
