create_leasing_calendar <- function(start_year = 2023, num_years = 3) {
  # Helper functions for season dates
  leasing_season_end <- function(year) lubridate::ymd(paste0(year, "-08-01"))
  leasing_season_start <- function(year) lubridate::ymd(paste0(year - 1, "-09-01"))
  pre_lease_season_start <- function(year) lubridate::ymd(paste0(year - 1, "-09-01"))

  # Generate date sequence
  date_seq <- dplyr::tibble(
    date = seq.Date(
      from = leasing_season_start(start_year),
      to = leasing_season_end(start_year + num_years),
      by = "day"
    )
  ) |>
    dplyr::mutate(
      date_key = as.integer(format(date, "%Y%m%d")),
      calendar_year = lubridate::year(date),
      leasing_year = dplyr::if_else(lubridate::month(date) >= 9, calendar_year + 1, calendar_year),
      pre_lease_year = leasing_year,
      fiscal_year = dplyr::if_else(lubridate::month(date) >= 7, calendar_year + 1, calendar_year),
      academic_year = paste0(leasing_year - 1, "-", substr(leasing_year, 3, 4)),

      # Week calculations
      calendar_week_number = lubridate::isoweek(date),
      leasing_season_start_date = leasing_season_start(leasing_year),
      leasing_season_end_date = leasing_season_end(leasing_year),
      pre_lease_season_start_date = pre_lease_season_start(leasing_year),

      # Week start/end dates
      leasing_week_start_date = lubridate::floor_date(date, unit = "week", week_start = 1),
      leasing_week_end_date = lubridate::ceiling_date(date, unit = "week", week_start = 1) - lubridate::days(1),
      weekly_period_start_date = date - lubridate::days(6),
      weekly_period_end_date = date,

      # Leasing week calculations
      leasing_week_number = floor(
        as.numeric(
          difftime(date, leasing_season_start_date, units = "weeks")
        )
      ) + 1,
      leasing_weeks_left_to_lease = ceiling(
        as.numeric(
          difftime(leasing_season_end_date, date, units = "weeks")
        )
      ),

      # Date formats
      entrata_formatted_date = format(date, "%m/%d/%Y"),
      http_date = format(date, "%a, %d %b %Y %H:%M:%S GMT"),
      utc_date = format(lubridate::with_tz(lubridate::as_datetime(date), "UTC"), "%Y-%m-%dT%H:%M:%SZ"),

      # Additional flags
      is_current_leasing_season = date >= leasing_season_start_date &
        date <= leasing_season_end_date,
      is_weekend = weekdays(date) %in% c("Saturday", "Sunday"),
      is_holiday = FALSE,
      day_of_week = weekdays(date),
      day_of_month = lubridate::day(date),
      day_of_year = lubridate::yday(date),
      month_of_year = lubridate::month(date, label = TRUE),
      quarter_of_year = lubridate::quarter(date)
    )

  return(date_seq)
}
