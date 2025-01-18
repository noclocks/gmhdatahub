#  ------------------------------------------------------------------------
#
# Title : Date Utilities
#    By : Jimmy Briggs
#  Date : 2024-12-01
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Date-Related Utilities
#'
#' @name utils_dates
#' @family Date Utilities
#'
#' @description
#' A collection of date-related utility functions used throughout the
#' GMH Data Hub application. These functions handle various aspects of
#' date manipulation, including leasing weeks, pre-lease seasons, and
#' general date formatting.
#'
#' @details
#' Dates are of particular importance in the GMH Data Hub application,
#' as they are used to calculate leasing weeks, pre-lease seasons, and
#' other time-based metrics. These functions help ensure consistency
#' in date calculations and formatting throughout the application.
#'
#' The functions are grouped into several categories:
#'
#' - **Leasing Week**: Functions related to calculating the start and end
#'   dates of a leasing week.
#' - **Pre-Lease Season**: Functions for determining the start and end dates
#'   of the pre-lease season.
#' - **Leasing Period**: Functions for calculating the start and end dates
#'   of the leasing period.
#' - **Weeks Left to Lease**: Determine the number of weeks left to lease
#'   until the start of the next leasing period.
#' - **Weekly Period**: Functions for working with weekly periods.
#' - **Date Formatting and Coercion**: Functions for formatting and coercing
#'   date strings to Date objects.
#'
#' @section Key Date Concepts:
#'
#' - **Leasing Week**: A period starting on a specific day of the week,
#'   depending on the "reporting cycle" used for leasing-related calculations,
#'   and ending on the day before the next leasing week starts.
#'
#' - **Pre-lease Season**: The period during which leasing for the upcoming
#'   academic year typically occurs. For GMH, this is generally September 1st of
#'   the current year through July 31st of the following year.
#'
#' - **Leasing Period**: The full duration of a lease, often spanning an
#'   academic year (August 1st through July 31st).
#'
#' - **Weekly Period**: A period of seven days, typically used for reporting
#'   purposes.
#'
#' - **Entrata Date Format**: The date format used by Entrata, a property
#'   management software used by GMH.
#'
#' @section Functions:
#'
#' - `get_leasing_week()`: Returns the start and end dates of the leasing week.
#' - `get_leasing_week_start_date()`: Returns the start date of the leasing week.
#' - `get_leasing_week_end_date()`: Returns the end date of the leasing week.
#' - `get_leasing_week_number()`: Returns the week number of the leasing week.
#'
#' - `get_pre_lease_year()`: Returns the year of the pre-lease season.
#' - `get_pre_lease_season()`: Returns the start and end dates of the pre-lease season.
#' - `get_pre_lease_season_start_date()`: Returns the start date of the pre-lease season.
#'
#' - `get_leasing_period()`: Returns the start and end dates of the leasing period.
#' - `get_leasing_period_start_date()`: Returns the start date of the leasing period.
#' - `get_leasing_period_end_date()`: Returns the end date of the leasing period.
#'
#' - `get_weeks_left_to_lease()`: Returns the number of weeks left in the pre-lease season.
#'
#' - `get_weekly_period()`: Returns the start and end dates of the weekly period.
#' - `get_weekly_period_start_date()`: Returns the start date of the weekly period.
#'
#' - `coerce_date()`: Coerces a date string to a Date object.
#' - `entrata_date()`: Formats a Date object in Entrata's date format.
#' - `http_date()`: Creates an HTTP date following the RFC 1123 format.
#'
#' @seealso
#' [leasing_week], [pre_lease_season], [leasing_period], [weeks_left_to_lease],
#' [weekly_period], [date_formatting], [month_dates]
NULL


# leasing calendar --------------------------------------------------------

#' Create a Leasing Calendar Schedule Table
#'
#' @description
#' This function generates a leasing calendar schedule table for a given
#' start year and number of years. The leasing season is defined as starting
#' on September 1st and ending on August 1st of the following year.
#'
#' The table includes the following columns:
#'
#' - `date`: The date in the sequence.
#' - `calendar_year`: The calendar year of the date.
#' - `leasing_year`: The leasing year corresponding to the date.
#' - `leasing_season_end_date`: The end date of the leasing season for the leasing year.
#' - `leasing_season_start_date`: The start date of the leasing season for the leasing year.
#' - `leasing_week`: The week number within the leasing season.
#' - `weeks_left_to_lease`: The number of weeks left to lease until the end of the leasing season.
#' - `is_current_leasing_season`: A logical indicating if the date is within the current leasing season.
#'
#' @param start_year The starting year for the leasing calendar.
#'   Defaults to 2023.
#' @param num_years The number of years to generate the leasing calendar for.
#'   Defaults to 3.
#'
#' @returns
#' A tibble containing the leasing calendar schedule.
#'
#' @export
create_leasing_calendar <- function(start_year = 2023, num_years = 3) {

  leasing_season_end <- function(year) {
    lubridate::ymd(paste0(year, "-08-01"))
  }

  leasing_season_start <- function(year) {
    lubridate::ymd(paste0(year - 1, "-09-01"))
  }

  date_seq <- tibble::tibble(
    date = seq.Date(
      from = leasing_season_start(start_year),
      to = leasing_season_end(start_year + num_years),
      by = "day"
    )
  ) |>
    dplyr::mutate(
      calendar_year = lubridate::year(date),
      leasing_year = dplyr::if_else(
        lubridate::month(date) >= 9,
        calendar_year + 1,
        calendar_year
      ),
      leasing_season_end_date = leasing_season_end(leasing_year),
      leasing_season_start_date = leasing_season_start(leasing_year),
      leasing_week = floor(
        as.numeric(
          difftime(date, leasing_season_start_date, units = "weeks")
        )
      ) + 1,
      weeks_left_to_lease = ceiling(
        as.numeric(
          difftime(leasing_season_end_date, date, units = "weeks")
        )
      ),
      is_current_leasing_season = date >= leasing_season_start_date &
        date <= leasing_season_end_date
    )

  return(date_seq)
}

# leasing week ------------------------------------------------------------

#' Leasing Week
#'
#' @name leasing_week
#' @family Date Utilities
#'
#' @description
#' Calculates the start and end dates of a leasing week for a given "as of"
#' date. The leasing week is defined as starting on a specific day of the week
#' (e.g., Monday, Sunday, etc.). Lubridate supports multiple ways of specifying
#' `week_start`.
#'
#' The following functions are defined:
#'
#' - `get_leasing_week()`: Returns both the start and end dates of the leasing week.
#' - `get_leasing_week_start_date()`: Returns the start date of the leasing week.
#' - `get_leasing_week_end_date()`: Returns the end date of the leasing week.
#' - `get_leasing_week_number()`: Returns the week number of the leasing week.
#'
#' @inheritParams .shared-params
#' @inheritParams lubridate::ceiling_date
#'
#' @returns
#' - `get_leasing_week()`: A list containing the start and end dates of the leasing week.
#' - `get_leasing_week_start_date()`: The start date of the leasing week.
#' - `get_leasing_week_end_date()`: The end date of the leasing week.
#' - `get_leasing_week_number()`: The week number of the leasing week.
#'
#' @examples
#' get_leasing_week()
#' get_leasing_week_start_date("2025-01-15", "Monday")
#' get_leasing_week_end_date(Sys.Date(), "Sunday")
NULL

#' @rdname leasing_week
#' @export
get_leasing_week <- function(
    as_of_date = Sys.Date(),
    week_start = "Monday"
) {
  as_of_date <- coerce_date(as_of_date)
  start <- get_leasing_week_start_date(as_of_date, week_start)
  end <- get_leasing_week_end_date(as_of_date, week_start)
  list(start = start, end = end)
}

#' @rdname leasing_week
#' @export
#' @importFrom lubridate floor_date
get_leasing_week_start_date <- function(
    as_of_date = Sys.Date(),
    week_start = "Monday"
) {
  as_of_date <- coerce_date(as_of_date)
  lubridate::floor_date(as_of_date, unit = "week", week_start = week_start)
}

#' @rdname leasing_week
#' @export
#' @importFrom lubridate ceiling_date
get_leasing_week_end_date <- function(
    as_of_date = Sys.Date(),
    week_start = "Monday"
) {
  as_of_date <- coerce_date(as_of_date)
  lubridate::ceiling_date(as_of_date, unit = "week", week_start = week_start) - 1
}

#' @rdname leasing_week
#' @export
get_leasing_week_number <- function(as_of_date = Sys.Date()) {
  date <- coerce_date(as_of_date)

  leasing_year <- ifelse(
    format(date, "%m-%d") >= "08-01",
    as.numeric(format(date, "%Y")),
    as.numeric(format(date, "%Y")) - 1
  )

  year_start <- as.Date(paste0(leasing_year, "-08-01"))
  week_num <- as.numeric(difftime(date, year_start, units = "weeks")) + 1
  ceiling(week_num)
}

# pre-lease ---------------------------------------------------------------

#' Pre-Lease Season and Leasing Cycle
#'
#' @name pre_lease_season
#' @family Date Utilities
#'
#' @description
#' These functions handle calculations related to the pre-lease season and the overall leasing cycle
#' for student housing. The pre-lease season is the period during which leasing for the upcoming
#' academic year typically occurs.
#'
#' The following functions are defined:
#'
#' - `get_pre_lease_year()`: Returns the year of the upcoming pre-lease season.
#' - `get_pre_lease_season()`: Returns the start and end dates of the upcoming pre-lease season.
#' - `get_pre_lease_season_start_date()`: Returns the start date of the upcoming pre-lease season.
#' - `get_leasing_period_end_date()`: Returns the end date of the current leasing period.
#'
#' @details
#' The pre-lease season starts on September 1st and ends on July 31st of the following year.
#' The actual leasing period begins on August 1st.
#'
#' If the current date is before September 1st, the functions will return dates for the upcoming
#' pre-lease season. If it's September 1st or later, they'll return dates for the pre-lease season
#' that has just begun.
#'
#' This approach aligns with typical academic calendars, where new leases often begin around
#' the start of the fall semester (August 1st), and pre-leasing for the next year starts shortly after.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' - `get_pre_lease_year()`: The year when the upcoming pre-lease season starts.
#' - `get_pre_lease_season()`: A list containing the start and end dates of the upcoming pre-lease season.
#' - `get_pre_lease_season_start_date()`: The start date of the upcoming pre-lease season.
#' - `get_leasing_period_end_date()`: The end date of the current leasing period.
#'
#' @examples
#' get_pre_lease_year()
#' get_pre_lease_season()
#' get_pre_lease_season_start_date()
#' get_leasing_period_end_date()
NULL

#' @rdname pre_lease_season
#' @export
#' @importFrom lubridate year month
get_pre_lease_year <- function(as_of_date = Sys.Date()) {
  curr_month <- lubridate::month(as_of_date)
  curr_year <- lubridate::year(as_of_date)
  if (curr_month < 9) curr_year else curr_year + 1
}

#' @rdname pre_lease_season
#' @export
#' @importFrom lubridate make_date year
get_pre_lease_season <- function(as_of_date = Sys.Date()) {
  start <- get_pre_lease_season_start_date(as_of_date)
  end <- lubridate::make_date(
    year = lubridate::year(start) + 1,
    month = 7,
    day = 31
  )
  list(start = start, end = end)
}

#' @rdname pre_lease_season
#' @export
#' @importFrom lubridate make_date
get_pre_lease_season_start_date <- function(as_of_date = Sys.Date()) {
  pre_lease_year <- get_pre_lease_year(as_of_date)
  lubridate::make_date(year = pre_lease_year, month = 9L, day = 1L)
}

#' @rdname pre_lease_season
#' @export
#' @importFrom lubridate make_date
get_entrata_custom_pre_lease_date <- function(as_of_date = Sys.Date()) {
  pre_lease_year <- get_pre_lease_year(as_of_date)
  lubridate::make_date(year = pre_lease_year, month = 8L, day = 20L)
}

# leasing period ----------------------------------------------------------

#' Leasing Period
#'
#' @name leasing_period
#' @family Date Utilities
#'
#' @description
#' These functions handle calculations related to the leasing period for student housing.
#' The leasing period typically spans an academic year, starting on August 1st
#' and ending on July 31st of the following year.
#'
#' The following functions are defined:
#'
#' - `get_leasing_period()`: Returns the start and end dates of the current leasing period.
#' - `get_leasing_period_start_date()`: Returns the start date of the current leasing period.
#' - `get_leasing_period_end_date()`: Returns the end date of the current leasing period.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' - `get_leasing_period()`: A list containing the start and end dates of the current leasing period.
#' - `get_leasing_period_start_date()`: The start date of the current leasing period.
#' - `get_leasing_period_end_date()`: The end date of the current leasing period.
#'
#' @examples
#' get_leasing_period()
#' get_leasing_period_start_date()
#' get_leasing_period_end_date()
#'
#' @seealso
#' [pre_lease_season], [date_formatting]
NULL

#' @rdname leasing_period
#' @export
get_leasing_period <- function(as_of_date = Sys.Date()) {
  start <- get_leasing_period_start_date(as_of_date)
  end <- get_leasing_period_end_date(as_of_date)
  list(start = start, end = end)
}

#' @rdname leasing_period
#' @export
#' @importFrom lubridate year month make_date
get_leasing_period_start_date <- function(as_of_date = Sys.Date()) {
  yr <- lubridate::year(as_of_date)
  if (lubridate::month(as_of_date) < 8) {
    yr <- yr - 1
  }
  lubridate::make_date(yr, 8, 1)
}

#' @rdname leasing_period
#' @export
#' @importFrom lubridate make_date
get_leasing_period_end_date <- function(as_of_date = Sys.Date()) {
  # End date is the next August 1 relative to the current pre-lease year
  pre_lease_year <- get_pre_lease_year(as_of_date)
  lubridate::make_date(year = pre_lease_year, month = 8L, day = 1L)
}

# weeks left to lease -----------------------------------------------------

#' Weeks Left to Lease
#'
#' @family Date Utilities
#'
#' @description
#' Determine the number of weeks left to lease until the start of the next
#' leasing period. This is calculated based on the current date and the
#' start date of the pre-lease season.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' The number of weeks left to lease until the start of the next leasing period.
#'
#' @export
#'
#' @examples
#' get_weeks_left_to_lease()
#'
#' @seealso
#' [pre_lease_season], [date_formatting]
get_weeks_left_to_lease <- function(as_of_date = Sys.Date()) {
  as_of_date <- coerce_date(as_of_date)
  end_date <- get_pre_lease_season_start_date(as_of_date)
  weeks <- as.numeric(difftime(end_date, as_of_date, units = "weeks"))
  ceiling(weeks)
}

# weekly period -----------------------------------------------------------

#' Weekly Period
#'
#' @name weekly_period
#' @family Date Utilities
#'
#' @description
#' These functions handle calculations related to weekly periods, which are
#' typically used for reporting purposes. A weekly period is defined as a
#' period of seven days.
#'
#' The following functions are defined:
#'
#' - `get_weekly_period()`: Returns the start and end dates of the weekly period.
#' - `get_weekly_period_start_date()`: Returns the start date of the weekly period.
#'
#' There is no need for a function to calculate the end date of the weekly period,
#' as it is simply the "as of" date.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' - `get_weekly_period()`: A list containing the start and end dates of the weekly period.
#' - `get_weekly_period_start_date()`: The start date of the weekly period.
#'
#' @examples
#' get_weekly_period()
#' get_weekly_period_start_date()
#'
#' @seealso
#' [leasing_week], [date_formatting]
NULL

#' @rdname weekly_period
#' @export
get_weekly_period <- function(as_of_date = Sys.Date()) {
  as_of_date <- coerce_date(as_of_date)
  start <- get_weekly_period_start_date(as_of_date)
  end <- as_of_date
  list(start = start, end = end)
}

#' @rdname weekly_period
#' @export
#' @importFrom lubridate days
get_weekly_period_start_date <- function(as_of_date = Sys.Date()) {
  as_of_date <- coerce_date(as_of_date)
  as_of_date - lubridate::days(7)
}

# end/start of month ------------------------------------------------------

#' Get the Start and End Dates for a Month
#'
#' @name month_dates
#' @family Date Utilities
#'
#' @description
#' These functions calculate the start and end dates for a given month.
#'
#' The following functions are defined:
#'
#' - `get_end_of_month()`: Returns the date of the last day of the month.
#' - `get_start_of_month()`: Returns the date of the first day of the month.
#'
#' @inheritParams .shared-params
#'
#' @returns
#' - `get_end_of_month()`: The date of the last day of the month.
#' - `get_start_of_month()`: The date of the first day of the month.
#'
#' @examples
#' get_end_of_month()
#' get_start_of_month()
NULL

#' @name month_dates
#' @export
#' @importFrom lubridate is.Date ceiling_date as_date
get_end_of_month <- function(as_of_date) {
  if (!lubridate::is.Date(as_of_date)) as_of_date <- lubridate::as_date(as_of_date)
  lubridate::ceiling_date(as_of_date, unit = "month") - 1
}

#' @name month_dates
#' @export
#' @importFrom lubridate is.Date floor_date as_date
get_start_of_month <- function(as_of_date) {
  if (!lubridate::is.Date(as_of_date)) as_of_date <- lubridate::as_date(as_of_date)
  as.Date(format(as_of_date, "%Y-%m-01"))
}

# coerce date -------------------------------------------------------------

#' Date Formatting and Coercion
#'
#' @name date_formatting
#' @family Date Utilities
#'
#' @description
#' These functions handle date formatting and coercion to ensure consistency
#' in date representations throughout the application. This is particularly
#' important when working with dates from different sources or in different
#' formats.
#'
#' The following functions are defined:
#'
#' - `coerce_date()`: Coerces a date string to a Date object.
#' - `entrata_date()`: Formats a Date object in Entrata's date format (MM/DD/YYYY).
#' - `parse_entrata_date()`: Parses a date string in Entrata's date format.
#' - `http_date()`: Creates an HTTP date following the RFC 1123 format.
#'
#' @inheritParams .shared-params
#'
#' @seealso
#' [leasing_week], [pre_lease_season]
#'
#' @examples
#' coerce_date("2025-01-15")
#' entrata_date(Sys.Date())
NULL

#' @rdname date_formatting
#' @export
#' @importFrom lubridate is.Date parse_date_time as_date
coerce_date <- function(date) {
  if (!lubridate::is.Date(date)) {
    lubridate::parse_date_time(
      date,
      orders = c("ymd", "mdy", "dmy", "ydm", "myd", "dym")
    ) |>
      lubridate::as_date()
  } else {
    date
  }
}

#' @rdname date_formatting
#' @export
#' @importFrom withr local_timezone
http_date <- function(str) {
  withr::local_timezone("UTC")
  hold <- strptime(str, "%a, %d %b %Y %H:%M:%S", tz = "GMT") |>
    as.POSIXct(tz = "UTC")
  attr(hold, "tzone") <- NULL
  hold
}

utc_date <- function(str) {
  withr::local_timezone("UTC")
  hold <- strptime(str, "%Y-%m-%d %H:%M:%S", tz = "GMT") |>
    as.POSIXct(tz = "UTC")
  attr(hold, "tzone") <- NULL
  hold
}


# entrata -----------------------------------------------------------------


#' @rdname date_formatting
#' @export
entrata_date <- function(date) {
  date <- coerce_date(date)
  format(date, format = "%m/%d/%Y")
}

#' @rdname date_formatting
#' @export
#' @importFrom lubridate parse_date_time as_date
parse_entrata_date <- function(date) {
  lubridate::parse_date_time(date, orders = "mdy") |>
    lubridate::as_date()
}

# calendars ---------------------------------------------------------------

#' Academic Year
#'
#' @description
#' Get the academic year based on the current date. The academic year typically
#' starts in August and ends in July of the following year.
#'
#' This function assumes that if the date is after August 1st, the academic year
#' is the current year to the next year. If the date is before August 1st, the
#' academic year is the previous year to the current year.
get_academic_year <- function(as_of_date = Sys.Date()) {
  curr_month <- lubridate::month(as_of_date)
  curr_year <- lubridate::year(as_of_date)
  if (curr_month < 8) {
    paste0(curr_year - 1, "-", curr_year)
  } else {
    paste0(curr_year, "-", curr_year + 1)
  }
}

#' Fiscal Year
#'
#' @description
#' Get the fiscal year based on the current date. The fiscal year typically
#' starts in July and ends in June of the following year.
#'
#' This function assumes that if the date is after July 1st, the fiscal year
#' is the current year to the next year. If the date is before July 1st, the
#' fiscal year is the previous year to the current year.
get_fiscal_year <- function(as_of_date = Sys.Date()) {
  curr_month <- lubridate::month(as_of_date)
  curr_year <- lubridate::year(as_of_date)
  if (curr_month < 7) {
    paste0(curr_year - 1, "-", curr_year)
  } else {
    paste0(curr_year, "-", curr_year + 1)
  }
}

# elapsed -----------------------------------------------------------------

elapsed_months <- function(start_date, end_date) {
  end <- as.POSIXlt(end_date)
  start <- as.POSIXlt(start_date)
  12 * (end$year - start$year) + (end$mon - start$mon)
}

elapsed_days <- function(start_date, end_date) {
  as.numeric(difftime(end_date, start_date, units = "days"))
}

elapsed_weeks <- function(start_date, end_date) {
  as.numeric(difftime(end_date, start_date, units = "weeks"))
}

elapsed_years <- function(start_date, end_date) {
  end <- as.POSIXlt(end_date)
  start <- as.POSIXlt(start_date)
  end$year - start$year
}

entrata_period <- function(
    period_type,
    period_start_date = NULL,
    period_end_date = NULL,
    period_date = NULL,
    report_date = Sys.Date()
) {

  validate_entrata_period_type(period_type)

  if (period_type == "daterange") {
    if (is.null(period_start_date) || is.null(period_end_date)) {
      cli::cli_abort("Must provide {.arg period_start_date} and {.arg period_end_date} when {.arg period_type} is 'daterange'.")
    }
    if (coerce_date(period_start_date) > coerce_date(period_end_date)) {
      cli::cli_abort("{.arg period_start_date} must be less than or equal to {.arg period_end_date}.")
    }
  }

  if (period_type == "date") {
    if (is.null(period_date)) {
      period_date <- report_date
    }
  }

}

