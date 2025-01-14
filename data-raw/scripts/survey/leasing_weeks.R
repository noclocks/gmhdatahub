
#  ------------------------------------------------------------------------
#
# Title : Survey Leasing Weeks
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

# derive tibble with leasing weeks
# leasing season starts on 9/1 and ends on 8/31

# start_dates <- seq.Date(from = as.Date("2023-09-01"), to = as.Date("2026-08-31"), by = "week") |>
#   get_leasing_week_start_date()
#
# end_dates <- seq.Date(from = as.Date("2023-09-01"), to = as.Date("2026-08-31"), by = "week") |>
#   get_leasing_week_end_date()
#
# leasing_weeks <- tibble(
#   start_date = start_dates,
#   end_date = end_dates,
#
#   fiscal_week = lubridate::week(start_dates),
#   fiscal_year = lubridate::year(start_dates),
#   fiscal_quarter = lubridate::quarter(start_dates),
#   fiscal_month = lubridate::month(start_dates)
# )
#
# dates <- seq.Date(from = as.Date("2023-09-01"), to = as.Date("2026-08-31"), by = "day")
# leasing_week_start_dates <- get_leasing_week_start_date(dates)
# leasing_week_end_dates <- get_leasing_week_end_date(dates)
# leasing_week_numbers <- get_leasing_week_number(dates)
# weeks_left_to_lease <- get_weeks_left_to_lease(dates)
# leasing_season_years
#
#
#
# create_leasing_calendar <- function(start_year = 2023, num_years = 3) {
#
#   leasing_season_end <- function(year) {
#     lubridate::ymd(paste0(year, "-08-01"))
#   }
#
#   leasing_season_start <- function(year) {
#     lubridate::ymd(paste0(year - 1, "-09-01"))
#   }
#
#   date_seq <- tibble::tibble(
#     date = seq.Date(
#       from = leasing_season_start(start_year),
#       to = leasing_season_end(start_year + num_years),
#       by = "day"
#     )
#   ) |>
#     dplyr::mutate(
#       calendar_year = lubridate::year(date),
#       leasing_year = dplyr::if_else(
#         lubridate::month(date) >= 9,
#         calendar_year + 1,
#         calendar_year
#       ),
#       leasing_season_end_date = leasing_season_end(leasing_year),
#       leasing_season_start_date = leasing_season_start(leasing_year),
#       leasing_week = floor(
#         as.numeric(
#           difftime(date, leasing_season_start_date, units = "weeks")
#         )
#       ) + 1,
#       weeks_left_to_lease = ceiling(
#         as.numeric(
#           difftime(leasing_season_end_date, date, units = "weeks")
#         )
#       ),
#       is_current_leasing_season = date >= leasing_season_start_date &
#         date <= leasing_season_end_date
#     )
#
#   return(date_seq)
# }
