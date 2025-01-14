source("data-raw/scripts/gmh.R")

pool <- db_connect()

db_init_tbl <- function(pool, tbl_name, tbl_data, sql_file = NULL) {

  # create table
  db_run_sql(sql_file, pool = pool)

  # seed table
  db_seed_tbl(pool, tbl_name = tbl_name, tbl_data = tbl_data)

  # read back
  hold <- db_read_tbl(pool, tbl_name = tbl_name)
  rm_cols <- c("created_at", "updated_at", "created_by", "updated_by", "modified_at", "modified_by")
  hold <- hold |> dplyr::select(-tidyselect::any_of(rm_cols))

  # compare
  waldo::compare(hold, tbl_data)

  return(invisible(tbl_data))

}

db_init_tbl(pool, "gmh.segments", gmh_segments_tbl, "inst/database/schemas/gmh/tables/gmh.segments.sql")
db_init_tbl(pool, "gmh.portfolios", gmh_portfolios_tbl, "inst/database/schemas/gmh/tables/gmh.portfolios.sql")
db_init_tbl(pool, "gmh.partners", gmh_partners_tbl, "inst/database/schemas/gmh/tables/gmh.partners.sql")
db_init_tbl(pool, "gmh.properties", gmh_properties_tbl, "inst/database/schemas/gmh/tables/gmh.properties.sql")
db_init_tbl(pool, "gmh.competitors", gmh_competitors_tbl, "inst/database/schemas/gmh/tables/gmh.competitors.sql")

# create tables
db_run_sql("inst/database/schemas/gmh/tables/gmh.segments.sql", pool = pool)
# seed tables
db_seed_tbl(pool, tbl_name = "gmh.segments", tbl_data = gmh_segments_tbl)
# read back
db_gmh_segments <- db_read_tbl(pool, tbl_name = "gmh.segments") |>
  dplyr::select(-created_at)
# compare
waldo::compare(db_gmh_segments, gmh_segments_tbl)


# leasing calendar --------------------------------------------------------

create_leasing_calendar <- function(start_year = 2023, num_years = 3) {
  # Helper functions for season dates
  leasing_season_end <- function(year) ymd(paste0(year, "-08-01"))
  leasing_season_start <- function(year) ymd(paste0(year - 1, "-09-01"))
  pre_lease_season_start <- function(year) ymd(paste0(year - 1, "-09-01"))

  # Generate date sequence
  date_seq <- tibble(
    date = seq.Date(
      from = leasing_season_start(start_year),
      to = leasing_season_end(start_year + num_years),
      by = "day"
    )
  ) |>
    mutate(
      date_key = as.integer(format(date, "%Y%m%d")),
      calendar_year = year(date),
      leasing_year = if_else(month(date) >= 9, calendar_year + 1, calendar_year),
      pre_lease_year = leasing_year,
      fiscal_year = if_else(month(date) >= 7, calendar_year + 1, calendar_year),
      academic_year = paste0(leasing_year - 1, "-", substr(leasing_year, 3, 4)),

      # Week calculations
      calendar_week_number = isoweek(date),
      leasing_season_start_date = leasing_season_start(leasing_year),
      leasing_season_end_date = leasing_season_end(leasing_year),
      pre_lease_season_start_date = pre_lease_season_start(leasing_year),

      # Week start/end dates
      leasing_week_start_date = floor_date(date, unit = "week", week_start = 1),
      leasing_week_end_date = ceiling_date(date, unit = "week", week_start = 1) - days(1),
      weekly_period_start_date = date - days(6),
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
      utc_date = format(with_tz(as_datetime(date), "UTC"), "%Y-%m-%dT%H:%M:%SZ"),

      # Additional flags
      is_current_leasing_season = date >= leasing_season_start_date &
        date <= leasing_season_end_date,
      is_weekend = weekdays(date) %in% c("Saturday", "Sunday"),
      is_holiday = FALSE,
      day_of_week = weekdays(date),
      day_of_month = day(date),
      day_of_year = yday(date),
      month_of_year = month(date, label = TRUE),
      quarter_of_year = quarter(date)
    )

  return(date_seq)
}



leasing_calendar_tbl <- create_leasing_calendar(start_year = 2023, num_years = 3) |>
  select(-date_key) |>
  dplyr::rename(date_key = date)

generate_table_ddl(leasing_calendar_tbl, "leasing_calendar", "gmh") |>
  cat(sep = "\n", file = "inst/database/schemas/gmh/tables/gmh.leasing_calendar.sql", append = TRUE)

db_run_sql("inst/database/schemas/gmh/tables/gmh.leasing_calendar.sql", pool = pool)

db_seed_tbl(pool, tbl_name = "gmh.leasing_calendar", tbl_data = leasing_calendar_tbl)
