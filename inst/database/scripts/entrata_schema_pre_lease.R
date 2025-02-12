# data --------------------------------------------------------------------
pre_lease_by_property <- entrata_pre_lease_report(summarize_by = "property")
pre_lease_by_unit <- entrata_pre_lease_report(summarize_by = "unit_type")

pre_lease_summary_by_property <- pre_lease_by_property$summary
pre_lease_summary_by_unit <- pre_lease_by_unit$summary

pre_lease_details_by_property <- pre_lease_by_property$details
pre_lease_details_by_unit <- pre_lease_by_unit$details

pre_lease_params_by_property <- pre_lease_by_property$parameters
pre_lease_params_by_unit <- pre_lease_by_unit$parameters

weekly_leasing_data <- entrata_lease_execution_report()

pool <- db_connect()
conn <- pool::poolCheckout(pool)
on.exit(pool::poolReturn(pool, conn))

DBI::dbAppendTable(
  conn,
  DBI::SQL("entrata.pre_lease_weekly"),
  value = weekly_leasing_data |>
    dplyr::select(-property_name, -weekly_total),
  append = TRUE
)

dplyr::copy_to(
  conn,
  df = pre_lease_summary_by_property,
  name = dbplyr::in_schema("entrata", "pre_lease_summary_by_property"),
  temporary = FALSE,
  overwrite = TRUE
)

dplyr::copy_to(
  conn,
  df = pre_lease_summary_by_unit,
  name = dbplyr::in_schema("entrata", "pre_lease_summary_by_unit"),
  temporary = FALSE,
  overwrite = TRUE
)

db_entrata_pre_lease_details <- db_read_tbl(pool, "entrata.pre_lease_details")

# figure out which columns are different between the two details tables (db and entrata)
names(db_entrata_pre_lease_details) |>
  setdiff(names(pre_lease_details_by_property))
# "resident_phone_number" "ledger_name"           "charge_code"

names(pre_lease_details_by_property) |>
  setdiff(names(db_entrata_pre_lease_details))
# "unit_status"    "floorplan_name" "resident_phone"

# merge db data into entrata data, altering db data to match entrata data
pre_lease_details <- pre_lease_details_by_property |>
  dplyr::bind_rows(
    db_entrata_pre_lease_details |>
      dplyr::select(
        resident_phone = resident_phone_number,
        -ledger_name,
        -charge_code
      ) |>
      dplyr::rename(
        unit_status = unit_type,
        floorplan_name = unit_type,
        resident_phone = resident_phone_number
      )
  )

dplyr::copy_to(
  conn,
  df = pre_lease_details_by_property,
  name = dbplyr::in_schema("entrata", "pre_lease_details_by_property"),
  temporary = FALSE,
  overwrite = TRUE
)

# database -----------------------------------------------------------------
db_pre_lease_report_summary <- db_read_tbl(pool, "entrata.pre_lease_report_summary") |>
  dplyr::filter(.data$report_date != Sys.Date())

db_pre_lease_report_details <- db_read_tbl(pool, "entrata.pre_lease_details") |>
  dplyr::filter(.data$report_date != Sys.Date()) |>
  dplyr::transmute(
    report_date = as.Date(report_date),
    property_id = as.integer(property_id),
    property_name = property_name,
    bldg_unit = bldg_unit,
    unit_type = unit_type,
    sqft = as.numeric(sqft),
    resident_id = as.integer(resident_id),
    resident_name = resident_name,
    resident_email = resident_email,
    resident_phone_number = resident_phone_number,
    resident_gender = resident_gender,
    lease_id = lease_id,
    lease_status = lease_status,
    lease_sub_status = lease_sub_status,
    lease_occupancy_type = lease_occupancy_type,
    lease_term_name = lease_term_name,
    lease_term_month = as.integer(lease_term),
    space_option_preferred = space_option_preferred,
    space_option = space_option,
    lease_start_date = as.Date(lease_start),
    lease_end_date = as.Date(lease_end),
    lease_started_on_date = as.Date(lease_started_on),
    lease_partially_completed_on_date = as.Date(lease_partially_completed_on),
    lease_completed_on_date = as.Date(lease_completed_on),
    lease_approved_on_date = as.Date(lease_approved_on),
    move_in_date = as.Date(move_in_date),
    deposit_charged = as.numeric(deposit_charged),
    deposit_held = as.numeric(deposit_held),
    market_rent = as.numeric(market_rent),
    budgeted_rent = as.numeric(budgeted_rent),
    advertised_rate = as.numeric(advertised_rate),
    ledger_name = ledger_name,
    charge_code = charge_code,
    scheduled_rent = as.numeric(scheduled_rent),
    actual_charges = as.numeric(actual_charges),
    scheduled_rent_total = as.numeric(scheduled_rent_total),
    leasing_agent = leasing_agent
  )

current_pre_lease_report_details <- dat$details |>
  dplyr::transmute(
    report_date = Sys.Date(),
    property_id = as.integer(property_id),
    property_name = property_name,
    bldg_unit = bldg_unit,
    unit_type = unit_type,
    sqft = as.numeric(sqft),
    resident_id = as.integer(resident_id),
    resident_name = resident,
    resident_email = email,
    resident_phone_number = phone_number,
    resident_gender = gender,
    lease_id = lease_id_display,
    lease_status = lease_status,
    lease_sub_status = lease_sub_status,
    lease_occupancy_type = lease_occupancy_type,
    lease_term_name = lease_term_name,
    lease_term_month = as.integer(lease_term),
    space_option_preferred = space_option_preferred,
    space_option = space_option,
    lease_start_date = lubridate::mdy(lease_start),
    lease_end_date = lubridate::mdy(lease_end),
    lease_started_on_date = lubridate::mdy(lease_started_on),
    lease_partially_completed_on_date = lubridate::mdy(lease_partially_completed_on),
    lease_completed_on_date = lubridate::mdy(lease_completed_on),
    lease_approved_on_date = lubridate::mdy(lease_approved_on),
    move_in_date = lubridate::mdy(move_in_date),
    deposit_charged = as.numeric(deposit_charged),
    deposit_held = as.numeric(deposit_held),
    market_rent = as.numeric(market_rent),
    budgeted_rent = as.numeric(budgeted_rent),
    advertised_rate = as.numeric(advertised_rate),
    scheduled_rent = as.numeric(scheduled_rent),
    actual_charges = as.numeric(actual_charges),
    scheduled_rent_total = as.numeric(scheduled_rent_total),
    leasing_agent = leasing_agent
  )

db_pre_lease_report_details <- dplyr::bind_rows(db_pre_lease_report_details, current_pre_lease_report_details)

pool::dbAppendTable(
  pool,
  DBI::SQL("entrata.pre_lease_report_details"),
  value = db_pre_lease_report_details
)

conn <- pool::poolCheckout(pool)

summary <- dat$summary |>
  dplyr::mutate(
    report_date = Sys.Date(),
    property_id = as.integer(property_id)
  ) |>
  dplyr::select(
    report_date,
    property_id,
    property_name,
    unit_type,
    avg_sqft,
    avg_advertised_rate,
    total_units = units,
    available_count,
    occupied_count,
    excluded_unit_count,
    rentable_unit_count,
    avg_scheduled_rent,
    dplyr::starts_with("started_"),
    dplyr::starts_with("partially_completed_"),
    dplyr::starts_with("completed_"),
    dplyr::starts_with("approved_"),
    dplyr::starts_with("preleased_"),
    variance
  )

dplyr::copy_to(conn, df = summary, name = dbplyr::in_schema("entrata", "pre_lease_report_summary"), temporary = FALSE, overwrite = TRUE)

dplyr::copy_to(
  conn,
  df = entrata_pre_lease_details_by_property,
  name = dbplyr::in_schema("entrata", "pre_lease_report_details_by_property"),
  temporary = FALSE,
  overwrite = TRUE
)
