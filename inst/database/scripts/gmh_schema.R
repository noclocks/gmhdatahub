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
