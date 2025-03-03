#  ------------------------------------------------------------------------
#
# Title : SQL Utilities
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

generate_sql_tbl_ddl <- function(df, table_name, schema = "public") {

  # Map R types to PostgreSQL types
  pg_type_map <- list(
    "numeric" = "NUMERIC",
    "integer" = "INTEGER",
    "character" = "TEXT",
    "logical" = "BOOLEAN",
    "Date" = "DATE",
    "POSIXct" = "TIMESTAMP WITH TIME ZONE",
    "factor" = "TEXT"
  )

  # Get column definitions
  col_defs <- purrr::map_chr(df, function(col) {
    base_type <- class(col)[1]
    sql_type <- pg_type_map[[base_type]] %||% "TEXT"

    # Add NOT NULL if no NA values
    if (!any(is.na(col))) {
      sql_type <- paste(sql_type, "NOT NULL")
    }

    return(sql_type)
  })

  # Create column definition strings
  col_strings <- paste(
    names(col_defs),
    col_defs,
    collapse = ",\n  "
  )

  # Build complete DDL
  ddl <- sprintf(
    "CREATE TABLE %s.%s (\n  %s\n);",
    schema,
    table_name,
    col_strings
  )

  # Add helpful indices
  date_cols <- names(df)[map_lgl(df, ~ inherits(., c("Date", "POSIXct")))]
  if (length(date_cols) > 0) {
    index_statements <- map_chr(date_cols, function(col) {
      sprintf(
        "CREATE INDEX idx_%s_%s ON %s.%s(%s);",
        table_name,
        col,
        schema,
        table_name,
        col
      )
    })
    ddl <- paste(c(ddl, index_statements), collapse = "\n\n")
  }

  return(ddl)
}
