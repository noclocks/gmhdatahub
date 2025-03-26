#  ------------------------------------------------------------------------
#
# Title : SQL Utilities
#    By : Jimmy Briggs
#  Date : 2025-01-13
#
#  ------------------------------------------------------------------------

#' Generate SQL Table DDL
#'
#' @description
#' This function generates a SQL table DDL statement based on the input data frame.
#'
#' @param df The data frame to generate the DDL for.
#' @param table_name The name of the table.
#' @param schema The schema to create the table in.
#'
#' @returns
#' A SQL DDL statement.
#'
#' @export
#'
#' @importFrom purrr map_chr map_lgl
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
  date_cols <- names(df)[purrr::map_lgl(df, ~ inherits(., c("Date", "POSIXct")))]
  if (length(date_cols) > 0) {
    index_statements <- purrr::map_chr(date_cols, function(col) {
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
