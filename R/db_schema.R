db_schema_tbl_name <- function(schema = NULL, tbl_name) {

  schemas <- c(
    "public",
    "gmh",
    "entrata",
    "auth",
    "mkt",
    "survey",
    "app",
    "util",
    "ext",
    "archive"
  )

  if (!is.null(schema)) {
    schema <- rlang::arg_match(schema, schemas)
  }

  if (is.null(schema)) {
    schema <- "public"
  }

  DBI::SQL(glue::glue("{schema}.{tbl_name}"))
}
