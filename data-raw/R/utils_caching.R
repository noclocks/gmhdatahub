
#  ------------------------------------------------------------------------
#
# Title : Caching Utilities
#    By : Jimmy Briggs
#  Date : 2025-03-21
#
#  ------------------------------------------------------------------------

setup_cache <- function(..., max_age = NULL) {

  cache_dir <- file.path("data-raw", "cache", ...)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    cli::cli_alert_success("Cache directory created at {.path {cache_dir}}.")
  }

  if (!is.null(max_age)) {
    cache <- cachem::cache_disk(cache_dir, max_age = max_age)
  } else {
    cache <- memoise::cache_filesystem(cache_dir)
  }

  return(cache)

}
