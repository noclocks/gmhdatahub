


check_endpoint <- function(
  endpoint,
  arg = rlang::caller_arg(endpoint),
  call = rlang::caller_env()
) {

  if (!exists("entrata_endpoints")) {
    entrata_endpoints <- getFromNamespace("entrata_endpoints", "gmhdatahub")
  }

  if (!endpoint %in% entrata_endpoints) {
    cli::cli_abort(
      c(
        "Invalid endpoint: {.field {endpoint}}",
        "{.arg {arg}} is not a valid Entrata API endpoint.",
        "i" = "Valid endpoints are: {.val {entrata_endpoints}}"
      ),
      call = call
    )
  }
}


