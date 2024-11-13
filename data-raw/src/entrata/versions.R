
#  ------------------------------------------------------------------------
#
# Title : Entrata Endpoint Method Versions
#    By : Jimmy Briggs
#  Date : 2024-11-10
#
#  ------------------------------------------------------------------------

# all endpoint method versions are "r1" except for the following:

entrata_r2_methods <- c(
  "getLeases",
  "getLeaseDetails",
  "getReportInfo",
  "getDependentFilter"
)

entrata_r3_methods <- c(
  "getReportData"
)


# merge -------------------------------------------------------------------

if (!exists("entrata_methods")) { source("data-raw/src/entrata/methods.R") }

# entrata_methods is a nested list where the first level object names are
# the endpoints and they contain a nested list of methods for that endpoint.
# we want to add the version to each method in each endpoint using the
# above lists (r1 by default, r2 for the methods in entrata_r2_methods, etc.)

entrata_method_versions <- purrr::map2(
  names(entrata_methods),
  entrata_methods,
  function(endpoint, endpoint_methods) {
    endpoint_methods <- purrr::map(endpoint_methods, function(method) {
      if (method %in% entrata_r2_methods) {
        version <- "r2"
      } else if (method %in% entrata_r3_methods) {
        version <- "r3"
      } else {
        version <- "r1"
      }
      return(list(method = method, version = version))
    })
    return(endpoint_methods)
  }
)

names(entrata_method_versions) <- names(entrata_methods)
