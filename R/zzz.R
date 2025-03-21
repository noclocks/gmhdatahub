#  ------------------------------------------------------------------------
#
# Title : zzz.R - Package .onLoad / .onAttach
#    By : Jimmy Briggs
#  Date : 2024-11-26
#
#  ------------------------------------------------------------------------

# onLoad ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}

.onAttach <- function(libname, pkgname) {
  msg <- "gmhcommunities R package developed by No Clocks, LLC (https://github.com/noclocks/gmhcommunities/)"
  packageStartupMessage(msg)
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
  Sys.unsetenv("R_CONFIG_FILE")
}
