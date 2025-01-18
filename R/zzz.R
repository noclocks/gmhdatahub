#  ------------------------------------------------------------------------
#
# Title : zzz.R - Package .onLoad / .onAttach
#    By : Jimmy Briggs
#  Date : 2024-11-26
#
#  ------------------------------------------------------------------------

# onLoad ------------------------------------------------------------------

.onLoad <- function(libname, pkgname) { # nocov start

  Sys.setenv("R_CONFIG_FILE" = pkg_sys("config/config.yml"))

  shiny::addResourcePath("www", pkg_sys("www"))

  logger::log_threshold(logger::INFO, namespace = "gmhcommunities")

  logger::log_layout(
    logger::layout_glue_generator(
      "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {msg}"
    ),
    namespace = "gmhcommunities"
  )

  logger::log_formatter(
    logger::formatter_glue,
    namespace = "gmhcommunities"
  )

  logger::log_appender(
    appender = logger::appender_stdout,
    namespace = "gmhcommunities"
  )
} # nocov end

.onAttach <- function(libname, pkgname) {
  msg <- "gmhcommunities R package developed by No Clocks, LLC (https://github.com/noclocks/gmhcommunities/)"
  packageStartupMessage(msg)
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
  Sys.unsetenv("R_CONFIG_FILE")
}
