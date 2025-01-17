
if (!exists("use_template")) {
  source("dev/R/use_template.R")
}

get_pkg_sysreqs <- function() {
  sysreqs <- pak::pkg_sysreqs("local::.", sysreqs_platform = "ubuntu")
  sysreqs$packages$system_packages |> unlist()
}

get_pkg_sysreqs_install_cmd <- function(sysreqs_pkgs) {

  paste0(
    "RUN apt-get update -y -qq && apt-get -y --no-install-recommends install \\\n",
    paste("  ", sysregs_pkgs, collapse = " \\\n"), " \\\n",
    "  && apt-get clean \\\n",
    "  && rm -rf /var/lib/apt/lists/* \\\n",
    "  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds"
  )
}

use_dockerfile <- function() {

  sysreqs <- get_pkg_sysreqs()
  sysreqs_install_cmd <- get_pkg_sysreqs_install_cmd(sysreqs)

  use_template(
    "dev/templates/Dockerfile.template",
    "Dockerfile",
    data = list(sysreqs_install_cmd = sysreqs_install_cmd),
    open = TRUE
  )

}
