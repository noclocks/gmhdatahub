
sysreqs <- pak::pkg_sysreqs("local::.", sysreqs_platform = "ubuntu")
sysregs_pkgs <- sysreqs$packages$system_packages |> unlist()

sysreqs_install_cmd <- paste0(
  "RUN apt-get update -y -qq && apt-get -y --no-install-recommends install \\\n",
  paste("  ", sysregs_pkgs, collapse = " \\\n"), " \\\n",
  "  && apt-get clean \\\n",
  "  && rm -rf /var/lib/apt/lists/* \\\n",
  "  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds"
)
