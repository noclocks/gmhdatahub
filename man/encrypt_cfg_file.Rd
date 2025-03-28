% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_config.R
\name{encrypt_cfg_file}
\alias{encrypt_cfg_file}
\title{Encrypt Configuration File}
\usage{
encrypt_cfg_file(
  cfg_file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
  key = "NOCLOCKS_ENCRYPTION_KEY"
)
}
\arguments{
\item{cfg_file}{The path to the configuration file to encrypt.}

\item{key}{The name of the environment variable containing the encryption key.}
}
\value{
The configuration values, silently.
}
\description{
Encrypts the configuration file using the provided encryption key.
}
