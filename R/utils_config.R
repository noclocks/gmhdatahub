
#' Encrypt Configuration File
#'
#' @description
#' Encrypts the configuration file using the provided encryption key.
#'
#' @param cfg_file The path to the configuration file to encrypt.
#' @param key The name of the environment variable containing the encryption key.
#'
#' @returns
#' The configuration values, silently.
#'
#' @export
#'
#' @importFrom cli cli_alert_success
#' @importFrom config get
#' @importFrom fs path path_ext_remove file_copy
#' @importFrom httr2 secret_has_key secret_encrypt_file
#' @importFrom cli cli_abort
encrypt_cfg_file <- function(
    cfg_file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
    key = "NOCLOCKS_ENCRYPTION_KEY"
) {

  if (is.null(Sys.getenv(key)) || !httr2::secret_has_key(key)) {
    cli::cli_abort(
      "Encryption key not found. Please set the encryption key environment variable."
    )
  }

  cfg_file <- fs::path(cfg_file)
  cfg_file_encrypted <- fs::path_ext_remove(cfg_file) |>
    paste0(".encrypted.yml") |>
    fs::path()

  fs::file_copy(
    cfg_file,
    cfg_file_encrypted,
    overwrite = TRUE
  )

  httr2::secret_encrypt_file(
    path = cfg_file_encrypted,
    key = key
  )

  cli::cli_alert_success("Successfully encrypted the config file: {.file cfg_file_encrypted}")

  return(config::get())
}


#' Decrypt Configuration File
#'
#' @description
#' Decrypts the configuration file using the provided encryption key.
#'
#' @param path The path to save the decrypted configuration file.
#' @param key The name of the environment variable containing the encryption key.
#' @param set_env Logical indicating if the `R_CONFIG_FILE` environment variable should be set.
#' @param cfg_file The path to the configuration file to decrypt.
#'
#' @returns
#' The configuration values, silently.
#'
#' @export
#'
#' @importFrom cli cli_abort cli_alert_success cli_alert_info
#' @importFrom config get
#' @importFrom fs path path_ext_remove file_exists file_copy
#' @importFrom httr2 secret_has_key secret_decrypt_file
decrypt_cfg_file <- function(
    path = ".",
    key = "NOCLOCKS_ENCRYPTION_KEY",
    set_env = TRUE,
    cfg_file = pkg_sys("config/config.yml")
) {

  if (!httr2::secret_has_key(key)) {
    cli::cli_abort(
      c(
        "Encryption key: {.field {key}} not found.",
        "Please set the encryption key in your environment variables."
      )
    )
  }

  cfg_file_encrypted <- fs::path_ext_remove(cfg_file) |>
    paste0(".encrypted.yml") |>
    fs::path()

  if (!fs::file_exists(cfg_file_encrypted)) {
    cli::cli_abort(
      c(
        "Encrypted config file: {.file {cfg_file_encrypted}} not found.",
        "Please ensure the file exists."
      )
    )
  }

  cfg_file_decrypted <- httr2::secret_decrypt_file(
    path = cfg_file_encrypted,
    key = key
  )

  cli::cli_alert_success("Successfully decrypted the config file: {.file {cfg_file}}")

  cfg_out <- fs::path(path, "config.yml")

  fs::file_copy(
    cfg_file_decrypted,
    cfg_out,
    overwrite = TRUE
  )

  cli::cli_alert_success("Successfully copied the decrypted file to: {.file {cfg_out}}")

  if (!set_env) {
    return(invisible(config::get()))
  }

  Sys.setenv("R_CONFIG_FILE" = cfg_out)
  cli::cli_alert_info("Set `R_CONFIG_FILE` to: {.file {cfg_out}}")

  return(invisible(config::get()))
}
