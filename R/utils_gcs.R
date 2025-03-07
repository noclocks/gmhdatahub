
#' Google Cloud Storage Configuration
#'
#' @description
#' This function configures the Google Cloud Storage (GCS) settings for the package.
#'
#' @param default_bucket The default GCS bucket to use.
#'
#' @export
#'
#' @importFrom googleCloudStorageR gcs_global_bucket
configure_gcs <- function(default_bucket = NULL) {

  gcs_client_json <- Sys.getenv("GAR_CLIENT_JSON", pkg_sys("config/credentials/gmh-communities-oauth-client-secret.json"))
  gcs_auth_file <- Sys.getenv("GCS_AUTH_FILE", pkg_sys("config/credentials/gmh-communities-compute-engine-default-service-account-key.json"))
  if (is.null(default_bucket)) {
    gcs_default_bucket <- Sys.getenv("GCS_DEFAULT_BUCKET", "entrata-pipeline")
  } else {
    gcs_default_bucket <- default_bucket
  }

  Sys.setenv("GAR_CLIENT_JSON" = gcs_client_json)
  Sys.setenv("GCS_AUTH_FILE" = gcs_auth_file)
  Sys.setenv("GCS_DEFAULT_BUCKET" = gcs_default_bucket)

  googleCloudStorageR::gcs_global_bucket(gcs_default_bucket)

}

#' Upload Entrata API Request/Response JSON to GCS
#'
#' @description
#' These functions upload the Entrata API request and response JSON to Google Cloud Storage.
#'
#' - `upload_entrata_request()`: Upload Entrata API request JSON to GCS.
#' - `upload_entrata_response()`: Upload Entrata API response JSON to GCS.
#'
#' @param req The request object to upload.
#' @param resp The response object to upload.
#' @param endpoint The Entrata API endpoint.
#' @param method The Entrata API method name.
#'
#' @returns
#' If successful, a metadata object is returned from [googleCloudStorageR::gcs_upload()].
#'
#' @export
#'
#' @importFrom jsonlite toJSON
#' @importFrom purrr pluck
#' @importFrom withr defer
#' @importFrom glue glue
#' @importFrom googleCloudStorageR gcs_upload
upload_entrata_request <- function(
    req,
    endpoint,
    method
) {

  check_request(req)
  validate_entrata_endpoint(endpoint)
  validate_entrata_method_name(method)

  req_json <- req |> purrr::pluck("body", "data") |> jsonlite::toJSON(auto_unbox = TRUE)
  req_file <- tempfile(fileext = ".json")
  writeLines(req_json, req_file)
  withr::defer(file.remove, req_file)

  date_str <- format(Sys.time(), "%Y%m%d")

  gcs_file <- glue::glue("{endpoint}.{method}.{date_str}.request.json")
  gcs_path <- glue::glue("{endpoint}/{method}/{gcs_file}")

  googleCloudStorageR::gcs_upload(
    file = req_file,
    name = gcs_path,
    predefinedAcl = "default"
  )
}

#' @rdname upload_entrata_request
#' @export
#' @importFrom jsonlite toJSON
#' @importFrom withr defer
#' @importFrom glue glue
#' @importFrom googleCloudStorageR gcs_upload
upload_entrata_response <- function(
    resp,
    endpoint,
    method
) {

  check_response(resp)
  validate_entrata_endpoint(endpoint)
  validate_entrata_method_name(method)

  resp_json <- resp |> jsonlite::toJSON(auto_unbox = TRUE)
  resp_file <- tempfile(fileext = ".json")
  writeLines(resp_json, resp_file)
  withr::defer(file.remove, resp_file)

  date_str <- format(Sys.time(), "%Y%m%d")

  gcs_file <- glue::glue("{endpoint}.{method}.{date_str}.response.json")
  gcs_path <- glue::glue("{endpoint}/{method}/{gcs_file}")

  googleCloudStorageR::gcs_upload(
    file = resp_file,
    name = gcs_path,
    predefinedAcl = "default"
  )
}

#' Upload Entrata API Report Parameters to GCS
#'
#' @description
#' This function uploads the Entrata API report parameters JSON to Google Cloud Storage.
#'
#' @param report_params The report parameters to upload.
#' @param report_name The name of the report.
#'
#' @returns
#' If successful, a metadata object is returned from [googleCloudStorageR::gcs_upload()].
#'
#' @export
#'
#' @importFrom jsonlite toJSON
#' @importFrom withr defer
#' @importFrom glue glue
#' @importFrom googleCloudStorageR gcs_upload
upload_entrata_report_params <- function(report_params, report_name) {

  params_json <- report_params |> jsonlite::toJSON(auto_unbox = TRUE)
  params_file <- tempfile(fileext = ".json")
  writeLines(params_json, params_file)
  withr::defer(file.remove, params_file)

  date_str <- format(Sys.time(), "%Y%m%d")

  gcs_file <- glue::glue("reports.{report_name}.params.{date_str}.json")
  gcs_path <- glue::glue("reports/{report_name}/{gcs_file}")

  googleCloudStorageR::gcs_upload(
    file = params_file,
    name = gcs_path,
    predefinedAcl = "default"
  )
}

#' Upload GMH Property Image to GCS
#'
#' @description
#' This function uploads a GMH property image to Google Cloud Storage.
#'
#' @param file The file path to the image to upload.
#' @param property_id The property ID to associate the image with.
#' @param image_type The type of image to upload. Defaults to "property".
#'
#' @returns
#' If successful, a metadata object is returned from [googleCloudStorageR::gcs_upload()].
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom googleCloudStorageR gcs_upload
upload_property_image <- function(
    file,
    property_id,
    image_type = "property"
) {

  if (!file.exists(file)) {
    cli::cli_alert_danger("File {.path {file}} does not exist.")
    return(NULL)
  }

  gcs_path <- glue::glue("properties/{property_id}/property/{basename(file)}")

  googleCloudStorageR::gcs_upload(
    file = file,
    name = gcs_path,
    predefinedAcl = "default"
  )

}

get_gcs_file_url <- function(file, bucket = "gmh-images") {
  glue::glue("https://storage.googleapis.com/{bucket}/{file}")
}

generate_gcs_signed_url <- function(
    file,
    bucket = "gmh-images",
    expiration = 60 * 60 * 24 * 7
) {
  file_obj <- googleCloudStorageR::gcs_get_object(file, bucket = bucket, meta = TRUE)
  googleCloudStorageR::gcs_signed_url(file_obj, expiration_ts = Sys.time() + expiration)
}

