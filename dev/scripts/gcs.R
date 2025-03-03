library(googleCloudStorageR)

pkgload::load_all()

Sys.setenv("GAR_CLIENT_JSON" = pkg_sys("config/credentials/gmh-communities-oauth-client-secret.json"))
Sys.setenv("GCS_AUTH_FILE" = pkg_sys("config/credentials/gmh-communities-compute-engine-default-service-account-key.json"))
Sys.setenv("GCS_DEFAULT_BUCKET" = "gmh-images")

googleCloudStorageR::gcs_setup()

googleCloudStorageR::gcs_global_bucket("gmh-images")

gcs_project <- "gmh-communities"
(buckets <- googleCloudStorageR::gcs_list_buckets(projectId = gcs_project))
(image_bucket <- buckets$name[buckets$name == "gmh-images"])
(bucket_info <- googleCloudStorageR::gcs_get_bucket(image_bucket))
(objects <- googleCloudStorageR::gcs_list_objects(bucket = image_bucket))
(files <- objects |> dplyr::filter(size != "0 bytes"))

properties_files <- files |>
  dplyr::filter(stringr::str_detect(name, "properties/")) |>
  dplyr::mutate(
    property_name = stringr::str_extract(name, "properties/(.*)/") |>
      stringr::str_replace_all("properties/", "") |>
      stringr::str_replace_all("/", "")
  )


properties_name_lookup_tbl <- tibble::tibble(
  gcs_name = properties_files$property_name |> unique(),
  gmh_name = c(
    "Academy 65",
    "",
    "The Academy on Charles",
    "",
    "The Academy at Frisco",
    "Academy Lincoln",
    "",
    "ANOVA uCity Square",
    "The Dean Campustown",
    "",
    "Shortbread Lofts",
    "SOVA",
    "1047 Commonwealth Avenue",
    "Venue at North Campus"
  )
) |>
  dplyr::filter(gmh_name != "") |>
  dplyr::mutate(
    property_id = purrr::map_int(gmh_name, ~get_property_id_by_name(.x))
  )

properties_images <- properties_files |>
  dplyr::filter(property_name %in% properties_name_lookup_tbl$gcs_name) |>
  dplyr::left_join(
    properties_name_lookup_tbl,
    by = c("property_name" = "gcs_name")
  ) |>
  dplyr::transmute(
    entity_type = "property",
    entity_id = property_id,
    image_type = "logo",
    image_url = purrr::map_chr(.data$name, ~get_gcs_file_url(.x)),
    image_description = "Logo for property",
    image_content_type = mime::guess_type(.data$name),
    image_color = dplyr::case_when(
      stringr::str_detect(.data$name, "black") ~ "Black",
      stringr::str_detect(.data$name, "white") ~ "White",
      stringr::str_detect(.data$name, "lightgreen") ~ "Light Green",
      TRUE ~ "Default"
    ),
    image_size = .data$size,
    gcs_bucket = image_bucket,
    gcs_path = .data$name
  ) |>
  dplyr::distinct(
    entity_type,
    entity_id,
    image_type,
    image_content_type,
    image_color,
    .keep_all = TRUE
  )

pool <- db_connect()

pool::dbAppendTable(
  pool,
  DBI::SQL("gmh.images"),
  properties_images
)

get_property_image_url <- function(pool, property_id) {

  check_db_pool(pool)

  db_read_tbl(pool, "gmh.images", collect = FALSE) |>
    dplyr::filter(.data$entity_type == "property", .data$entity_id == .env$property_id) |>
    dplyr::pull(.data$image_url)

}

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

signed_url <- generate_gcs_signed_url(objects$name[[2]])
browseURL(signed_url)
