welcome_banner_ui <- function() {
  bslib::layout_columns(
    col_widths = c(12),
    bslib::card(
      class = "my-3",
      bslib::card_header(
        class = "bg-primary",
        htmltools::tags$h2(
          "Welcome to GMH Communities Data Hub",
          class = "mb-0"
        )
      ),
      bslib::card_body(
        htmltools::tags$p(
          class = "lead",
          "Your centralized platform for student housing portfolio analytics and insights"
        )
      )
    )
  )
}

gmaps_properties_map_embed_iframe <- function(width = "100%", height = "500px") {
  url <- "https://www.google.com/maps/d/embed?mid=19tP5Bf66khGcrNnqTBsk879W2fS-u7U&ehbc=2E312F"

  htmltools::tags$iframe(
    src = url,
    width = width,
    height = height
  )
}

with_loader <- function(x) {
  shinycustomloader::withLoader(x)
}

with_tooltip <- function(input, tooltip_text, placement = "right") {
  current_label <- input$children[[1]]$children[[1]]

  updated_label <- htmltools::tags$span(
    current_label,
    bslib::tooltip(
      bsicons::bs_icon("info-circle", style = "cursor: help;"),
      tooltip_text,
      placement = placement
    )
  )

  input$children[[1]]$children[[1]] <- updated_label

  return(input)
}

icon_text <- function(icon, text, .function = shiny::icon) {
  if (is.character(icon) && length(icon) == 1) {
    icon <- .function(icon)
  }

  text <- paste0(" ", text)

  htmltools::tags$span(
    icon,
    text
  )
}

text_icon <- function(text, icon, .function = shiny::icon) {
  icon <- .function(icon)
  text <- paste0(text, " ")

  htmltools::tags$span(
    text,
    icon
  )
}

parse_request <- function(req) {
  http_method <- req$REQUEST_METHOD
  path_info <- req$PATH_INFO
  protocol <- req$HTTP_X_FORWARDED_PROTO

  list(
    http_method = http_method,
    path_info = path_info,
    protocol = protocol
  )
}

get_default_app_choices <- function(type) {
  type <- rlang::arg_match0(type, names(app_choices_lst))
  app_choices_lst[[type]]
}

get_survey_choices <- function(section, type) {
  section <- rlang::arg_match0(section, names(survey_choices_lst))
  type <- rlang::arg_match0(type, names(survey_choices_lst[[section]]))
  survey_choices_lst[[section]][[type]]
}

get_property_name_by_id <- function(property_id) {
  valid_property_ids <- get_default_app_choices("properties")
  if (!property_id %in% valid_property_ids) {
    cli::cli_abort("{.arg property_id} is not a valid property ID.")
  }

  names(
    get_default_app_choices("properties")
  )[which(get_default_app_choices("properties") == property_id)]
}

get_property_id_by_name <- function(property_name) {
  valid_property_names <- names(get_default_app_choices("properties"))
  if (!property_name %in% valid_property_names) {
    cli::cli_abort("{.arg property_name} is not a valid property name.")
  }

  get_default_app_choices("properties")[[property_name]]
}

get_competitor_name_by_id <- function(competitor_id) {
  valid_competitor_ids <- get_default_app_choices("competitors")
  if (!competitor_id %in% valid_competitor_ids) {
    cli::cli_abort("{.arg competitor_id} is not a valid competitor ID.")
  }

  names(
    get_default_app_choices("competitors")
  )[which(get_default_app_choices("competitors") == competitor_id)]
}

get_competitor_id_by_name <- function(competitor_name) {
  valid_competitor_names <- names(get_default_app_choices("competitors"))
  if (!competitor_name %in% valid_competitor_names) {
    cli::cli_abort("{.arg competitor_name} is not a valid competitor name.")
  }

  get_default_app_choices("competitors")[[competitor_name]]
}

get_amenity_id_by_name <- function(pool, amenity_name) {
  check_db_conn(pool)

  amenity_name <- tolower(amenity_name)

  amenity_id <- survey_amenities_tbl |>
    dplyr::filter(tolower(.data$amenity_name) == .env$amenity_name) |>
    dplyr::pull("amenity_id")

  if (length(amenity_id) == 0) {
    cli::cli_alert_danger(
      "Amenity not found in database."
    )
    return(NULL)
  }

  return(amenity_id)
}

#' NULL Coalescing Operator
#'
#' @description
#' This function is a wrapper for the NULL coalescing operator:
#' If x if `NULL`, return y, else return x.
#'
#' @param x, y Values to compare.
#'
#' @keywords internal
`%||%` <- rlang::`%||%`

#' Inverted versions of in
#' @noRd
`%notin%` <- Negate(`%in%`)

encrypt_cfg_file <- function(
    cfg_file = Sys.getenv("R_CONFIG_FILE", "config.yml"),
    key = "NOCLOCKS_ENCRYPTION_KEY",
    ...) {
  if (is.null(Sys.getenv(key)) || !httr2::secret_has_key(key)) {
    rlang::abort(
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

decrypt_cfg_file <- function(
    cfg_file = Sys.getenv("R_CONFIG_FILE", "inst/config/config.yml"),
    key = "NOCLOCKS_ENCRYPTION_KEY") {
  if (!httr2::secret_has_key(key)) {
    rlang::abort(
      glue::glue(
        "Encryption key: {key} not found.",
        "Please set the encryption key in your environment variables."
      )
    )
  }

  cfg_file <- fs::path(cfg_file)
  cfg_file_encrypted <- fs::path_ext_remove(cfg_file) |>
    paste0(".encrypted.yml") |>
    fs::path()

  cfg_file_decrypted <- httr2::secret_decrypt_file(
    path = cfg_file_encrypted,
    key = key
  )

  fs::file_move(
    cfg_file_decrypted,
    cfg_file
  )

  cli::cli_alert_success("Successfully decrypted the config file: {.file cfg_file}")
  cli::cli_alert_info("The decrypted file is now the active config file.")

  Sys.setenv("R_CONFIG_FILE" = cfg_file)
  cli::cli_alert_info("Set `R_CONFIG_FILE` to: {.file cfg_file}")

  return(invisible(config::get()))
}
