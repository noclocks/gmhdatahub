
#  ------------------------------------------------------------------------
#
# Title : General Utilities
#    By : Jimmy Briggs
#  Date : 2025-02-18
#
#  ------------------------------------------------------------------------

#' Get Closest Match
#'
#' @description
#' Get the closest match to a name from a list of valid names.
#'
#' @param name The name to match.
#' @param valid_names A vector of valid names.
#'
#' @returns
#' The closest match to the name from the list of valid names.
#'
#' @export
#'
#' @importFrom stringdist stringdist
get_closest_match <- function(name, valid_names) {

  if (!is.character(name)) name <- as.character(name)
  if (!is.character(valid_names)) valid_names <- as.character(valid_names)

  dists <- stringdist::stringdist(name, valid_names, method = "jw")
  min_indices <- which(dists == min(dists))
  closest_match <- valid_names[min_indices[which.min(nchar(valid_names[min_indices]))]]

  return(closest_match)

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

get_partner_name_by_id <- function(partner_id) {
  valid_partner_ids <- get_default_app_choices("partners")

  if (!partner_id %in% valid_partner_ids) {
    cli::cli_abort("{.arg partner_id} is not a valid partner ID.")
  }

  names(
    get_default_app_choices("partners")
  )[which(get_default_app_choices("partners") == partner_id)]
}

get_partner_id_by_name <- function(partner_name) {
  valid_partner_names <- names(get_default_app_choices("partners"))
  if (!partner_name %in% valid_partner_names) {
    cli::cli_abort("{.arg partner_name} is not a valid partner name.")
  }

  get_default_app_choices("partners")[[partner_name]]
}

get_amenity_id_by_name <- function(amenity_name) {
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
