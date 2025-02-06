#  ------------------------------------------------------------------------
#
# Title : Validation Utilities
#    By : Jimmy Briggs
#  Date : 2025-01-14
#
#  ------------------------------------------------------------------------

set_validation_rules <- function(iv, section) {
  switch(section,
    "Property Summary" = {
      input_ids <- property_summary_inputs_tbl$id
      required_inputs <- property_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(id)
      validation_msgs <- property_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(name) |>
        purrr::map_chr(~ paste0(.x, " is required."))
      iv <- shinyvalidate::InputValidator$new()
      purrr::walk2(
        required_inputs,
        validation_msgs,
        function(input_id, validation_msg) {
          iv$add_rule(
            input_id,
            shinyvalidate::sv_required(message = validation_msg)
          )
        }
      )
      # add regex rules for phone and website
      phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"
      iv$add_rule("property_website", shinyvalidate::sv_url(message = "Please enter a valid URL for the property website."))
      iv$add_rule("property_phone_number", shinyvalidate::sv_regex(phone_regex, "Enter a valid phone number."))
      # add rules for distance to campus and property rating
      iv$add_rule("distance_to_campus", shinyvalidate::sv_gt(0, "Distance must be greater than 0."))
      iv$add_rule("property_rating", shinyvalidate::sv_gt(0, "Rating must be greater than 0."))
      return(iv)
    },
    "Leasing Summary" = {
      input_ids <- leasing_summary_inputs_tbl$id
      required_inputs <- leasing_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(id)
      validation_msgs <- leasing_summary_inputs_tbl |>
        dplyr::filter(required == TRUE) |>
        dplyr::pull(name) |>
        purrr::map_chr(~ paste0(.x, " is required."))
      iv <- shinyvalidate::InputValidator$new()
      purrr::walk2(
        required_inputs,
        validation_msgs,
        function(input_id, validation_msg) {
          iv$add_rule(
            input_id,
            shinyvalidate::sv_required(message = validation_msg)
          )
        }
      )
      iv$add_rule("current_occupancy", shinyvalidate::sv_between(0, 1))
      iv$add_rule("last_year_occupancy", shinyvalidate::sv_between(0, 1))
      iv$add_rule("current_pre_lease", shinyvalidate::sv_between(0, 1))
      iv$add_rule("last_year_pre_lease", shinyvalidate::sv_between(0, 1))
      iv$add_rule("total_renewals", shinyvalidate::sv_integer())
      iv$add_rule("total_new_leases", shinyvalidate::sv_integer())
      iv$add_rule("total_leases_weekly", shinyvalidate::sv_integer())
      iv$add_rule("traffic_weekly", shinyvalidate::sv_integer())
      iv$add_rule("incentive_amount", shinyvalidate::sv_numeric())
      return(iv)
    }
  )
}

intro_validator <- function() {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    cli::cli_abort(
      "This function must be called within a valid shiny session."
    )
  }

  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("survey_user", shinyvalidate::sv_email())
}

property_summary_validator <- function() {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    cli::cli_abort(
      "This function must be called within a valid shiny session."
    )
  }

  input_ids <- property_summary_inputs_tbl$id
  required_inputs <- property_summary_inputs_tbl |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(id)
  validation_msgs <- property_summary_inputs_tbl |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(name) |>
    purrr::map_chr(~ paste0(.x, " is required."))

  iv <- shinyvalidate::InputValidator$new()

  purrr::walk2(
    required_inputs,
    validation_msgs,
    function(input_id, validation_msg) {
      iv$add_rule(
        input_id,
        shinyvalidate::sv_required(message = validation_msg)
      )
    }
  )

  # add regex rules for phone and website
  phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"
  iv$add_rule("property_website", shinyvalidate::sv_url(message = "Please enter a valid URL for the property website."))
  iv$add_rule("property_phone_number", shinyvalidate::sv_regex(phone_regex, "Enter a valid phone number."))

  # add rules for distance to campus and property rating
  iv$add_rule("distance_to_campus", shinyvalidate::sv_gt(0, "Distance must be greater than 0."))
  iv$add_rule("property_rating", shinyvalidate::sv_gt(0, "Rating must be greater than 0."))

  return(iv)
}


leasing_summary_validator <- function() {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    cli::cli_abort(
      "This function must be called within a valid shiny session."
    )
  }

  input_ids <- leasing_summary_inputs_tbl$id
  required_inputs <- leasing_summary_inputs_tbl |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(id)
  validation_msgs <- leasing_summary_inputs_tbl |>
    dplyr::filter(required == TRUE) |>
    dplyr::pull(name) |>
    purrr::map_chr(~ paste0(.x, " is required."))

  iv <- shinyvalidate::InputValidator$new()

  purrr::walk2(
    required_inputs,
    validation_msgs,
    function(input_id, validation_msg) {
      iv$add_rule(
        input_id,
        shinyvalidate::sv_required(message = validation_msg)
      )
    }
  )

  iv$add_rule("current_occupancy", shinyvalidate::sv_between(0, 1))
  iv$add_rule("last_year_occupancy", shinyvalidate::sv_between(0, 1))
  iv$add_rule("current_pre_lease", shinyvalidate::sv_between(0, 1))
  iv$add_rule("last_year_pre_lease", shinyvalidate::sv_between(0, 1))
  iv$add_rule("total_renewals", shinyvalidate::sv_integer())
  iv$add_rule("total_new_leases", shinyvalidate::sv_integer())
  iv$add_rule("total_leases_weekly", shinyvalidate::sv_integer())
  iv$add_rule("traffic_weekly", shinyvalidate::sv_integer())
  iv$add_rule("incentive_amount", shinyvalidate::sv_numeric())

  return(iv)
}

validate_phone_regex <- function(phone) {
  phone_regex <- "^(?:\\(?[0-9]{3}\\)?[-. ]?[0-9]{3}[-. ]?[0-9]{4})$"
  grepl(phone_regex, phone)
}

validate_address_regex <- function(address) {
  address_regex <- "^[0-9]+\\s+([a-zA-Z]+|[a-zA-Z]+\\s[a-zA-Z]+)\\s[a-zA-Z]+"
  grepl(address_regex, address)
}

validate_address_geocode <- function(address) {
  geocode <- tryCatch(
    {
      geocode_address(address)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(geocode)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


validate_property_data <- function(input, is_new = TRUE) {

  errors <- character()

  if (is.null(input$property_name) || nchar(input$property_name) < 1)
    errors <- c(errors, "Property name is required")

  if (is.null(input$total_units) || input$total_units < 1)
    errors <- c(errors, "Total units must be greater than 0")

  if (is.null(input$total_beds) || input$total_beds < 1)
    errors <- c(errors, "Total beds must be greater than 0")

  if (is.null(input$occupancy) || input$occupancy < 0 || input$occupancy > 100)
    errors <- c(errors, "Occupancy must be between 0 and 100")

  if (is.null(input$prelease) || input$prelease < 0 || input$prelease > 100)
    errors <- c(errors, "Pre-lease must be between 0 and 100")

  if (is_new && (is.null(input$selected_place) || is.null(input$selected_place$lat)))
    errors <- c(errors, "Valid address selection is required")

  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors))
  }

  return(list(valid = TRUE))
}
