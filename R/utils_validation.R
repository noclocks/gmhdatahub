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
