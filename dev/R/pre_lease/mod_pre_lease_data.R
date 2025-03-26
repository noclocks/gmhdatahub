
#  ------------------------------------------------------------------------
#
# Title : Pre-Lease Data Module (Server Only)
#    By : Jimmy Briggs
#  Date : 2025-03-04
#
#  ------------------------------------------------------------------------

#' Pre-Lease Data Module (Server Only)
#'
#' @description
#' This module contains the server-side logic for the Pre-Lease Data module.
#'
#' It produces the following reactive data objects:
#'
#' - `pre_lease_summary_data`: The pre-lease summary data.
#' - `pre_lease_details_data`: The Entrata pre-lease details data.
#' - `pre_lease_details_by_property_data`: The Entrata pre-lease details by property data.
#'
#' @param id The module's namespace ID.
#' @param db_trigger The database trigger.
#' @param pool The database connection pool.
#'
#' @returns
#' List of reactive data objects:
#'
#' - `pre_lease_summary_data`: The pre-lease summary data.
#' - `pre_lease_details_data`: The Entrata pre-lease details data.
#' - `pre_lease_details_by_property_data`: The Entrata pre-lease details by property data.
#'
#' @export
#'
#' @importFrom cli cat_rule
#' @importFrom dplyr mutate select filter across coalesce transmute
#' @importFrom shiny is.reactive moduleServer reactive req
#' @importFrom tidyselect where
mod_pre_lease_data_server <- function(id, db_trigger, pool = NULL) {

  stopifnot(shiny::is.reactive(db_trigger))

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    cli::cat_rule("[Module]: mod_pre_lease_data_server()")

    # database connection
    if (is.null(pool)) pool <- session$userData$db_pool %||% db_connect()
    check_db_conn(pool)

    # pre_lease_summary_data
    pre_lease_summary_data <- shiny::reactive({
      shiny::req(pool, db_trigger())
      db_read_gmh_pre_lease_summary_tbl(pool)
    })

    # details data
    entrata_pre_lease_details_data <- shiny::reactive({
      shiny::req(pool, db_trigger())

    })

    # details by property data
    entrata_pre_lease_details_by_property_data <- shiny::reactive({
      shiny::req(pool, db_trigger())
      db_read_tbl(pool, "entrata.pre_lease_summary_by_unit") |>
        dplyr::filter(
          .data$report_date == max(.data$report_date, na.rm = TRUE)
        ) |>
        dplyr::transmute(
          report_date = .data$report_date,
          property_id = .data$property_id,
          property_name = .data$property_name,
          unit_type = .data$unit_type,
          total_units = .data$total_unit_count,
          excluded_units = .data$excluded_unit_count,
          rentable_units = .data$rentable_unit_count,
          available_units = .data$available_count,
          avg_scheduled_charges = .data$avg_scheduled_rent,
          current_occupied = .data$occupied_count,
          current_occupancy = .data$occupied_count / .data$available_count,
          current_total_new = .data$approved_new_count + .data$partially_completed_new_count + .data$completed_new_count,
          current_total_renewals = .data$approved_renewal_count + .data$partially_completed_renewal_count + .data$completed_renewal_count,
          current_total_leases = .data$current_total_new + .data$current_total_renewals,
          current_preleased_percent = .data$current_total_leases / .data$available_count,
          prior_total_new = .data$approved_new_count_prior + .data$partially_completed_new_count_prior + .data$completed_new_count_prior,
          prior_total_renewals = .data$approved_renewal_count_prior + .data$partially_completed_renewal_count_prior + .data$completed_renewal_count_prior,
          prior_total_leases = .data$approved_count_prior + .data$partially_completed_count_prior + .data$completed_count_prior,
          prior_preleased_percent = .data$prior_total_leases / .data$available_count,
          yoy_variance_count = .data$current_total_leases - .data$prior_total_leases,
          yoy_variance_percent = .data$current_preleased_percent - .data$prior_preleased_percent
        ) |>
        dplyr::mutate(
          dplyr::across(
            tidyselect::where(is.numeric),
            function(x) {
              as.numeric(x) |> dplyr::coalesce(0.00)
            }
          ),
          dplyr::across(
            tidyselect::where(is.integer),
            function(x) {
              as.integer(x) |> dplyr::coalesce(0L)
            }
          )
        )

    })

    # return the data
    return(
      list(
        pre_lease_summary_data = pre_lease_summary_data,
        pre_lease_details_data = entrata_pre_lease_details_data,
        pre_lease_details_by_property_data = entrata_pre_lease_details_by_property_data
      )
    )

  })

}
