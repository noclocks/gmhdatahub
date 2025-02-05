mod_survey_insights_overview_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "Weekly Leases",
        value = shiny::textOutput(ns("val_weekly_leases")),
        showcase = bsicons::bs_icon("graph-up")
      ),
      bslib::value_box(
        title = "Weekly Traffic",
        value = shiny::textOutput(ns("val_weekly_traffic")),
        showcase = bsicons::bs_icon("people")
      ),
      bslib::value_box(
        title = "Current Occupancy",
        value = shiny::textOutput(ns("val_current_occupancy")),
        showcase = bsicons::bs_icon("building")
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Property vs. Competitor Performance Overview"),
        bslib::card_body(
          reactable::reactableOutput(
            ns("property_competitor_performance_table")
          )
        )
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Weekly Velocity and Traffic"),
        bslib::card_body(
          apexcharter::apexchartOutput(
            ns("velocity_traffic_chart"),
            height = "400px"
          )
        )
      ),
      bslib::card(
        bslib::card_header("Rate Adjustments (Prior Six Weeks)"),
        bslib::card_body(
          apexcharter::apexchartOutput(
            ns("rate_adjustments_chart"),
            height = "400px"
          )
        )
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Market Velocity Comparison"),
        bslib::card_body(
          apexcharter::apexchartOutput(
            ns("market_velocity_comparison_chart"),
            height = "400px"
          )
        )
      )
    )
  )

}


mod_survey_insights_overview_server <- function(
  id,
  pool = NULL,
  selected_property_ids = NULL,
  selected_competitor_ids = NULL,
  selected_date_range = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      cli::cli_rule("[Module]: mod_survey_insights_overview_server()")

      # check database connection
      if (is.null(pool)) pool <- session$userData$pool %||% db_connect()
      check_db_conn(pool)

      # demo data
      demo_data <- shiny::reactive({
        set.seed(123)
        dates <- seq(lubridate::today() - lubridate::weeks(6), lubridate::today(), by = "week")
        properties <- c("Subject Property", "Competitor A", "Competitor B", "Competitor C")

        expand.grid(
          date = dates,
          property = properties
        ) |>
          dplyr::mutate(
            total_new_leases = round(runif(n(), 3, 15)),
            weekly_traffic = round(runif(n(), 10, 30)),
            market_rent_per_square_foot = round(runif(dplyr::n(), 700, 900)),
            current_occupancy = round(runif(dplyr::n(), 0.85, 0.98)),
            units_available = round(runif(dplyr::n(), 10, 50))
          )
      })

      # derive property + competitor names
      selected_properties_and_competitors <- shiny::reactive({
        prop_ids <- if (is.null(selected_property_ids())) {
          get_property_id_by_name("1047 Commonwealth Avenue")
        } else {
          selected_property_ids()
        }
        comp_ids <- if (is.null(selected_competitor_ids()) || selected_competitor_ids() == "none") {
          get_default_app_choices("competitors")
        } else {
          selected_competitor_ids()
        }
        prop_names <- Vectorize(get_property_name_by_id)(prop_ids)
        comp_names <- Vectorize(get_competitor_name_by_id)(comp_ids)
        props <- prop_ids |> setNames(prop_names)
        comps <- comp_ids |> setNames(comp_names)
        c(props, comps)
      })

      # initial data
      survey_leasing_data <- shiny::reactive({
        shiny::req(selected_properties_and_competitors())

        prop_names <- names(selected_properties_and_competitors())

        leasing_summary_tbl <- db_read_tbl(pool, "survey.leasing_summary") |>
          dplyr::filter(
            property_name %in% prop_names
          ) |>
          dplyr::select(
            property_name,
            lease_launch_date,
            renewal_launch_date,
            current_occupancy,
            prior_year_occupancy,
            current_pre_lease,
            prior_year_pre_lease = last_year_pre_lease,
            total_renewals,
            total_new_leases,
            weekly_leases,
            weekly_traffic
          )

        rents_tbl <- db_read_tbl(pool, "survey.rents_by_floorplan") |>
          dplyr::filter(
            property_name %in% prop_names
          ) |>
          dplyr::select(
            property_name,
            floorplan_type,
            floorplan_id,
            square_feet,
            total_units_count,
            available,
            market_rent_per_bed,
            market_rent_per_square_foot,
            effective_rent_per_bed,
            effective_rent_per_square_foot,
            expenses_total,
            bundled_rent_per_bed,
            bundled_rent_per_square_foot
          )

        dplyr::left_join(
          leasing_summary_tbl,
          rents_tbl,
          by = "property_name"
        )

      })


      output$val_weekly_leases <- shiny::renderText({
        demo_data() |>
          dplyr::slice_tail(n = 1) |>
          dplyr::pull("total_new_leases") |>
          scales::comma()
      })

      output$val_weekly_traffic <- shiny::renderText({
        demo_data() |>
          dplyr::slice_tail(n = 1) |>
          dplyr::pull("weekly_traffic") |>
          scales::comma()
      })

      output$val_current_occupancy <- shiny::renderText({
        demo_data() |>
          dplyr::slice_tail(n = 1) |>
          dplyr::pull("current_occupancy") |>
          scales::percent()
      })

      output$property_competitor_performance_table <- reactable::renderReactable({

        tbl_data <- demo_data() |>
          dplyr::group_by(property) |>
          dplyr::summarize(
            total_new_leases = sum(total_new_leases),
            weekly_traffic = sum(weekly_traffic),
            market_rent_per_square_foot = mean(market_rent_per_square_foot),
            current_occupancy = mean(current_occupancy),
            units_available = sum(units_available)
          ) |>
          dplyr::ungroup()

        reactable::reactable(
          data = tbl_data,
          defaultPageSize = 5,
          sortable = TRUE,
          striped = TRUE,
          highlight = TRUE,
          defaultSorted = "property",
          width = "100%",
          columns = list(
            property = reactable::colDef(
              name = "Property",
              align = "left"
            ),
            total_new_leases = reactable::colDef(
              name = "Total New Leases",
              align = "center",
              format = reactable::colFormat(digits = 0)
            ),
            weekly_traffic = reactable::colDef(
              name = "Weekly Traffic",
              align = "center",
              format = reactable::colFormat(digits = 0)
            ),
            market_rent_per_square_foot = reactable::colDef(
              name = "Market Rent/SqFt",
              align = "center",
              format = reactable::colFormat(currency = "USD")
            ),
            current_occupancy = reactable::colDef(
              name = "Current Occupancy",
              align = "center",
              format = reactable::colFormat(percent = TRUE)
            ),
            units_available = reactable::colDef(
              name = "Units Available",
              align = "center",
              format = reactable::colFormat(digits = 0)
            )
          )
        )
      })

      output$velocity_traffic_chart <- apexcharter::renderApexchart({

        chart_data <- demo_data() |>
          tidyr::pivot_longer(
            cols = c("total_new_leases", "weekly_traffic"),
            names_to = "metric",
            values_to = "value"
          )

        apexcharter::apex(
          chart_data,
          mapping = apexcharter::aes(
            x = date,
            y = value,
            color = interaction(property, metric)
          ),
          type = "area"
        ) |>
          apexcharter::ax_chart(
            stacked = TRUE,
            animations = list(enabled = TRUE, easing = "easeinout")
          ) |>
          apexcharter::ax_title(
            text = "Weekly Velocity and Traffic"
          ) |>
          apexcharter::ax_yaxis(
            title = list(
              text = "Number of Leases / Traffic"
            ),
            labels = list(
              formatter = htmlwidgets::JS("function(val) { return Math.round(val) }")
            )
          ) |>
          apexcharter::ax_xaxis(
            title = list(text = "Date")
          ) |>
          apexcharter::ax_colors(
            c(
              "#1f77b4",
              "#ff7f0e",
              "#2ca02c",
              "#d62728",
              "#7f7f7f",
              "#bcbd22",
              "#17becf",
              "#9467bd"
            )
          ) |>
          apexcharter::ax_legend(
            position = "top",
            horizontalAlign = "center"
          ) |>
          apexcharter::ax_tooltip(
            shared = TRUE,
            followCursor = TRUE
          ) |>
          apexcharter::ax_stroke(
            curve = "smooth"
          ) |>
          apexcharter::ax_dataLabels(
            enabled = FALSE
          )
      })

      output$rate_adjustments_chart <- apexcharter::renderApexchart({

        chart_data <- demo_data()

        apexcharter::apex(
          chart_data,
          mapping = apexcharter::aes(
            x = date,
            y = market_rent_per_square_foot,
            color = property
          ),
          type = "line"
        ) |>
          apexcharter::ax_chart(
            animations = list(enabled = TRUE, easing = "easeinout"),
            dropShadow = list(enabled = TRUE, opacity = 0.3)
          ) |>
          apexcharter::ax_title(
            text = "Rate Adjustments (Prior Six Weeks)"
          ) |>
          apexcharter::ax_yaxis(
            title = list(text = "Average Rate ($)"),
            labels = list(formatter = htmlwidgets::JS("function(val) { return '$' + val }")),
            tickAmount = 5
          ) |>
          apexcharter::ax_xaxis(
            title = list(text = "Date")
          ) |>
          apexcharter::ax_legend(
            position = "top",
            horizontalAlign = "center"
          ) |>
          apexcharter::ax_colors(
            c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
          ) |>
          apexcharter::ax_tooltip(
            shared = TRUE,
            followCursor = TRUE,
            y = list(formatter = htmlwidgets::JS("function(val) { return '$' + val }"))
          ) |>
          apexcharter::ax_stroke(
            curve = "smooth",
            width = 3
          ) |>
          apexcharter::ax_markers(
            size = 6,
            hover = list(size = 8)
          )
      })

      output$market_velocity_comparison_chart <- apexcharter::renderApexchart({
        demo_data()  |>
          dplyr::group_by(property) |>
          apexcharter::apex(
            mapping = apexcharter::aes(x = date, y = total_new_leases, fill = property),
            type = "column"
          ) |>
          apexcharter::ax_chart(
            animations = list(enabled = TRUE, easing = "easeinout")
          ) |>
          apexcharter::ax_plotOptions(
            bar = list(
              columnWidth = "70%",
              dataLabels = list(position = "top")
            )
          ) |>
          apexcharter::ax_yaxis(
            title = list(text = "Count"),
            labels = list(formatter = htmlwidgets::JS("function(val) { return Math.round(val) }")),
            tickAmount = 5
          ) |>
          apexcharter::ax_xaxis(title = list(text = "Date")) |>
          apexcharter::ax_colors(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) |>
          apexcharter::ax_legend(
            position = "bottom",
            horizontalAlign = "center"
          ) |>
          apexcharter::ax_tooltip(
            shared = TRUE,
            followCursor = TRUE,
            intersect = FALSE
          ) |>
          apexcharter::ax_dataLabels(
            enabled = TRUE,
            formatter = htmlwidgets::JS("function(val) { return Math.round(val) }")
          )
      })

    }
  )
}
