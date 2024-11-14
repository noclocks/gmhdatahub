
#  ------------------------------------------------------------------------
#
# Title : Home (Page) Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------


# topic -------------------------------------------------------------------

#' Home (Page) Shiny Module
#'
#' @name mod_home
#'
#' @description
#' A Shiny module for creating the home page tab of the GMH Data Hub's Leasing
#' Dashboard.
#'
#' This module includes the UI and Server functions:
#' - `mod_home_ui()`
#' - `mod_home_server()`
#'
#' @param id The module id.
#'
#' @return
#' - `mod_home_ui()`: The UI HTML wrapped in a [htmltools::tagList()].
#' - `mod_home_server()`: A shiny server function.
NULL

# UI ----------------------------------------------------------------------

#' @rdname mod_home
#' @export
#' @importFrom htmltools tags tagList
#' @importFrom bslib card card_body card_title layout_columns layout_column_wrap
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @importFrom shiny NS textOutput uiOutput
mod_home_ui <- function(id, ...) {

  ns <- shiny::NS(id)

  # value boxes -------------------------------------------------------------
  htmltools::tagList(
    bslib::layout_column_wrap(
      width = 1/3,
      heights_equal = "all",
      bslib::value_box(
        id = ns("properties_value_box"),
        title = "Properties",
        value = shiny::textOutput(ns("properties_value_box"), inline = TRUE),
        shiny::uiOutput(ns("properties_value_box_details")),
        showcase = bsicons::bs_icon("building-check"),
        showcase_layout = "left center",
        full_screen = TRUE,
        theme = "info",
        height = NULL,
        max_height = NULL,
        min_height = NULL,
        fill = TRUE,
        class = NULL
      ),
      bslib::value_box(
        id = ns("leases_value_box"),
        title = "Leases",
        value = shiny::textOutput(ns("leases_value_box"), inline = TRUE),
        shiny::uiOutput(ns("leases_value_box_details")),
        showcase = bsicons::bs_icon("file-earmark-text-fill"),
        showcase_layout = "left center",
        full_screen = TRUE,
        theme = "success",
        height = NULL,
        max_height = NULL,
        min_height = NULL,
        fill = TRUE,
        class = NULL
      ),
      bslib::value_box(
        id = ns("rate_value_box"),
        title = "Occupancy Rate",
        value = shiny::textOutput(ns("rate_value_box"), inline = TRUE),
        shiny::uiOutput(ns("rate_value_box_details")),
        showcase = bsicons::bs_icon("house-check-fill"),
        showcase_layout = "left center",
        full_screen = TRUE,
        theme = "primary",
        height = NULL,
        max_height = NULL,
        min_height = NULL,
        fill = TRUE,
        class = NULL
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(bs_icon("graph-up"), "Key Metrics"),
        bslib::card_body(
          "This is where you would display additional metrics or charts."
        ),
        full_screen = TRUE
      ),
      bslib::card(
        bslib::card_header(bs_icon("clock-history"), "Recent Activity"),
        bslib::card_body(
          "This section could show recent updates or activities."
        ),
        full_screen = TRUE
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header(bs_icon("calendar2-check"), "Upcoming Events"),
        bslib::card_body(
          "This section could show upcoming events or important dates."
        ),
        full_screen = TRUE
      ),
      bslib::card(
        bslib::card_header(bs_icon("chat-dots"), "Messages"),
        bslib::card_body(
          "This section could show recent messages or notifications."
        ),
        full_screen = TRUE
      )
    )
  )

}


# Server ------------------------------------------------------------------

#' @rdname mod_home
#' @export
#'
mod_home_server <- function(id, filters, data, user) {
  shiny::server <- function(input, output, session) {

    ns <- session$ns

    # reactive values -------------------------------------------------------
    properties <- shiny::reactive({
      shiny::req(input$properties)
      shiny::req(input$leases)
      shiny::req(input$rate)

      list(
        properties = input$properties,
        leases = input$leases,
        rate = input$rate
      )
    })

  }
}


# summary_table_col_names <- c(
#   "Property",
#   "Investment Partner",
#   "Total Beds",
#   "Model Beds",
#   "Current Occupancy",
#   "Total New",
#   "Total Renewals",
#   "Total Leases",
#   "25-26 Prelease %",
#   "New",
#   "Renewals",
#   "Total",
#   "Prelease %",
#   "Year-Over-Year Variance",
#   "Year-Over-Year Variance",
#   "New",
#   "Renewals",
#   "Total",
#   "% Gained",
#   "Beds Left to Lease",
#   "90%",
#   "95%",
#   "100%"
# )
#
# summary_table_header_style <- "text-align: center; border: 1px solid #0000004D; vertical-align: middle;"
#
# summary_table_container <- htmltools::withTags(
#   table(
#     class = 'display table-responsive',
#     style = 'width: -webkit-fill-available; background-color: #FFFFFF;',
#     thead(
#       style = 'background: #666666; color: #FFFFFF;',
#       tr(
#         lapply(summary_data_colnames[1:9], function(x) th(rowspan = "2", x, style = custom_header_style)),
#         th(colspan = 4, '24-25 Prior Year Same Store', style = custom_header_style),
#         th(colspan = 2, 'Year-Over-Year Variance', style = custom_header_style),
#         th(colspan = 4, 'Preleasing Activity - Prior Seven Days:', style = custom_header_style),
#         th(colspan = 4, 'Weekly Velocity Needed:', style = paste0(custom_header_style, ' border-right: 1px solid #0000004D;'))
#     ),
#     tr(
#       lapply(summary_data_colnames[10:length(summary_data_colnames)], function(x) th(x, style = ifelse(
#         x == '100%',
#         'text-align: center; border: 1px solid #0000004D; vertical-align: middle;',
#         'text-align: center; border-left: 1px solid #0000004D; vertical-align: middle;'
#       )))
#     )
#   ),
#   tfoot(
#     tr(
#       # Create footer row based on first row of `dat` (if available)
#       lapply(if (!is.null(dat) && nrow(dat) > 0) dat[1, ] else rep('', length(summary_data_colnames)),
#              function(x) th(x, style = 'background-color: #0D1B2D; color: white; text-align: center; vertical-align: middle; padding-right: 5px;'))
#     )
#   )
# ))
#
# output$summary_table <- DT::renderDT({
#   # Fetch and validate data
#   dat <- summary_table_out()
#   investment_partners_df <- investment_partners_rv() %||% gmhLeasingDashboard::investment_partners
#
#   # Handle empty data case with default structure
#   if (is.null(dat) || nrow(dat) == 0L) {
#     dat <- data.frame(matrix(ncol = length(summary_data_colnames), nrow = 0))
#   } else {
#     # Add Investment Partner information and format columns
#     dat <- dat |>
#       dplyr::mutate(
#         X13 = dplyr::if_else(X13 < 0, paste0('(', abs(X13), ')'), as.character(X13))
#       ) |>
#       dplyr::left_join(investment_partners_df, by = dplyr::join_by(X1 == property_name)) |>
#       dplyr::mutate(investment_partner = dplyr::if_else(is.na(investment_partner), '', investment_partner)) |>
#       dplyr::select(X1, investment_partner, dplyr::everything())
#   }
#
#   # Define editable fields based on user permissions
#   editable_fields <- if (session$userData$user()$is_admin) {
#     list(target = 'cell', disable = list(columns = c(0, 2, 4:ncol(dat))))
#   } else {
#     FALSE
#   }
#
#   # Render DataTable
#   DT::datatable(
#     data = dat[-1, ],  # Exclude the header row for display
#     class = 'cell-border stripe hover compact table-responsive',
#     container = dt_container,
#     elementId = 'summary_table',
#     autoHideNavigation = TRUE,
#     rownames = FALSE,
#     selection = 'none',
#     style = 'bootstrap',
#     editable = editable_fields,
#     extensions = c('Buttons'),
#     options = list(
#       autoWidth = FALSE,
#       dom = 'Bfrti',
#       buttons = list('print'),
#       pageLength = nrow(dat) - 1,
#       scrollX = TRUE,
#       rowCallback = DT::JS("
#         function(row, data, index) {
#           if (data[0]?.includes('AGC')) {
#             $('td', row).css({'background-color': '#0D1B2D', 'font-weight': 'bold', 'color': 'white'});
#           }
#         }")
#     ),
#     callback = DT::JS("
#         $(table.table().container()).addClass('table-responsive');
#         $.fn.dataTable.ext.errMode = 'none';
#
#         table.column('3').nodes().to$().each(function(i, cell) {
#           $(cell).css('color', '#1400FF');
#         });
#
#        return table;
#       ")
#   ) |>
#     DT::formatPercentage(columns = c(5, 9, 13, 15, 19), digits = 1) |>
#     DT::formatRound(columns = c(21, 22, 23), digits = 0) |>
#     DT::formatStyle(columns = 2:ncol(dat), textAlign = 'center')
# }, server = FALSE)
