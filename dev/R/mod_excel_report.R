
#  ------------------------------------------------------------------------
#
# Title : Excel Report Shiny Module
#    By : Jimmy Briggs
#  Date : 2024-11-27
#
#  ------------------------------------------------------------------------

# topic -------------------------------------------------------------------

#' Excel Report Shiny Module
#'
#' @name mod_excel_report
#'
#' @description
#' A Shiny Module for generating and exporting/downloading a custom Excel
#' Report from a Shiny Application.
#'
#' - `mod_excel_report_ui()`: User interface including the download button
#'   and any necessary export options.
#' - `mod_excel_report_server()`: Server logic for generating the Excel
#'   report's [shiny::downloadHandler()] logic.
#'
#' @param id Module's namespace ID.
#'
#' @return
#' - `mod_excel_report_ui()`: UI HTML Output.
#' - `mod_excel_report_server()`: Reactive values returned from server logic.
#'
#' @examples
#' if (interactive()) {
#'   mod_excel_report_demo()
#' }
NULL


# UI ----------------------------------------------------------------------

#' @rdname mod_excel_report
#' @export
#' @importFrom shiny NS
#' @importFrom htmltools tagList tags
mod_excel_report_ui <- function(id) {

  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::tags$div(
      class = "btn-group",
      style = "display: flex; justify-content: center; align-items: center;",
      shiny::downloadButton(
        outputId = ns("download_excel"),
        label = "Excel Report",
        class = "btn btn-success",
        icon = shiny::icon("file-excel")
      )
    )
  )
}


# server ------------------------------------------------------------------

#' @rdname mod_excel_report
#' @export
#' @importFrom shiny moduleServer
#' @importFrom cli cat_rule
mod_excel_report_server <- function(
    id,
    pool = NULL,
    summary_table_data = NULL,
    details_table_data = NULL,
    report_parameters = NULL
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      cli::cat_rule("[Module]: mod_excel_report_server()")

      output$download_excel <- shiny::downloadHandler(
        filename = get_xl_report_file_name("GMH Pre-Lease Summary"),
        content = function(file) {

          template_xlsx <- pkg_sys("extdata", "excel", "pre_lease_report_template.xlsx")
          template_wb <- openxlsx::loadWorkbook(template_xlsx)
          summary_table_data

        }
      )

      return(
        list(

        )
      )

    }
  )
}


# demo --------------------------------------------------------------------

#' @rdname mod_excel_report
#' @export
#' @importFrom pkgload load_all
#' @importFrom bslib page_fluid bs_theme
#' @importFrom shiny shinyApp
mod_excel_report_demo <- function() {

  pkgload::load_all()

  ui <- bslib::page_fluid(
    title = "Demo",
    theme = bslib::bs_theme(version = 5),
    lang = "en",
    mod_excel_report_ui("demo")
  )

  server <- function(input, output, session) {
    mod_excel_report_server("demo")
  }

  shiny::shinyApp(ui, server)
}


# helpers -----------------------------------------------------------------


