#' Excel Utility Functions
#'
#' @description
#' Functions for working with and munging data originating from Excel.
#'
#' @name excel
NULL


#' Create Excel Ranges
#'
#' @description
#' Helper function to create ranges for Excel sheets based on filter patterns
#' and column offsets.
#'
#' @param cells `data.frame` of raw Excel cells created via [tidyxl::xlsx_cells()].
#'   Must contain (at least) the columns `sheet`, `row`, `col`, `data_type`
#'   and `character`.
#' @param filter_pattern `character` pattern to filter cells by.
#' @param start_offset `integer` offset to start the range from.
#' @param stop_col `integer` or `character` column to stop the range at.
#'
#' @return `data.frame` of ranges for each sheet.
#' @export
#'
#' @importFrom dplyr group_by slice ungroup mutate select filter
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom tidyr unite
#' @importFrom readxl excel_sheets read_excel
create_ranges <- function(
    cells,
    filter_pattern,
    start_offset,
    stop_col
) {

  # validate inputs
  stopifnot(exprs = {
    is.data.frame(cells)
    all(
      c("sheet", "row", "col", "data_type", "character") %in% colnames(cells)
    )
    is.character(filter_pattern)
    is.integer(start_offset)
    is.character(stop_col) || is.integer(stop_col)
  }) |> try()

  # create ranges
  cells |>
    dplyr::filter(
      stringr::str_detect(.data$character, .env$filter_pattern)
    ) |>
    dplyr::group_by(.data$sheet) |>
    dplyr::slice(1) |>  # get first row of each sheet
    dplyr::mutate(
      start_cell = paste0("A", .env$start_offset),
      stop_cell = paste0(.env$stop_col, .data$row - 1)
    ) |>
    dplyr::ungroup() %>%
    dplyr::mutate(range = paste0(.data$start_cell, ":", .data$stop_cell)) |>
    dplyr::select(.data$sheet, .data$range)
}
