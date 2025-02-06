derive_tbl_totals <- function(data, count_cols = NULL, sum_cols = NULL, avg_cols = NULL) {

  cols <- c(count_cols, sum_cols, avg_cols)
  validate_col_names(data, cols)

  data |>
    dplyr::select(tidyselect::all_of(cols)) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::all_of(count_cols),
        function(x) {
          dplyr::n_distinct(x, na.rm = TRUE)
        }
      ),
      dplyr::across(
        tidyselect::all_of(sum_cols),
        function(x) {
          sum(x, na.rm = TRUE)
        }
      ),
      dplyr::across(
        tidyselect::all_of(avg_cols),
        function(x) {
          mean(x, na.rm = TRUE)
        }
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(count_cols),
        function(x) {
          prettyNum(x, big.mark = ",")
        }
      ),
      dplyr::across(
        tidyselect::all_of(avg_cols),
        function(x) {
          round(x, 2) |>
            prettyNum(big.mark = ",")
        }
      ),
      dplyr::across(
        tidyselect::all_of(sum_cols),
        function(x) {
          prettyNum(x, big.mark = ",")
        }
      )
    )
}
