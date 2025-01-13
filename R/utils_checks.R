validate_col_names <- function(data, cols) {
  stopifnot(all(cols %in% colnames(data)))
}
