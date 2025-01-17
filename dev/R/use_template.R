read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

render_template <- function(template_path, data = list()) {
  strsplit(whisker::whisker.render(read_utf8(template_path), data), "\n")[[1]]
}

use_template <- function(template, save_as, data = list(), ignore = FALSE, open = FALSE) {

  if (is.list(data) && length(data) > 0 && !is.null(data[["name"]])) {
    name <- data[["name"]]
    data[["title"]] <- snakecase::to_title_case(name)
  }

  data[["author"]] <- whoami::fullname()
  data[["date"]] <- format(Sys.Date(), "%Y-%m-%d")

  content <- render_template(template, data)
  new <- usethis::write_over(save_as, content)
  if (ignore) {
    usethis::use_build_ignore(save_as)
  }
  if (open && new) {
    file.edit(save_as)
  }
  invisible(new)
}
