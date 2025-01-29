#' App Information
#'
#' @description
#' This function provides information about the app.
#'
#' @param ... character vectors, specifying the information to retrieve.
#'
#' @returns
#' A list of information about the app.
#'
#' @export
#'
#' @examples
#' app_info()
app_info <- function(...) {

  info <- list(
    name = "gmhdatahub",
    title = "GMH Data Hub",
    company = "GMH Communities",
    version = "1.0",
    logo = "www/gmh-logo.svg",
    symbol = "www/gmh-icon.png",
    repo_url = "https://github.com/noclocks/gmhdatahub",
    docs_url = "https://docs.noclocks.dev/gmhdatahub"
  )

  # Return the requested info
  if (missing(...)) {
    return(info)
  } else {
    return(info[[...]])
  }
}


