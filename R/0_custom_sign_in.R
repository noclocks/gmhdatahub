#' Custom Sign In UI
#'
#' @description
#' the custom Sign In UI for `noClocksAuthR`
#'
#'
#' @importFrom noClocksAuthR sign_in_module_2_ui sign_in_ui_default
#'
#' @export
#'
custom_sign_in_ui <- noClocksAuthR::sign_in_ui_default(
  sign_in_module = noClocksAuthR::sign_in_module_2_ui('sign_in'),
  color = '#14beea',
  company_name = 'GMH',
  icon_href = file.path("www/images/favicon.png"),
  logo_top = shiny::tags$h1(
    # Make style better for title of Log In page
    style = 'margin-bottom: 2.5%; font-weight: 500; font-size: 3.5em; color: #FFF;',
    "GMH Data Hub"
  ),
  logo_bottom = shiny::tags$img(
    src = file.path("www/images/logos/gmh-logo.svg"),
    alt = 'GMH Communities',
    style = 'width: 40%; margin-top: 2.5%;'
  )
)
