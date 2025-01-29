#' Custom Sign In UI
#'
#' @description
#' Sign In UI from [noClocksAuthR::sign_in_ui_default()].
#'
#' @export
#'
#' @returns
#' Sign In UI
#'
#' @importFrom htmltools tags
#' @importFrom noClocksAuthR sign_in_module_2_ui sign_in_ui_default
custom_sign_in_ui <- noClocksAuthR::sign_in_ui_default(
  sign_in_module = noClocksAuthR::sign_in_module_2_ui("sign_in"),
  color = gmh_colors("primary"),
  company_name = app_info("company"),
  icon_href = file.path("www/images/favicon.png"),
  logo_top = htmltools::tags$h1(
    style = "margin-bottom: 2.5%; font-weight: 500; font-size: 3.5em; color: #FFF;",
    "GMH Data Hub"
  ),
  logo_bottom = htmltools::tags$img(
    src = file.path("www/images/gmh/logos/gmh-logo-white.svg"),
    alt = "GMH Communities",
    style = "width: 40%; margin-top: 2.5%;"
  )
)
