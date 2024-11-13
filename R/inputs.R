
#  ------------------------------------------------------------------------
#
# Title : Custom Shiny Inputs
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------


# properties --------------------------------------------------------------

# input_gmh_properties <- function(
#   id,
#   label = "Select Property:",
#   choices = metadata$app_choices$properties,
#   selected = metadata$app_choices$properties,
#   multiple = TRUE,
#   options = metadata$app_defaults$picker_options,
#   width = "auto",
#
#   shinyWidgets::pickerInput(
#     ns("properties"),
#     label = icon_text("building", "Select Properties"),
#     choices = .app_choices$properties,
#     multiple = TRUE,
#     options =
#   )
#
# }


# reports -----------------------------------------------------------------

input_gmh_reports <- function(
  id,
  label = "Select Report:",
  choices = metadata$app_choices$reports,
  selected = metadata$app_choices$reports,
  multiple = TRUE,
  options = metadata$app_defaults$picker_options,
  width = "auto"
) {
  shinyWidgets::pickerInput(
    inputId = id,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = options,
    width = width
  )
}

# password ----------------------------------------------------------------



# email -------------------------------------------------------------------



# otp code ----------------------------------------------------------------




