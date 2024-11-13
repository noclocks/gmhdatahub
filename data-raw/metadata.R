
#  ------------------------------------------------------------------------
#
# Title : Metadata
#    By : Jimmy Briggs
#  Date : 2024-11-11
#
#  ------------------------------------------------------------------------


# application metadata ----------------------------------------------------

app_metadata <- list(
  ui = list(
    labels = list(
      title = "App Title",
      sidebar = "App Sidebar",
      inputs = "Custom Shiny Inputs"
    ),
    input_choices = list(
      properties = c("Property 1", "Property 2", "Property 3"),
      reports = c("Report 1", "Report 2", "Report 3")
    )
  ),
  validation = list(
    required_fields = c("name", "email"),
    numeric_ranges = list(
      age = c(0, 120)
    )
  )
)
