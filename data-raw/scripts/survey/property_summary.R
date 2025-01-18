#  ------------------------------------------------------------------------
#
# Title : Property Summary
#    By : Jimmy Briggs
#  Date : 2025-01-14
#
#  ------------------------------------------------------------------------

property_summary_inputs_tbl <- tibble::tibble(
  order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
  name = c(
    "Property Name",
    "Property Website",
    "Property Address",
    "Property Phone Number",
    "Property Developer",
    "Property Manager",
    "Property Owner",
    "Property Type",
    "Property Status",
    "Product Type",
    "Property Rating",
    "Comp Status",
    "Year Built/Renovated",
    "Most Recent Sale Date",
    "Distance to Campus"
  ),
  id = c(
    "property_name",
    "property_website",
    "property_address",
    "property_phone_number",
    "property_developer",
    "property_manager",
    "property_owner",
    "property_type",
    "property_status",
    "product_type",
    "property_rating",
    "comp_status",
    "year_built_renovated",
    "most_recent_sale_date",
    "distance_to_campus"
  ),
  label = c(
    "Property Name",
    "Property Website",
    "Property Address",
    "Property Phone Number",
    "Property Developer",
    "Property Manager",
    "Property Owner",
    "Property Type",
    "Property Status",
    "Product Type",
    "Property Rating",
    "Comp Status",
    "Year Built/Renovated",
    "Most Recent Sale Date",
    "Distance to Campus"
  ),
  placeholder = c(
    "Enter Property Name",
    "Enter Property Website",
    "Enter Property Address",
    "Enter Property Phone Number",
    "Enter Property Developer",
    "Enter Property Manager",
    "Enter Property Owner",
    "Select Property Type",
    "Select Property Status",
    "Select Product Type",
    "Enter Property Rating",
    "Select Comp Status",
    "Enter Year Built/Renovated",
    "Enter Most Recent Sale Date",
    "Enter Distance to Campus"
  ),
  icon = c(
    "building",
    "globe",
    "map-marker-alt",
    "phone",
    "hard-hat",
    "user-tie",
    "user",
    "check",
    "building",
    "star",
    "check-circle",
    "calendar-alt",
    "dollar-sign",
    "ruler-horizontal",
    "map-marker-alt"
  ),
  help = c(
    "Enter the name of the property.",
    "Enter the website of the property.",
    "Enter the address of the property.",
    "Enter the phone number of the property.",
    "Enter the developer of the property.",
    "Enter the manager of the property.",
    "Enter the owner of the property.",
    "Select the type of property.",
    "Select the status of the property.",
    "Select the product type of the property.",
    "Enter the rating of the property.",
    "Select the comp status of the property.",
    "Enter the year the property was built.",
    "Enter the most recent sale date of the property.",
    "Enter the distance from campus in miles."
  ),
  type = c(
    "text",
    # Property Name
    "text",
    # Property Website
    "text",
    # Property Address
    "text",
    # Property Phone Number
    "text",
    # Property Developer
    "text",
    # Property Manager
    "text",
    # Property Owner
    "mc",
    # Property Type
    "mc",
    # Property Status
    "mc",
    # Product Type
    "numeric",
    # Property Rating
    "mc",
    # Comp Status
    "date",
    # Year Built
    "date",
    # Most Recent Sale Date
    "numeric" # Distance to Campus
  ),
  required = c(
    TRUE,
    # Property Name
    FALSE,
    # Property Website
    TRUE,
    # Property Address
    TRUE,
    # Property Phone Number
    FALSE,
    # Property Developer
    FALSE,
    # Property Manager
    FALSE,
    # Property Owner
    TRUE,
    # Property Type
    TRUE,
    # Property Status
    TRUE,
    # Product Type
    TRUE,
    # Property Rating
    TRUE,
    # Comp Status
    TRUE,
    # Year Built
    FALSE,
    # Most Recent Sale Date
    TRUE # Distance to Campus
  ),
  choices = NULL,
  default = c(
    NA,
    # Property Name
    NA,
    # Property Website
    NA,
    # Property Address
    NA,
    # Property Phone Number
    NA,
    # Property Developer
    "GMH Communities",
    # Property Manager
    "GMH Communities",
    # Property Owner
    "Student",
    # Property Type
    "Operational",
    # Property Status
    "Mid-Rise",
    # Product Type
    "3",
    # Property Rating
    "Subject Property",
    # Comp Status
    NA,
    # Year Built
    NA,
    # Most Recent Sale Date
    NA # Distance to Campus
  )
) |>
  dplyr::mutate(
    choices = dplyr::case_when(
      id == "property_type" ~ 'c("Student", "Conventional", "Affordable", "Innovative")',
      id == "property_status" ~ 'c("Operational", "New Construction", "Undergoing Renovations")',
      id == "product_type" ~ 'c("High-Rise", "Mid-Rise", "Wrap", "Garden", "Cottage", "SFR")',
      id == "comp_status" ~ 'c("Subject Property", "Tier 1", "Tier 2")',
      TRUE ~ NA_character_
    )
  )
