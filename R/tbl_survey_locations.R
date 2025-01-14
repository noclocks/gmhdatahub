tbl_survey_locations <- function(
  locations = NULL,
  properties = NULL,
  competitors = NULL,
  universities = NULL
) {

  # validate inputs
  validate_tbl_survey_locations(locations, properties, competitors, universities)

  # create survey locations table
  survey_locations <- tibble::tibble(
    location = locations,
    property = properties,
    competitor = competitors,
    university = universities
  )

  # return survey locations table
  survey_locations

}
