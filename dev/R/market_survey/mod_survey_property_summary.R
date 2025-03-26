
card_property_field_group <- function(id, title, ...) {

  bslib::card(
    bslib::card_header(title),
    bslib::card_body(...),
    class = "field-group"
  )

}

