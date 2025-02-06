html_tel <- function(phone) {
  phone_digits <- gsub("[^0-9]", "", phone)
  glue::glue("<a href='tel:{phone_digits}' target='_blank'>{phone}</a>")
}

format_address <- function(address) {
  address_parts <- strsplit(address, ", ")[[1]]
  street <- address_parts[1]
  city <- address_parts[2]
  state <- address_parts[3]
  zip <- address_parts[4]
  city_state_zip <- glue::glue("{city}, {state}, {zip}")
  glue::glue("{street}\n{city_state_zip}") |> cat()
}

format_phone_number <- function(phone) {
  phone_digits <- gsub("[^0-9]", "", phone)
  if (nchar(phone_digits) == 11) {
    phone_digits <- substr(phone_digits, 2, 11)
  }
  glue::glue("+1 ({substr(phone_digits, 1, 3)}) {substr(phone_digits, 4, 6)}-{substr(phone_digits, 7, 10)}")
}
