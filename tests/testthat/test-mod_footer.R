# tests/testthat/test-mod_footer.R

test_that("mod_footer_ui returns a shiny.tag.list", {
  ui <- mod_footer_ui(id = "footer_test")
  expect_s3_class(ui, "card_item")
  expect_s3_class(ui, "shiny.tag")
})

test_that("mod_footer_ui includes required elements", {
  ui <- mod_footer_ui(id = "footer_test")

  # Convert UI to HTML for easier searching
  ui_html <- as.character(htmltools::renderTags(ui)$html)

  # Check for the presence of the footer element
  expect_true(grepl("<footer", ui_html))

  # Check for developer logo image
  expect_true(grepl("noclocks-logo-black.svg", ui_html))

  # Check for client logo image
  expect_true(grepl("gmh-logo.svg", ui_html))
})

test_that("validate_image function works correctly", {
  # Assuming you have access to the validate_image function
  # If not, you may need to export it or test via the UI function

  # Valid image path
  valid_img <- "www/images/shared/app/logos/app-logo.svg"
  result <- validate_image(valid_img)
  expect_equal(result, valid_img)

  # Invalid image path
  invalid_img <- "www/img/logos/nonexistent-image.svg"
  expect_warning(validate_image(invalid_img), "Image not found")
  result <- suppressWarnings(validate_image(invalid_img))
  expect_equal(result, "www/images/shared/placeholders/default-image.png")
})
