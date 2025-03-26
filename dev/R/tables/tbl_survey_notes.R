# reactable table showing textual notes with CRUD

tbl_survey_notes <- function() {
  tbl_survey_notes <- tibble::tibble(
    note_id = c(1, 2, 3, 4, 5),
    note_date = c("2025-01-01", "2025-01-02", "2025-01-03", "2025-01-04", "2025-01-05"),
    note_text = c(
      "This is a note about the property.",
      "This is a note about the leasing office.",
      "This is a note about the residents.",
      "This is a note about the property.",
      "This is a note about the leasing office."
    )
  )

  tbl_survey_notes
}
