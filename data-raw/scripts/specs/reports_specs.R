reports_specs <- list(
  "getReportList" = list(
    main = tibblify::tspec_df(
      tibblify::tib_int("id"),
      tibblify::tib_chr("reportName"),
      tibblify::tib_chr("systemName"),
      tibblify::tib_row(
        "reportVersions",
        tibblify::tib_df(
          "reportVersion",
          tibblify::tib_chr("version"),
          tibblify::tib_lgl("isLatest"),
          tibblify::tib_chr("titleAddendum", required = FALSE),
          tibblify::tib_chr("expiryDate", required = FALSE),
        )
      )
    ),
    versions = tibblify::tspec_row(
      tibblify::tib_chr("version"),
      tibblify::tib_lgl("isLatest"),
      tibblify::tib_chr("titleAddendum", required = FALSE),
      tibblify::tib_chr("expiryDate", required = FALSE)
    )
  ),
  "getReportInfo" = list()
)
