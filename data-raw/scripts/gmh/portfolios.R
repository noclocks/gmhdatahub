

#  ------------------------------------------------------------------------
#
# Title : GMH Portfolios
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_portfolios_tbl <- tibble::tibble(
  portfolio_id = c(1, 2, 3, 4, 5),
  portfolio_name = c("AGC", "CBRE", "CRG", "Medistar", "Principal"),
  portfolio_type = c("Equity Partner", "Equity Partner", "Equity Partner", "Owner", "Equity Partner"),
  portfolio_description = c("AGC Properties", "CBRE Properties", "CRG Properties", "Medistar Properties", "Principal Properties"),
  number_of_properties = c(10, 2, 1, 1, 4)
)

gmh_portfolios_lst <- as.list(gmh_portfolios_tbl$portfolio_id) |>
  setNames(gmh_portfolios_tbl$portfolio_name)
