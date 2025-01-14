
#  ------------------------------------------------------------------------
#
# Title : GMH Investment Partners
#    By : Jimmy Briggs
#  Date : 2025-01-09
#
#  ------------------------------------------------------------------------

gmh_partners_tbl <- tibble::tibble(
  partner_id = c(1, 2, 3, 4, 5, 6, 7),
  partner_name = c("CBRE", "AGC", "JHU", "AEW", "Principal", "CRG and Canyon", "Medistar Student Housing LLC"),
  partner_type = rep("Equity Partner", 7),
  partner_description = c("CBRE Properties", "AGC Properties", "JHU Properties", "AEW Properties", "Principal Properties", "CRG and Canyon Properties", "Medistar Student Housing LLC Properties"),
  partner_url = c("https://www.cbre.com", "https://www.agc.com", "https://www.jhu.com", "https://www.aew.com", "https://www.principal.com", "https://www.crg.com", "https://www.medistar.com")
)

gmh_partners_lst <- unique(gmh_partners_tbl$partner_name)
