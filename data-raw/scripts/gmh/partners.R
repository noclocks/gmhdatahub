
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










#   tibble::tibble(
#   property_id = c(
#     "1143679",
#     "952515",
#     "1311849",
#     "739085",
#     "739079",
#     "739080",
#     "739084",
#     "739076",
#     "641240",
#     "518042",
#     NA,
#     "518041",
#     "676055",
#     NA,
#     "1115679",
#     "1197886",
#     NA,
#     "1161867",
#     "833617",
#     "1197887",
#     NA,
#     NA,
#     "518046",
#     "518044",
#     "577897",
#     NA
#   ),
#   property_name = c(
#     "Torre",
#     "SOVA",
#     "Venue at North Campus",
#     "1047 Commonwealth Avenue",
#     "307 E. Daniel",
#     "501 S. 6th",
#     "908 S. 1st",
#     "1008 S. 4th",
#     "Academy 65",
#     "The Academy on Charles",
#     "Academy Chorro",
#     "The Academy at Frisco",
#     "Academy Lincoln",
#     "Academy Palomar",
#     "ANOVA uCity Square",
#     "The Caswell at Runnymeade",
#     "Central Station",
#     "Courts at Spring Mill Station",
#     "The Dean Campustown",
#     "The Dean Reno",
#     "Life Tower",
#     "The Pendleton",
#     "The Rise at Northgate",
#     "Shortbread Lofts",
#     "Station Nine",
#     "Yards at Malvern"
#   ),
#   investment_partner = c(
#     "CBRE",
#     "CBRE",
#     "CBRE",
#     "AGC",
#     "AGC",
#     "AGC",
#     "AGC",
#     "AGC",
#     "AGC",
#     "JHU",
#     "AGC",
#     "AEW",
#     "AGC",
#     "AGC",
#     "CBRE",
#     "AEW",
#     NA,
#     "Principal",
#     "AGC",
#     "CRG and Canyon",
#     "Medistar Student Housing LLC",
#     "CBRE",
#     "AEW",
#     "Principal",
#     "Principal",
#     "AEW"
#   )
# )

gmh_partners_lst <- unique(gmh_partners_tbl$partner_name)
