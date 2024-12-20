
#  ------------------------------------------------------------------------
#
# Title : GMH/Entrata Properties Data
#    By : Jimmy Briggs
#  Date : 2024-11-09
#
#  ------------------------------------------------------------------------

property_ids <- c(

)

property_names <- c(

)

entrata_property_names <- c(

)

name_diffs <- tibble::tibble(
  gmh_property_name = c(
    "Campustown 1008 S. 4th",
    "Campustown 307 E. Daniel",
    "Campustown 501 S. 6th",
    "Campustown 908 S. 1st",
    "The Venue at North Campus"
  ),
  entrata_property_name = c(
    "1008 S. 4th",
    "307 E. Daniel",
    "501 S. 6th",
    "908 S. 1st",
    "Venue at North Campus"
  )
)

properties <- tibble::tribble(
  ~property_id,                  ~property_name_entrata,
  739076L,                   "1008 S. 4th",
  739085L,      "1047 Commonwealth Avenue",
  739079L,                 "307 E. Daniel",
  739080L,                    "501 S. 6th",
  739084L,                    "908 S. 1st",
  641240L,                    "Academy 65",
  676055L,               "Academy Lincoln",
  1115679L,            "ANOVA uCity Square",
  1161867L, "Courts at Spring Mill Station",
  518044L,              "Shortbread Lofts",
  952515L,                          "SOVA",
  577897L,                  "Station Nine",
  518041L,         "The Academy at Frisco",
  518042L,        "The Academy on Charles",
  1197886L,     "The Caswell at Runnymeade",
  833617L,           "The Dean Campustown",
  1197887L,                 "The Dean Reno",
  518046L,         "The Rise at Northgate",
  1143679L,                         "Torre",
  1311849L,         "Venue at North Campus"
)
