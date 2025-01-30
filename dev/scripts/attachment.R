require(attachment)

# move sysdata.rda from R/ to dev/
fs::file_move("R/sysdata.rda", "dev/sysdata.rda")

# run attachment
attachment::att_amend_desc()
devtools::document()

# move sysdata back
fs::file_move("dev/sysdata.rda", "R/sysdata.rda")
devtools::document()
pkgload::load_all()
