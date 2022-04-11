#
#   Create the ERT data
#   

devtools::load_all()

data(vdem)

episodes <- get_eps()

usethis::use_data(episodes, overwrite = TRUE)
