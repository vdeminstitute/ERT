#
#   Create the ERT data
#   

devtools::load_all()

data(vdem)

episodes <- get_eps()

# Fix some data types
episodes$country_id <- as.integer(episodes$country_id)
episodes$year <- as.integer(episodes$year)

usethis::use_data(episodes, overwrite = TRUE)
