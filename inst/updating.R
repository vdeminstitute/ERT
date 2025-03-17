
# this script is about the yearly updating process of the package
# (vdem, codebook)

# PREP: fork the package / update your fork on your personal rep
# pull latest package version to your local RStudio/git (version control set up)
# do the following updates

# load the new vdem dataset and save it as RData in the package folder "data"
# vdem
vdem <- vdemdata::vdem
save("vdem", file = "data/vdem.RData")

# load vdem codebook
# save as RData in the package folder "data"
codebook <- vdemdata::codebook
save("codebook", file = "data/codebook.RData")

# update ERT codebook online (overleaf)
# save it as pdf in the /inst folder of the package

# document new package version
Rcpp::compileAttributes() 
devtools::document()
devtools::clean_dll()

# save dataset as csv in "inst" folder
episodes <- get_eps()
stopifnot(max(episodes$year) == max(vdem$year))
save("episodes", file = "data/episodes.rda")
# provide the data also as .cvs and .xlsx files for non R-users
write.csv(episodes,"inst/ert.csv")
library("xlsx")
library("rJava")
write.xlsx(episodes, file = "inst/ert.xlsx", sheetName = "episodes", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

# do any additional changes/updates of the scripts in
# package folder /R or DESCRIPTION or README.md if required
# e.g. update citation of datasets, package, etc.

# document again and check package
Rcpp::compileAttributes() 
devtools::document()
devtools::check()

# optional/sometimes needed
devtools::clean_dll()


# push to your personal rep
# do a pull request to the original rep

#### Release #####
# Go to the repository on Github at https://github.com/vdeminstitute/ERT

# Click on "releases" on the right; you see a list with all previous releases

# Click on "Draft a new release"

# Choose a new tag for the release (e.g., V4.0, V4.1, V5.0)

# Select a title for the release and adjust the description 
# (mostly copy-paste from last release to keep it consistent)
# You can use the "save draft" and "preview" buttons to make sure the release looks good

# Click on "publish release"

# all done and up-to-date :-)
