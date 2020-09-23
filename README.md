

An R package to load, explore, and work with the Episodes of Regime Transformation (ERT) dataset - a project of the [V-Dem Institute](https://www.v-dem.net/en/).


## Episodes of Regime Transformation (ERT) ##

#### Load, explore, and work with the ERT dataset (for details see also the [ERT Codebook](https://github.com/vdeminstitute/ERT/blob/master/inst/ERT_codebook.pdf)): ####

* NOTE: for non-R users we provide [the ERT dataset here as csv. file](https://github.com/vdeminstitute/ERT/blob/master/inst) - however, we recommend loading the ERT dataset via the package since one huge advantage of the package is that it allows to flexibly set parameters for generating the episodes.

#### Functions ####
* `get_eps`: Identify episodes of regime transitions (autocratization, democratization) in the most recent V-dem data set. Autocratization is defined as any movement towards autocracy which starts within democracies or autocracies [(cf. Lührmann and Lindberg, Democratization, 2019)](https://www.tandfonline.com/doi/full/10.1080/13510347.2019.1582029). Democratization is defined as any movement towards democracy which starts in autocracies or democracies [(cf. Wilson et al., 2020)](https://www.v-dem.net/media/filer_public/40/01/40014eba-6f42-467c-b693-15221e210415/wp_97_final.pdf)
* `find_overlap`: Find potential overlaps between episodes of democratization and autocratization which may occur depending on how the thresholds are set.
* `fix_overlap`: Interactive function to fix potential overlaps between episodes of democratization and autocratization which may occur depending on how the thresholds are set.
* `plot_episode`: Plot Episodes of Regime Transitions (ERT) over time.

## Installation ##

```
# Install the development version of the ERT package 
# (since this package is still an ongoing project, 
# keep checking for updates, new functions, etc.!)

# First, you need to have the devtools package installed
install.packages("devtools")
# now, install the ERT package directly from GitHub
devtools::install_github("vdeminstitute/ERT")

# NOTE: make sure you have an updated R version (> 3.5) and
# - since the package is still a development version - 
# an updated version of xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to the package maintainer seraphine.maerz@v-dem.net
```


