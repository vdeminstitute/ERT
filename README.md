

An R package to load, explore, and work with the Episodes of Regime Transformation (ERT) dataset - a project of the [V-Dem Institute](https://www.v-dem.net/).


## Episodes of Regime Transformation (ERT) ##

#### Load, explore, and work with the ERT dataset (for details see also the [ERT Codebook](https://github.com/vdeminstitute/ERT/blob/master/inst/ERT_codebook.pdf)): ####

* NOTE: for non-R users we provide [the ERT dataset here as csv. or .xlsx file](https://github.com/vdeminstitute/ERT/blob/master/inst) - however, we recommend loading the ERT dataset via the package since one huge advantage of the package is that it allows to flexibly set parameters for generating the episodes.
* RELEASES: ERT 15.0 is based on the V-Dem dataset v15. For earlier releases using earlier versions of the V-Dem dataset, see the "Releases" column on the right sight. 

#### Functions ####
* `get_eps`: Identify episodes of regime transitions (autocratization, democratization) in the most recent V-dem data set. Autocratization is defined as any movement towards autocracy which starts within democracies or autocracies [(cf. Maerz et al., Journal of Peace Research, 2023](https://journals.sagepub.com/doi/10.1177/00223433231168192) and [Lührmann and Lindberg, Democratization, 2019)](https://www.tandfonline.com/doi/full/10.1080/13510347.2019.1582029). Democratization is defined as any movement towards democracy which starts in autocracies or democracies [(cf. Wilson et al., 2022)](https://www.cambridge.org/core/journals/political-science-research-and-methods/article/episodes-of-liberalization-in-autocracies-a-new-approach-to-quantitatively-studying-democratization/CD86064BF11FEEC8BD9354921E3C9BE3)
* `find_overlap`: Find potential overlaps between episodes of democratization and autocratization which may occur depending on how the thresholds are set.
* `plot_episodes`: Plot Episodes of Regime Transitions (ERT) over time for a selected country.
* `plot_all`: Plot share or absolute number of all countries in Episodes of Regime Transitions (ERT) over time.

#### ShinyApp for validity tests ####

* For additional transparency, we provide a [ShinyApp for validation](https://episodes.shinyapps.io/validation/) which allows users to flexibly adjust the ERT parameters and to test how changes to the default thresholds affect the episodes and their validity.

## Installation ##

```
# Install the development version of the ERT package 
# (this package is an ongoing project, 
# keep checking for updates)

# First, you need to have the devtools package installed
install.packages("devtools")
# now, install the ERT package directly from GitHub
devtools::install_github("vdeminstitute/ERT")

# NOTE: make sure you have an updated R version and
# - since the package is a development version - 
# an updated version of xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to contact@v-dem.net at the V-Dem Institute.

```


