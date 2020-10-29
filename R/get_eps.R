#' Get episodes of regime transformation (ERT)
#'
#' Helps to identify episodes of democratization (liberalizing autocracy, democratic deepening) 
#' and autocratization (democratic regression, autocratic regression) in the most recent vdem data set. 
#' For further details please check the ERT codebook attached to the package 
#' and available here: https://github.com/vdeminstitute/ERT/blob/master/inst/ERT_codebook.pdf
#'
#' \emph{Democratization} is an umbrella term for any movement towards demcracy - 
#' be it in autocracies or democracies (cf. Wilson et al., 2020).
#' \emph{Liberalizing autocracy} is defined as a subtype of democratiztion and specifically focuses on any movement towards democracy
#' which starts in autocracies. \emph{Democratic deepening} is also a subtype of democratization and
#' concerns all those which are already democratic and further improve their democratic traits.
#'
#' \emph{Autocratization} is defined as any movement towards autocracy which starts within democracies 
#' or autocracies (cf. Lührmann and Lindberg, Democratization, 2019).
#' \emph{Democratic regression} is defined as a subtype of autocratization and specifically focuses on any movement towards autocracy
#' which starts in democracies. \emph{Autocratic regression} is also a subtype of autocratization and
#' concerns all those which are already autocratic and further decline.
#'
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set.
#'
#' @param start_incl What is the minimum annual change on V-Dem's Electoral Democracy Index (EDI) necessary to trigger an episode? 
#' This is the absolute value of the first difference in the EDI required for the onset 
#' of either a democratization (+) or autocratization episode (–).
#'
#' @param cum_incl What is the minimum amount of total change on the EDI necessary to constitute a manifest episode?
#' A potential episode might be a period involving any amount of changes over a period following an annual change equal 
#' to the start inclusion (e.g. 0.01). To identify substantial changes, we set a cumulative inclusion threshold. 
#' This is the absolute value of the total amount of change needed on the EDI to be considered manifest.
#'
#' @param year_turn What is the amount of annual change in the opposite direction to trigger the termination of an episode? 
#' An episode may end when the case suddenly moves in the opposite direction. 
#' For example, during an episode of democratization, a country may experience a sudden drop on the EDI. 
#' This could signal the onset of an autocratization episode. To avoid overlap between episodes, 
#' we set the absolute value of a change in the opposite direction on the EDI 
#' as a trigger for the termination of an episode. \emph{Note: Advanced users who wish to remove this criteria altogether 
#' should set the value of year\_turn equal to cum\_turn. 
#' Setting this to zero would allow for an episode to terminate when any year of no change is encountered.}
#' 
#' @param cum_turn What is the amount of gradual change in the opposite direction to trigger the termination of an episode?
#' An episode may end when the case begins moving in the opposite direction gradually. For example, 
#' during an episode of democratization, a country may experience a gradual drop on the EDI over a number of years 
#' that signals democratization has ended. This could also signal the onset of an autocratization episode. 
#' To avoid overlap between episodes, we set the absolute value of a gradual change in the opposite direction 
#' on the EDI over the tolerance period (e.g. 5 years) as a trigger for the termination of an episode.
#'
#' @param tolerance What is the number of years considered as tolerance for stasis or a gradual movement in the opposite direction?
#' The tolerance defines the number of years an episode is allowed to remain in stasis 
#' (i.e. no more movements equal to the start inclusion) and/or move in the opposite direction before it is terminated. 
#' This parameter also defines the number of years necessary for a case to be considered a democratic breakdown or 
#' stabilized electoral autocracy. \emph{Therefore, care is necessary when manipulating the default value. 
#' This could lead to large changes in the composition of episodes. 
#' We set the default to 5 years because this is the typical amount for an electoral cycle for most countries.}
#'
#' @return A data frame specifying episodes of regime transformation and their outcomes in the most recent V-Dem data set.
#' For further details and explanations on episodes and outcomes please check the ERT codebook attached to the package 
#' and available here: https://github.com/vdeminstitute/ERT/blob/master/inst/ERT_codebook.pdf
#'
#' @import dplyr
#' @import Rcpp
#' @importFrom hablar s
#' @import tidyr
#' @importFrom plm make.pconsecutive
#' @export
#'
#' @examples
#' #Don't run
#' #Get the episodes with standard parameters:
#' #episodes <- get_eps()
#'
### set the parameters ###
get_eps <- function(data = ERT::vdem,
                    start_incl = 0.01,
                    cum_incl = 0.1,
                    year_turn = 0.03,    
                    cum_turn = 0.1,
                    tolerance = 5)
{
  
  if(year_turn == 0)
    print("You set year_turn = 0. Did you mean to do this? Doing so means an episode ends when it experiences a year of no annual change on the EDI. Perhaps, instead, you meant to set its value equal to cum_turn. See p.3 of the ERT codebook.")
  
 
  ### DATA CLEANING AND PREP ###
  
  # selecting the variables we need to construct the episodes dataframe 
  full.df <- data %>%
    dplyr::select(country_name, country_id, country_text_id, year,
                  v2x_polyarchy, codingstart, codingend, matches("v2x_polyarchy", ignore.case = FALSE),
                  gapstart1, gapstart2, gapstart3, gapend1, gapend2, gapend3,
                  v2x_regime, matches("v2eltype", ignore.case = FALSE), v2elasmoff_ord) %>%
    dplyr::filter(year >= 1900) %>%
    dplyr::arrange(country_text_id, year) %>%
    dplyr::group_by(country_id) %>%
    # make codingstart 1900 or first year thereafter
    dplyr::mutate(codingstart2 = min(hablar::s(ifelse(!is.na(v2x_regime), year, NA))),
                  # tag original sample for later use
                  origsample = 1) %>%
    # we need to deal with gaps in v-dem coding
    # this balances the dataset
    plm::make.pconsecutive(balanced = TRUE, index = c("country_id", "year")) %>%
    dplyr::group_by(country_id) %>%
    # this fills missing variables we need that are constant within countries
    tidyr::fill(c(country_text_id, country_name, codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3)) %>%
    tidyr::fill(c(country_text_id, country_name,codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3), .direction = "up")  %>%
    # here we need to recode the gaps as only during the period prior to and during the gap (for our "uncertain" variables)
    dplyr::mutate(gapstart = ifelse(year <= gapend1, gapstart1, NA),
                  gapend = ifelse(year <= gapend1, gapend1, NA),
                  gapstart = ifelse(!is.na(gapend2) & year > gapend1 & year <= gapend2, gapstart2, gapstart),
                  gapend = ifelse(!is.na(gapend2) & year > gapend1 & year <= gapend2, gapend2, gapend),
                  gapstart = ifelse(!is.na(gapend3) & year > gapend2 & year <= gapend3, gapstart3, gapstart),
                  gapend = ifelse(!is.na(gapend3) & year > gapend2 & year <= gapend3, gapend3, gapend)) %>%
    
    
    ### CODING THE REGIME TYPE VARIABLES ###
    
    dplyr::arrange(country_id, year) %>%
    # here we code whether a regime change event on RoW occurred in the given country year, 1 = to democracy, -1 = to autocracy
    dplyr::mutate(row_regch_event = ifelse(v2x_regime > 1 & dplyr::lag(v2x_regime < 2, n = 1), 1, 0),
                  row_regch_event = ifelse(v2x_regime < 2 & dplyr::lag(v2x_regime > 1, n = 1), -1, row_regch_event),
                  # here we code the year of the most recent RoW regime change event
                  row_regch_year = ifelse(row_regch_event == -1 | row_regch_event == 1, year, NA),
                  # here we code the filled regime change variable, telling us what was the type of the most recent RoW regime change
                  row_regch_filled = ifelse(!is.na(row_regch_year), row_regch_event, NA)) %>%
    # intially we fill everything
    tidyr::fill(c(row_regch_filled, row_regch_year)) %>%
    # here we replace with NA for gaps
    dplyr::mutate(row_regch_filled = ifelse(!is.na(row_regch_year) & ((!is.na(gapend1) & row_regch_year<gapstart1 & year>=gapstart1) |
                                                                        (!is.na(gapend2) & row_regch_year<gapstart2 & year>=gapstart2) |
                                                                        (!is.na(gapend3) & row_regch_year<gapstart3 & year>=gapstart3)),
                                            NA, row_regch_filled),
                  row_regch_year = ifelse(is.na(row_regch_filled), NA, row_regch_year)) %>%
    ungroup() %>%
    group_by(country_id, row_regch_year) %>%
    # here we check whether the RoW regime change is censored
    # censored near end of coding
    dplyr::mutate(row_regch_censored = ifelse(codingend - row_regch_year < tolerance, 1, 0),
                  # censored near gap
                  row_regch_censored = ifelse(!is.na(gapstart) & gapstart - row_regch_year < tolerance, 1, row_regch_censored),
                  # here we check to see if a regime change to democracy produced a founding election
                  dem_founding_elec = min(hablar::s(ifelse(v2x_regime > 1 & year >= row_regch_year & v2elasmoff_ord > 1 &
                                                             # must hold leg, exec, or CA election
                                                             (v2eltype_0 == 1 | v2eltype_4 == 1 | v2eltype_6 == 1),
                                                           year, NA))),
                  row_demtrans_dum = ifelse(row_regch_event == 1 & !is.na(dem_founding_elec), 1, NA),
                  row_demtrans_dum = ifelse(row_regch_event == 1 & is.na(dem_founding_elec), 0, row_demtrans_dum),
                  row_regch_censored = ifelse(row_demtrans_dum == 1, 0, row_regch_censored),
                  row_demtrans_dum = ifelse(row_regch_censored == 1 & row_demtrans_dum == 0, NA, row_demtrans_dum),
                  
                  # here we check to see if a regime change to autocracy produced a democratic breakdown
                  # we start by looking for autocratic founding elections
                  aut_founding_elec = min(hablar::s(ifelse(v2x_regime==1 & year>=row_regch_year &
                                                             # must hold leg, exec, or CA election
                                                             (v2eltype_0 == 1 | v2eltype_4 ==1 | v2eltype_6 ==1),
                                                           year, NA))),
                  # we also check if it remained autocratic for the tolerance period
                  aut_stabilized = min(hablar::s(ifelse(v2x_regime==1 & year==row_regch_year &
                                                          dplyr::lead(v2x_regime==1, n=tolerance), 1, NA))),
                  # finally if it became closed
                  aut_closed = ifelse(row_regch_event==-1,1-min(hablar::s(v2x_regime)),NA),
                  # check to see if any of the above conditons hold
                  row_breakdown_dum = ifelse(row_regch_event==-1 & (!is.na(aut_founding_elec) |
                                                                      (!is.na(aut_stabilized) & aut_stabilized==1) |
                                                                      (!is.na(aut_closed) & aut_closed==1)), 1, NA),
                  row_breakdown_dum = ifelse(row_regch_event == -1 & is.na(row_breakdown_dum), 0, row_breakdown_dum),
                  row_regch_censored = ifelse(!is.na(row_breakdown_dum) & row_breakdown_dum==1, 0, row_regch_censored),
                  row_breakdown_dum = ifelse(!is.na(row_regch_censored) & row_regch_censored==1, NA, row_breakdown_dum)) %>%
    # here we code the regimes based on our criteria for democracy and autocracy
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # year the country transitioned to democracy on RoW provided it held a founding election
    dplyr::mutate(reg_start_year=ifelse(!is.na(dem_founding_elec) & row_regch_event==1, year, NA),
                  # year the country transitioned to autocracy on RoW provided closed, or electoral autocracy persisted or held election
                  reg_start_year=ifelse(!is.na(row_breakdown_dum) & row_breakdown_dum==1, year, reg_start_year),
                  # here we coding founding as first year observed
                  reg_start_year = ifelse(year==codingstart2, year, reg_start_year),
                  # here we code founding as first year observed after a gap
                  reg_start_year = ifelse(!is.na(gapend1) & year==gapend1+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend2) & year==gapend2+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend3) & year==gapend3+1, year, reg_start_year)) %>%
    tidyr::fill(reg_start_year) %>%
    dplyr::mutate(reg_start_year = ifelse(!is.na(reg_start_year) & ((!is.na(gapend1) & reg_start_year<gapstart1 & year>=gapstart1) |  # here we replace with NA for gaps
                                                                      (!is.na(gapend2) & reg_start_year<gapstart2 & year>=gapstart2) |
                                                                      (!is.na(gapend3) & reg_start_year<gapstart3 & year>=gapstart3)),
                                          NA, reg_start_year)) %>%
    ungroup() %>%
    group_by(country_id, reg_start_year) %>%
    # regime type is democracy (1) if v2x_regime is democratic in starting year
    dplyr::mutate(reg_type = ifelse(year == reg_start_year & v2x_regime > 1, 1, NA),
                  # regime type is autocratic (0) if v2x_regime is autocratic in starting year
                  reg_type = ifelse(year == reg_start_year & v2x_regime < 2, 0, reg_type),
                  # fill for entire regime period
                  reg_type = min(hablar::s(reg_type))) %>%
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # here we look for years where democratic becomes autocratic or vice versa
    dplyr::mutate(reg_trans = ifelse(!is.na(reg_type), reg_type - dplyr::lag(reg_type, n=1), NA),
                  # then we need to recode the starting years based on actual regime changes
                  reg_start_year = ifelse(!is.na(reg_trans) & reg_trans!=0, year, NA),
                  # here we coding founding as first year observed
                  reg_start_year = ifelse(year==codingstart2, year, reg_start_year),
                  # here we code founding as first year observed after a gap
                  reg_start_year = ifelse(!is.na(gapend1) & year==gapend1+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend2) & year==gapend2+1, year, reg_start_year),
                  reg_start_year = ifelse(!is.na(gapend3) & year==gapend3+1, year, reg_start_year)) %>%
    tidyr::fill(reg_start_year) %>%
    # here we replace with NA for gaps
    dplyr::mutate(reg_start_year = ifelse(!is.na(reg_start_year) & ((!is.na(gapend1) & reg_start_year<gapstart1 & year>=gapstart1) |
                                                                      (!is.na(gapend2) & reg_start_year<gapstart2 & year>=gapstart2) |
                                                                      (!is.na(gapend3) & reg_start_year<gapstart3 & year>=gapstart3)),
                                          NA, reg_start_year)) %>%
    ungroup() %>%
    group_by(country_id, reg_start_year) %>%
    # here we code the end of the regime
    dplyr::mutate(reg_end_year = dplyr::last(year),
                  # here we code the id for the regime
                  reg_id = ifelse(!is.na(reg_start_year), paste(country_text_id, reg_start_year, reg_end_year, sep = "_"), NA),
                  # here we recode the demtrans and breakdown dummies based on actual regime changes
                  row_demtrans_dum = ifelse(reg_trans==0 | is.na(reg_trans), 0, row_demtrans_dum),
                  row_breakdown_dum = ifelse(reg_trans==0 | is.na(reg_trans), 0, row_breakdown_dum),
                  # here we create a founding election variable for democratic regimes
                  founding_elec = min(hablar::s(dem_founding_elec))) %>%
    ungroup() %>%
    # make sure the data are sorted and grouped properly before sending to C++
    arrange(country_text_id, year) %>%
    group_by(country_text_id) %>%
    
    
    ### CODING THE DEMOCRATIZATION EPISODES ###
  
  # detect and save potential episodes with the help of the c++ function find_seqs
  dplyr::mutate(episode_id = find_seqs_dem(v2x_polyarchy, v2x_regime, reg_trans,
                                           start_incl, year_turn = year_turn * -1, cum_turn = cum_turn * -1,
                                           tolerance),
                # set a temporary id for these potential episodes and group accordinly
                character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(character_id) %>%
    # general check: is there a potential democratization episode?
    dplyr::mutate(dem_ep = ifelse(!is.na(episode_id), 1, 0),
                  # we check whether the cumulated change in each potential episode was substantial (> cum_inc), 
                  # i.e. the episode is manifest
                  dem_ep = ifelse(dem_ep==1 & max(v2x_polyarchy, na.rm = T) - min(v2x_polyarchy, na.rm = T) >= cum_incl, 1, 0)) %>%
    dplyr::ungroup() %>%
    # then we clean out variables for non-manifest episodes
    dplyr::mutate(episode_id = ifelse(dem_ep!=1, NA, episode_id),
                  character_id = ifelse(dem_ep!=1, NA, character_id)) %>%
    dplyr::group_by(character_id) %>%
    # generate the initial end year for the episode (note:  we have to filter out the stasis years that C++ gives us, but we will do this later):
    dplyr::mutate(dem_ep_end_year = ifelse(dem_ep==1, last(year), NA),
                  #  find potential episodes with uncertain outcome (note: we might change this later depending on termination)
                  dem_ep_uncertain = ifelse(dem_ep==1 & codingend-dem_ep_end_year<tolerance, 1, 0),
                  dem_ep_uncertain = ifelse(dem_ep==1 & !is.na(gapstart) & (gapstart-1)-dem_ep_end_year<tolerance, 1, dem_ep_uncertain),
                  # generate the start year for the potential episode as the first year after the pre-episode year
                  dem_ep_start_year = ifelse(dem_ep==1,first(year)+1, NA),
                  # here we code a dummy for the pre-episode year
                  dem_pre_ep_year = ifelse(dem_ep==1, ifelse(year == dplyr::first(year), 1, 0), 0),
                  # we create a unique identifier for episodes using the country_text_id, start, and end years
                  dem_ep_id = ifelse(dem_ep==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    # remove the old identifiers we no longer need
    dplyr::select(-character_id, -episode_id) %>%
    # make sure the data is sorted properly
    dplyr::arrange(country_name, year) %>%
    # just to make sure we have a dataframe
    as.data.frame %>%
    
    
    # code termination type of democratization episode
    
    # democratization episodes end when one of five things happens:
    # 0. the outcome is unknown
    # 1. stasis: the case experiences no annual increase = start_incl for the tolerance period (or more)
    # 2. year drop: the case experiences an annual drop <= year_turn
    # 3. cumulative drop: the case experiences a gradual drop <= cum_turn over the tolerance period (or less)
    # 4. breakdown: the case experienced a democratic breakdown (really only applies for subtype 1: democratic deepening) or
    #                reverted to closed authoritarianism (applies to all cases, but reg_trans should capture for subtype 1)
  
  # first find the last positive change on EDI equal to the start_incl parameter
  # this will become the new end of episodes at some point, once we clean things up
  dplyr::group_by(dem_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)>=start_incl, year, NA))),
                  # here we just replace with NA non-episode years
                  last_ch_year = ifelse(dem_ep==0, NA, last_ch_year)) %>%
    
    
    # here we check for breakdown within the episode period (termination type #4)
    # first lets make sure to group by the country (not the episode!) and arrange by country-year
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year) %>%
    # then we find years where a country moved from higher values on RoW to closed (0)
    dplyr::mutate(breakdown = ifelse((dplyr::lead(v2x_regime, n=1) == 0 & v2x_regime > 0) |
                                       # and the years where the country experienced a breakdown on reg_trans
                                       dplyr::lead(reg_trans==-1), year, NA)) %>%
    # now we need to group by episode to fill the values within episodes
    dplyr::ungroup() %>%
    dplyr::group_by(dem_ep_id) %>%
    # here we then find the first time in the episode that a breakdown occurs
    # limits breakdown to episode years, making sure to exclude pre-episode year
    dplyr::mutate(breakdown = ifelse(dem_ep==1 & year>=dem_ep_start_year, breakdown,NA),
                  # finds the first year within the episode where back_closed happens
                  breakdown = min(hablar::s(breakdown)),
                  # we recode the potential end date for the episode as the year before breakdown
                  dem_ep_end_year = ifelse(!is.na(breakdown) & dem_ep_end_year>breakdown, breakdown, dem_ep_end_year)) %>%
    # then we need to update our dem_ep_id with the new end date (we can clean up the other variables later)
    dplyr::ungroup() %>%
    dplyr::mutate(dem_ep_start_year = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_start_year),
                  dem_ep_end_year = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_end_year),
                  dem_ep = ifelse(dem_ep==1 & year>dem_ep_end_year, 0, dem_ep),
                  dem_ep_id = ifelse(dem_ep==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), NA)) %>%
    # then we can update our last_ch_year variable to reflect the new range of years for the episodes that terminated due to breakdown
    dplyr::group_by(dem_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)>=start_incl, year, NA))),
                  # here we just replace with NA non-episode years
                  last_ch_year = ifelse(dem_ep==0, NA, last_ch_year)) %>%
    
    # now lets make sure to group by the country (not the episode!) and arrange by country-year
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year)
  
  # then check to see what happened after the episode had its last substantive change equal to start_incl
  # we start with the yearly drop, aka year_turn
  year_drop <- list()
  # here we loop over the number of years (n) equal to the tolerance period after the last_change_year
  for (i in 1:tolerance) {
    # we calculate the first difference in the EDI for each of these yearly changes within the tolernce
    year_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-dplyr::lead(full.df$v2x_polyarchy, n=i-1), NA)
  }
  # then we generate a dataframe from these calculations
  df1 <- do.call(cbind, lapply(year_drop, data.frame, stringsAsFactors=FALSE))
  # this just renames the columns to match the years ahead we are looking
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  # this transforms the result into a dataframe that we can use as a column in our existing dataframe
  # first write a small function to deal with Inf
  my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)
  year_drop <- df1 %>%
    dplyr::mutate(year_drop = ifelse(apply(df1, 1, FUN = my.min) < year_turn*-1, 1,NA))  %>%
    dplyr::select(year_drop)
  # now we can also use the first-differences we calculated above to look for stasis as well
  # note - we will have to clean this up later to account for cum_turn as well
  stasis <- df1 %>%
    # this checks whether the maximum annual change is less than start_incl over the tolerance period &
    #    that it is also greater than the year_turn parameter, i.e. stasis
    dplyr::mutate(stasis = ifelse(apply(df1, 1, FUN = max) < start_incl & apply(df1, 1, FUN = min) >= year_turn*-1, 1,NA))  %>%
    dplyr::select(stasis)
  
  # now we look for a gradual drop equal to cum_drop over the tolerance period
  cum_drop <- list()
  # here we loop over time equal to the tolerance, looking for the difference between the last_change_year and that year on the EDI
  for (i in 1:tolerance) {
    cum_drop[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-full.df$v2x_polyarchy, NA)
  }
  # then we rename the columns and generate a dataframe we can use for our existing data
  df <- do.call(cbind, lapply(cum_drop, data.frame, stringsAsFactors=FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_drop <- df %>%
    dplyr::mutate(cum_drop = ifelse(apply(df, 1, FUN = my.min) <= cum_turn*-1, 1,NA)) %>%
    dplyr::select(cum_drop)
  
  # merge these new columns to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_drop, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(stasis, 'newid'), by = 'newid') %>%
    dplyr::select(-newid) %>%
    
    # now we can finally code our termination variable
    # first we group by episode
    dplyr::group_by(dem_ep_id) %>%
    dplyr::arrange(dem_ep_id, year) %>%
    # first, lets fill everything in for the episode
    dplyr::mutate(stasis = ifelse(dem_ep==1, max(hablar::s(stasis)), NA),
                  year_drop = ifelse(dem_ep==1, max(hablar::s(year_drop)), NA),
                  cum_drop = ifelse(dem_ep==1, max(hablar::s(cum_drop)), NA),
                  # then we can code the termination variable
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(stasis) & is.na(year_drop) & is.na(cum_drop)
                                              & is.na(breakdown), 1, NA),
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(year_drop) & is.na(breakdown), 2, dem_ep_termination),
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(cum_drop) & is.na(year_drop) & is.na(breakdown), 3, dem_ep_termination),
                  dem_ep_termination = ifelse(dem_ep==1 & !is.na(breakdown), 4, dem_ep_termination),
                  dem_ep_termination = ifelse(dem_ep==1 & dem_ep_uncertain==1 & is.na(dem_ep_termination), 0, dem_ep_termination),
                  # now we can clean up the other variables to reflect the true end of the episodes that have a known outcome
                  # first, let's fix the "uncertain" variable
                  dem_ep_uncertain = ifelse(dem_ep_termination !=0 & dem_ep==1, 0, dem_ep_uncertain),
                  # then we recode the end year as the final positive change if not uncertain
                  dem_ep_end_year = ifelse(dem_ep_uncertain==0 & dem_ep==1, last_ch_year, dem_ep_end_year),
                  # then we clean up the other variables for non-episode years
                  dem_ep_termination = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_termination),
                  dem_ep_start_year = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_start_year),
                  dem_ep_end_year = ifelse(dem_ep==1 & year>dem_ep_end_year, NA, dem_ep_end_year),
                  dem_ep = ifelse(is.na(dem_ep_end_year), 0, dem_ep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dem_ep_id = ifelse(dem_ep==1, paste(country_text_id, dem_ep_start_year, dem_ep_end_year, sep = "_"), NA)) %>%
    

  # here we generate a variable capturing whether the episode had the potential for a regime change 
  dplyr::group_by(dem_ep_id) %>%
    # to do so we look for the regime type in the pre-episode year, and if it was autocratic, we consider this episode to have the potential for regime change
    dplyr::mutate(dem_ep_prch = ifelse(!is.na(dem_ep_id), 1-dplyr::first(reg_type, order_by= year), 0),
                  # did a regime change on RoW during the episode produce a genuine democratic transition?
                  dem_ep_outcome = ifelse(dem_ep_prch==1 & reg_trans==1 & dem_pre_ep_year==0, 1, NA),
                  # did a regime change on RoW during the episode fail to produce a democratic transition?
                  dem_ep_outcome = ifelse(dem_ep_prch==1 & any(row_regch_event==1 & dem_pre_ep_year==0) &
                                            year==dem_ep_end_year & dem_ep_uncertain==0 &
                                            is.na(dem_ep_outcome), 2, dem_ep_outcome),
                  # did the autocratic liberalization phase result in a stabilized electoral autocracy?
                  dem_ep_outcome = ifelse(dem_ep_prch==1 & year==dem_ep_end_year & dem_ep_termination==1
                                          & is.na(dem_ep_outcome), 3, dem_ep_outcome),
                  # did the autocratic liberalization phase result in a failed liberalization?
                  dem_ep_outcome = ifelse(dem_ep_prch==1 & year==dem_ep_end_year &
                                            (dem_ep_termination==2 | dem_ep_termination==3 | dem_ep_termination==4) &
                                            is.na(dem_ep_outcome), 4, dem_ep_outcome),
                  # code the outcome for completed democratic deepening
                  dem_ep_outcome = ifelse(dem_ep_prch==0 & year==dem_ep_end_year &
                                            is.na(dem_ep_outcome), 5, dem_ep_outcome),
                  # code episodes with uncertain outcome
                  dem_ep_outcome = ifelse(dem_ep==1 & dem_ep_uncertain==1 & is.na(dem_ep_outcome) & year==dem_ep_end_year, 6, dem_ep_outcome),
                  dem_ep_outcome = ifelse(dem_ep==0, 0, dem_ep_outcome),
                  # code censored/ongoing episodes for survival analysis, new dummy variable
                  dem_ep_censored = ifelse(dem_ep==1 & dem_ep_uncertain == 1, 1, 0)) %>%
    # fill for the entire episode
    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(dem_ep_outcome = min(hablar::s(dem_ep_outcome)),
                  dem_ep_uncertain = ifelse(dem_ep==1 & max(dem_ep_outcome)!=6, 0, dem_ep_uncertain)) %>%
    # add aggregated outcome variable 
    dplyr::mutate(dem_ep_outcome_agg = dplyr::case_when(
      dem_ep_outcome == 2 | dem_ep_outcome == 3 | dem_ep_outcome == 4 ~ 2,
      dem_ep_outcome == 1  ~ 1,
      dem_ep_outcome == 5 ~ 3,
      dem_ep_outcome == 6 ~ 4,
      dem_ep_outcome == 0 ~ 0),
      # add variable for post-transition years within the episode
      dem_ep_ptr = ifelse(dem_ep_outcome == 1 & reg_type==1 & reg_trans !=1, 1, 0),
      # add variable for episodes that have both transition and deepening
      dem_ep_subdep = ifelse(dem_ep_outcome==1 & max(hablar::s(dem_ep_ptr == 1)), 1, 0)) %>%
    dplyr::group_by(country_text_id) %>%
    dplyr::arrange(country_id, year) %>%
    dplyr::select(-stasis)
  
  
  ### CODING THE AUTOCRATIZATION EPISODES ###
  
  # detect and save potential episodes with the help of the c++ function find_seqs
  full.df <- full.df %>% dplyr::mutate(episode_id = find_seqs_aut(v2x_polyarchy, v2x_regime, reg_trans,
                                                                  start_incl = start_incl * -1, year_turn, cum_turn, tolerance),
                                       # set a temporary id for these potential episodes and group accordinly
                                       character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(character_id) %>%
    # general check: is there a potential autocratization episode?
    dplyr::mutate(aut_ep = ifelse(!is.na(episode_id), 1, 0),
                  # we check whether the cumulated change in each potential episode was substantial (> cum_inc), i.e. the episode is manifest
                  aut_ep = ifelse(aut_ep == 1 & min(hablar::s(v2x_polyarchy)) - max(hablar::s(v2x_polyarchy)) <= cum_incl*-1, 1, 0)) %>%
    ungroup() %>%
    # then we clean out variables for non-manifest episodes
    dplyr::mutate(episode_id = ifelse(aut_ep != 1, NA, episode_id),
                  character_id = ifelse(aut_ep != 1, NA, character_id)) %>%
    group_by(character_id) %>%
    # generate the initial end year for the episode (note:  we have to filter out the stasis years that C++ gives us, but we will do this later):
    dplyr::mutate(aut_ep_end_year = ifelse(aut_ep == 1, last(year), NA),
                  #  find potential episodes with uncertain outcome (note: we might change this later depending on termination)
                  aut_ep_uncertain = ifelse(aut_ep == 1 & codingend - aut_ep_end_year<tolerance, 1, 0),
                  aut_ep_uncertain = ifelse(aut_ep == 1 & !is.na(gapstart) & (gapstart-1)-aut_ep_end_year<tolerance, 1, aut_ep_uncertain),
                  # generate the start year for the potential episode as the first year after the pre-episode year
                  aut_ep_start_year = ifelse(aut_ep == 1, first(year) + 1, NA),
                  # here we code a dummy for the pre-episode year
                  aut_pre_ep_year = ifelse(aut_ep == 1, ifelse(year == dplyr::first(year), 1, 0), 0),
                  # we create a unique identifier for episodes and phases using the country_text_id, start, and end years
                  aut_ep_id = ifelse(aut_ep == 1, paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"), NA)) %>%
    ungroup() %>%
    # remove the old identifiers we no longer need
    dplyr::select(-character_id, -episode_id) %>%
    # make sure the data is sorted properly
    dplyr::arrange(country_name, year) %>%
    # just to make sure we have a dataframe
    as.data.frame %>%
    
    # code termination type of autocratization episode
    
    # autocratization episodes end when one of five things happens:
    # 0. the episode outcome is unknown
    # 1. stasis: the case experiences no annual decrease = start_incl for the tolerance period (or more)
    # 2. year increase: the case experiences an annual increase >= year_turn
    # 3. cumulative increase: the case experiences a gradual increase >= cum_turn over the tolerance period (or less)
    # 4. democratic transition: the case experienced a democratic transition  or
    #                reverted to liberal democracies (applies to all cases, but reg_trans should capture for subtype 1)
    
  # first find the last negative change on EDI equal to the start_incl parameter
  group_by(aut_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)<=start_incl*-1, year, NA))),
                  # here we just replace with NA non-episode years
                  last_ch_year = ifelse(aut_ep==0, NA, last_ch_year)) %>%
    
    # here we check for dem. transition within the episode period (termination type #4)
    # now lets make sure to group by the country (not the episode!) and arrange by country-year
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    # then we find years where a country moved from lower values on RoW to liberal (4)
    dplyr::mutate(dem_trans = ifelse((dplyr::lead(v2x_regime, n=1) == 4 & v2x_regime < 4) |
                                       # and the years where the country experienced a transition on reg_trans
                                       dplyr::lead(reg_trans==1), year, NA)) %>%
    # now we need to group by episode to fill the values within episodes
    dplyr::ungroup() %>%
    dplyr::group_by(aut_ep_id) %>%
    # here we then find the first time in the episode that a democratic transition (dem_trans) occurs
    # limits dem_trans to episode years, making sure to exclude pre-episode year
    dplyr::mutate(dem_trans = ifelse(aut_ep==1 & year>=aut_ep_start_year, dem_trans, NA),
                  # finds the first year within the episode where dem_trans happens
                  dem_trans = min(hablar::s(dem_trans)),
                  # we recode the potential end date for the episode as the year before dem_trans
                  aut_ep_end_year = ifelse(!is.na(dem_trans) & aut_ep_end_year>dem_trans, dem_trans, aut_ep_end_year)) %>%
    # then we need to update our aut_ep_id with the new end date (we can clean up the other variables later)
    dplyr::ungroup() %>%
    dplyr::mutate(aut_ep_start_year = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_start_year),
                  aut_ep_end_year = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_end_year),
                  aut_ep = ifelse(aut_ep==1 & year>aut_ep_end_year, 0, aut_ep),
                  aut_ep_id = ifelse(aut_ep==1, paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"), NA)) %>%
    # then we can update our last_ch_year variable to reflect the new range of years for the episodes that terminated due to dem_trans
    dplyr::group_by(aut_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2x_polyarchy-dplyr::lag(v2x_polyarchy, n=1)<=start_incl*-1, year, NA))),
                  # here we just replace with NA non-episode years
                  last_ch_year = ifelse(aut_ep==0, NA, last_ch_year)) %>%
    
    # now lets make sure to group by the country (not the episode!) and arrange by country-year
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year)
  
  #### then check to see what happened after the episode had its last substantive change equal to start_incl*-1
  
  # we start with the yearly increase, aka year_turn
  year_incr <- list()
  # here we loop over the number of years (n) equal to the tolerance period after the last_change_year
  for (i in 1:tolerance) {
    # we calculate the first difference in the EDI for each of these yearly changes within the tolernce
    year_incr[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-dplyr::lead(full.df$v2x_polyarchy, n=i-1), NA)
  }
  # then we generate a dataframe from these calculations
  df1 <- do.call(cbind, lapply(year_incr, data.frame, stringsAsFactors=FALSE))
  # this just renames the columns to match the years ahead we are looking
  names <- paste0('year', seq(1:tolerance))
  colnames(df1) <- names
  # this transforms the result into a dataframe that we can use as a column in our existing dataframe
  # first write a function to deal with INF warnings
  my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)
  year_incr <- df1 %>%
    dplyr::mutate(year_incr = ifelse(apply(df1, 1, FUN = my.max) > year_turn, 1,NA))  %>%
    dplyr::select(year_incr)
  # now we can also use the first-differences we calculated above to look for stasis as well
  # this transforms the result into a dataframe that we can use as a column in our existing dataframe
  # note - we will have to clean this up later to account for cum_turn as well
  stasis <- df1 %>%
    # this checks whether the minimum annual change is less than start_incl*-1 over the tolerance period &
    #    that it is also greater than the year_turn parameter, i.e. stasis
    dplyr::mutate(stasis = ifelse(apply(df1, 1, FUN = min) > start_incl*-1 & apply(df1, 1, FUN = max) <= year_turn, 1,NA))  %>%
    dplyr::select(stasis)
  
  # now we look for a gradual increase equal to cum_incr over the tolerance period
  cum_incr <- list()
  # here we loop over time equal to the tolerance, looking for the difference between the last_change_year and that year on the EDI
  for (i in 1:tolerance) {
    cum_incr[[i]] <- ifelse(full.df$year == full.df$last_ch_year & dplyr::lead(full.df$country_id, n=i)==full.df$country_id, dplyr::lead(full.df$v2x_polyarchy, n=i)-full.df$v2x_polyarchy, NA)
  }
  # then we rename the columns and generate a dataframe we can use for our existing data
  df <- do.call(cbind, lapply(cum_incr, data.frame, stringsAsFactors=FALSE))
  names <- paste0('cum', seq(1:tolerance))
  colnames(df) <- names
  cum_incr <- df %>%
    dplyr::mutate(cum_incr = ifelse(apply(df, 1, FUN = my.max) >= cum_turn, 1,NA)) %>%
    dplyr::select(cum_incr)
  
  # merge these new columns to our full.df
  full.df <- full.df %>%
    tibble::rownames_to_column('newid') %>%
    left_join(tibble::rownames_to_column(year_incr, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(cum_incr, 'newid'), by = 'newid') %>%
    left_join(tibble::rownames_to_column(stasis, 'newid'), by = 'newid') %>%
    
    # now we can finally code our termination variable
    # lets make sure to group by the autocratization episode and arrange by country-year
    ungroup() %>%
    group_by(aut_ep_id) %>%
    dplyr::arrange(aut_ep_id, year) %>%
    # first, lets fill everything in for the episode
    dplyr::mutate(stasis = ifelse(aut_ep==1, max(hablar::s(stasis)), NA),
                  year_incr = ifelse(aut_ep==1, max(hablar::s(year_incr)), NA),
                  cum_incr = ifelse(aut_ep==1, max(hablar::s(cum_incr)), NA),
                  # then we can code the termination variable
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(stasis) & is.na(year_incr) & is.na(cum_incr)
                                              & is.na(dem_trans), 1, NA),
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(year_incr) & is.na(dem_trans), 2, aut_ep_termination),
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(cum_incr) & is.na(year_incr) & is.na(dem_trans), 3, aut_ep_termination),
                  aut_ep_termination = ifelse(aut_ep==1 & !is.na(dem_trans), 4, aut_ep_termination),
                  aut_ep_termination = ifelse(aut_ep==1 & aut_ep_uncertain==1 & is.na(aut_ep_termination), 0, aut_ep_termination),
                  # now we can clean up the other variables to reflect the true end of the episodes that have a known outcome
                  # first, let's fix the "uncertain" variable
                  aut_ep_uncertain = ifelse(aut_ep_termination !=0 & aut_ep==1, 0, aut_ep_uncertain),
                  # then we recode the end year as the final negative change if outcome is not unknown
                  aut_ep_end_year = ifelse(aut_ep_uncertain==0 & aut_ep==1, last_ch_year, aut_ep_end_year),
                  # then we clean up the other variables for non-episode years
                  aut_ep_termination = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_termination),
                  aut_ep_start_year = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_start_year),
                  aut_ep_end_year = ifelse(aut_ep==1 & year>aut_ep_end_year, NA, aut_ep_end_year),
                  aut_ep = ifelse(is.na(aut_ep_end_year), 0, aut_ep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(aut_ep_id = ifelse(aut_ep==1, paste(country_text_id, aut_ep_start_year, aut_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year) %>%
    
  # here we generate a variable capturing whether the episode had the potential for a regime change 
  dplyr::group_by(aut_ep_id) %>%
    # to do so we look for the regime type in the pre-episode year, and if it was autocratic, we consider this episode to have the potential for regime change
    dplyr::mutate(aut_ep_prch = ifelse(!is.na(aut_ep_id), dplyr::first(reg_type, order_by= year), 0),
                  # then we code the outcomes:
                  # did a regime change on RoW during the episode produce a genuine democratic breakdown?
                  aut_ep_outcome = ifelse(aut_ep_prch==1 & reg_trans==-1 & aut_pre_ep_year==0, 1, NA),
                  # did a regime change on RoW during the episode fail to produce a democratic breakdown (preempted breakdown)?
                  aut_ep_outcome = ifelse(aut_ep_prch==1 & any(row_regch_event==-1 & aut_pre_ep_year==0) &
                                            year==aut_ep_end_year & aut_ep_uncertain==0 &
                                            is.na(aut_ep_outcome), 2, aut_ep_outcome),
                  # did the democratic regression phase result in a diminished democracy?
                  aut_ep_outcome = ifelse(aut_ep_prch==1 & year==aut_ep_end_year & aut_ep_termination==1
                                          & is.na(aut_ep_outcome), 3, aut_ep_outcome),
                  # did the democratic regression phase result in an averted regression?
                  aut_ep_outcome = ifelse(aut_ep_prch==1 & year==aut_ep_end_year &
                                            (aut_ep_termination==2 | aut_ep_termination==3 | aut_ep_termination==4) &
                                            is.na(aut_ep_outcome), 4, aut_ep_outcome),
                  # code the outcome for completed autocratic regression
                  aut_ep_outcome = ifelse(aut_ep_prch==0 & year==aut_ep_end_year &
                                            is.na(aut_ep_outcome), 5, aut_ep_outcome),
                  # code episodes with uncertain outcome
                  aut_ep_outcome = ifelse(aut_ep==1 & aut_ep_uncertain==1 & is.na(aut_ep_outcome) & year==aut_ep_end_year, 6, aut_ep_outcome),
                  aut_ep_outcome = ifelse(aut_ep==0, 0, aut_ep_outcome),
                  # code censored/ongoing episodes for survival analysis, new dummy variable
                  aut_ep_censored = ifelse(aut_ep==1 & aut_ep_uncertain == 1, 1, 0)) %>%
    # fill for the entire episode
    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(aut_ep_outcome = min(hablar::s(aut_ep_outcome)),
                  aut_ep_uncertain = ifelse(aut_ep==1 & max(aut_ep_outcome)!=6, 0, aut_ep_uncertain)) %>%
    # add aggregated outcome variable for outcomes
    dplyr::mutate(aut_ep_outcome_agg = dplyr::case_when(
      aut_ep_outcome == 2 | aut_ep_outcome == 3 | aut_ep_outcome == 4 ~ 2,
      aut_ep_outcome == 1 ~ 1,
      aut_ep_outcome == 5 ~ 3,
      aut_ep_outcome == 6 ~ 4,
      aut_ep_outcome == 0 ~ 0),
      # add variable for post-breakdown years within the episode
      aut_ep_pbr = ifelse(aut_ep_outcome == 1 & reg_type==0 & reg_trans !=-1, 1, 0),
      # add variable for episodes that have both breakdown and autocratic regression
      aut_ep_subreg = ifelse(aut_ep_outcome==1 & max(hablar::s(aut_ep_pbr == 1)), 1, 0)) %>%
    dplyr::group_by(country_text_id) %>%
    dplyr::arrange(country_id, year) %>%
    
    
    # clean out values from pre-episode year
    dplyr::mutate(dem_ep = ifelse(dem_pre_ep_year==1, 0, dem_ep),
                  dem_ep_termination = ifelse(dem_pre_ep_year==1, NA, dem_ep_termination),
                  # sub_dem_ep = ifelse(dem_pre_ep_year==1, 0, sub_dem_ep),
                  dem_ep_prch = ifelse(dem_pre_ep_year==1, 0, dem_ep_prch),
                  dem_ep_ptr = ifelse(dem_pre_ep_year==1, 0, dem_ep_ptr), 
                  dem_ep_subdep = ifelse(dem_pre_ep_year==1, 0, dem_ep_subdep),
                  dem_ep_outcome_all = dem_ep_outcome,
                  dem_ep_outcome = ifelse(dem_pre_ep_year==1, 0, dem_ep_outcome),
                  dem_ep_outcome_agg = ifelse(dem_pre_ep_year==1, 0, dem_ep_outcome_agg),
                  dem_ep_uncertain = ifelse(dem_pre_ep_year==1, 0, dem_ep_uncertain),
                  dem_ep_censored = ifelse(dem_pre_ep_year==1, 0, dem_ep_censored),
                  aut_ep = ifelse(aut_pre_ep_year==1, 0, aut_ep),
                  aut_ep_termination = ifelse(aut_pre_ep_year==1, NA, aut_ep_termination),
                  # sub_aut_ep = ifelse(aut_pre_ep_year==1, 0, sub_aut_ep),
                  aut_ep_prch = ifelse(aut_pre_ep_year==1, 0, aut_ep_prch),
                  aut_ep_pbr = ifelse(aut_pre_ep_year==1, 0, aut_ep_pbr), 
                  aut_ep_subreg = ifelse(aut_pre_ep_year==1, 0, aut_ep_subreg),
                  aut_ep_outcome_all = aut_ep_outcome,
                  aut_ep_outcome = ifelse(aut_pre_ep_year==1, 0, aut_ep_outcome),
                  aut_ep_outcome_agg = ifelse(aut_pre_ep_year==1, 0, aut_ep_outcome_agg),
                  aut_ep_uncertain = ifelse(aut_pre_ep_year==1, 0, aut_ep_uncertain),
                  aut_ep_censored = ifelse(aut_pre_ep_year==1, 0, aut_ep_censored)) %>%
    
    # select the variables we need to keep
    dplyr::filter(!is.na(origsample)) %>%
    dplyr::select(country_id, country_text_id, country_name, year, v2x_regime, v2x_polyarchy, v2x_polyarchy_codelow, v2x_polyarchy_codehigh,
                  reg_start_year, reg_end_year, reg_id, reg_type, reg_trans, founding_elec, row_regch_event, row_regch_censored,
                  dem_ep, dem_ep_id, dem_ep_start_year, dem_ep_end_year, dem_pre_ep_year, dem_ep_termination,
                  #sub_dem_ep, sub_dem_ep_id, sub_dem_ep_start_year, sub_dem_ep_end_year, 
                  dem_ep_prch, dem_ep_ptr, dem_ep_subdep, 
                  dem_ep_outcome, dem_ep_outcome_agg, dem_ep_censored,
                  aut_ep, aut_ep_id, aut_ep_start_year, aut_ep_end_year, aut_pre_ep_year, aut_ep_termination,
                  #sub_aut_ep, sub_aut_ep_id, sub_aut_ep_start_year, sub_aut_ep_end_year, 
                  aut_ep_prch, aut_ep_pbr, aut_ep_subreg,
                  aut_ep_outcome, aut_ep_outcome_agg, aut_ep_censored) %>%
    ungroup()
  
  
  {
    return(full.df)
    }
}
### done ;-) ###
