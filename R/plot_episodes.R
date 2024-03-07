#' Plot Episodes of Regime Transformation (ERT) over time.
#'
#' `plot_episodes` plots Episodes of Regime Transformation (ERT) over time for a selected country and a selected time frame.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a plot that shows
#' democratization and autocratization episodes for a selected country over time.
#' The legend includes information on the start and end data of each episode,
#' as well as the episode outcome. The function calls the [ERT:get_eps()] function
#' to identify episodes.
#'
#' @param years Vector with two numeric values indicating the minimum and maximum year to be plotted.
#'
#' @param country Character vector containing the country for which episodes should be shown. Only entries from the
#'  country_name column in the V-Dem data set are accepted.
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
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set.
#'
#' @return The output of this function is a [ggplot2:ggplot()] object with episodes for a selected country.

#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'
#' # Plot episodes for Belgium between 1910 and 2010.
#'
#'  plot_episodes(country = c("Belgium"),
#'                years = c(1910, 2010))
#' }
#' @export

plot_episodes <- function(years = c(1900, 2023),
                          country = c("Sweden"),
                          start_incl  = 0.01,
                          cum_incl  = 0.1,
                          year_turn = 0.03,
                          cum_turn = 0.1,
                          tolerance = 5,
                          data = ERT::vdem) {
  
  eps <- ERT::get_eps(data = data,
                      start_incl = start_incl,
                      cum_incl = cum_incl,
                      year_turn = year_turn,
                      cum_turn = cum_turn,
                      tolerance = tolerance)
  
  
  stopifnot(is.numeric(years), length(years) == 2, years[2] > years[1])
  
  stopifnot(is.character(country))
  
  if(length(country) > 1)
    stop("Error: More than one country selected")
  
  if(length(country) == 0)
    stop("Error: No country selected")
  
  if(!country %in% data$country_name)
    stop("Error: Country not found")
  
  if(max(years) < min(eps %>% filter(country_name==country) %>% pull(year)) | max(years)>max(eps %>% filter(country_name==country) %>% pull(year)))
    stop("Error: Data not available for time range")
  
  stopifnot(is.numeric(start_incl), length(start_incl) == 1)
  
  stopifnot(is.numeric(cum_incl), length(cum_incl) == 1)
  
  stopifnot(is.numeric(year_turn), length(year_turn) == 1)
  
  stopifnot(is.numeric(cum_turn), length(cum_turn) == 1)
  
  stopifnot(is.numeric(tolerance), length(tolerance) == 1)
  
  
  year <- country_name <- dem_ep <- aut_ep <- overlap_eps <- country_text_id <- v2x_polyarchy <-
    ep_type <- episode <- vdem <- aut_ep_start_year <- aut_ep_end_year <-
    dem_ep_start_year <- dem_ep_end_year <- aut_pre_ep_year <-
    dem_pre_ep_year <- episode_id <- countries <- NULL
  
  eps_year <- eps %>%
    dplyr::filter(country_name == country, dplyr::between(year, min(years), max(years))) %>%
    dplyr::filter(dem_ep == 1 | aut_ep == 1) 
  
  if(nrow(eps_year)>1){
    eps_year <- eps_year %>% 
      dplyr::mutate(overlap_eps = ifelse(!is.na(aut_ep_id) & !is.na(dem_ep_id), "overlaps", NA)) %>% 
      tidyr::pivot_longer(cols = c(aut_ep_id, dem_ep_id, overlap_eps), names_to = "ep_type", values_to = "episode") %>%
      dplyr::select(country_name, country_text_id, year, v2x_polyarchy, ep_type, episode,
                    aut_ep_start_year, aut_ep_end_year, aut_ep_outcome,
                    dem_ep_start_year, dem_ep_end_year,
                    aut_pre_ep_year, dem_pre_ep_year, dem_ep_outcome,
                   aut_ep_censored, dem_ep_censored) %>%
      dplyr::filter((ep_type == "dem_ep_id" & dem_pre_ep_year == 0) |
                      (ep_type == "aut_ep_id" & aut_pre_ep_year == 0) |
                      ep_type == "overlaps" & aut_pre_ep_year == 0 & dem_pre_ep_year == 0) %>%
      drop_na(episode) %>%
      group_by(year) %>%
      mutate(overlap_eps = n(),
             outcome_dem_ep = case_when(dem_ep_outcome == 6 | dem_ep_censored == 1 ~ "Outcome censored",
                                        dem_ep_outcome == 1 ~ "Democratic transition",
                                        dem_ep_outcome == 2 ~ "Preempted democratic transition",
                                        dem_ep_outcome == 3 ~ "Stabilized electoral autocracy",
                                        dem_ep_outcome == 4 ~ "Reverted liberalization",
                                        dem_ep_outcome == 5 ~ "Deepened democracy",
                                        T ~ NA_character_),
             outcome_aut_ep = case_when(aut_ep_outcome == 6  | aut_ep_censored == 1 ~ "Outcome censored",
                                        aut_ep_outcome == 1 ~ "Democratic breakdown",
                                        aut_ep_outcome == 2 ~ "Preempted democratic breakdown",
                                        aut_ep_outcome == 3 ~ "Diminished democracy",
                                        aut_ep_outcome == 4 ~ "Averted regression",
                                        aut_ep_outcome == 5 ~ "Regressed autocracy",
                                        T ~ NA_character_),
             episode_id = ifelse(ep_type == "aut_ep_id", paste0("AUT: ", aut_ep_start_year, "-", aut_ep_end_year, " ", outcome_aut_ep), episode),
             episode_id = ifelse(ep_type == "dem_ep_id", paste0("DEM: ", dem_ep_start_year, "-", dem_ep_end_year, " ", outcome_dem_ep), episode_id)) %>%
      ungroup()
    
    polyarchy <- eps %>%
      filter(country_name == country, between(year, min(years), max(years))) %>%
      ungroup() %>%
      select(year, v2x_polyarchy)
    
    if(max(eps_year$overlap_eps) > 1) {
      print("Warning: Some episodes overlap!")
    }
    
    p <-   ggplot2::ggplot() +
      geom_line(data = eps_year, aes(group = episode_id, color = episode_id, linetype = ep_type,x = year, y = v2x_polyarchy)) +
      geom_line(data = polyarchy, aes(x = year, y = v2x_polyarchy), alpha = 0.35) +
      scale_colour_grey(breaks = levels(factor(eps_year$episode_id[eps_year$episode_id!="overlaps"])),
                        name = "Episode", start = 0.01, end = 0.01) +
      scale_linetype_manual(name = "Episode type", breaks = c("aut_ep_id", "dem_ep_id", "overlaps"),
                            labels = c("Autocratization", "Democratization", "Overlap"),
                            values = c("dashed", "dotted", "solid")) +
      scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
      xlab("Year") +  ylab("Electoral Democracy Index") + ylim(0,1) +
      theme_bw() +
      guides(color = guide_legend(override.aes = list(size = 0))) +
      ggtitle(sprintf("%s", country))
    
    if (isTRUE(length(which(eps_year$ep_type == "dem_ep_id")) > 0)){
      
      if (any(eps_year$year%in%c(eps_year$dem_ep_start_year))) {
        p <- p +  geom_point(data = eps_year, aes(x = year, y = ifelse(year == dem_ep_start_year, v2x_polyarchy, NA)), shape = 2, alpha = 0.75) 
        
      } else {
        p
      }
      
      if (any(eps_year$year%in%c(eps_year$dem_ep_end_year))) {
        p <- p +geom_point(data = eps_year, aes(x = year, y = ifelse(year == dem_ep_end_year, v2x_polyarchy, NA)), shape = 17, alpha = 0.75)
      } else {
        p
      }
    }
    
    if (isTRUE(length(which(eps_year$ep_type == "aut_ep_id")) > 0)) {
      
      if (any(eps_year$year%in%c(eps_year$aut_ep_start_year))){
        p <- p +  geom_point(data = eps_year, aes(x = year, y = ifelse(year == aut_ep_start_year, v2x_polyarchy, NA)), shape = 1, alpha = 0.75) 
      } else {
        p
      }
      if (any(eps_year$year%in%c(eps_year$aut_ep_end_year))){
        p<- p+ geom_point(data = eps_year, aes(x = year, y = ifelse(year == aut_ep_end_year, v2x_polyarchy, NA)), shape = 16, alpha = 0.75)
      } else {
        p
      }
    }
    p
    
    
  } else {
    print("No episodes during selected period.")
    
    polyarchy <- eps %>%
      filter(country_name == country, between(year, min(years), max(years))) %>%
      ungroup() %>%
      select(year, v2x_polyarchy)
    
    p <-ggplot2::ggplot() +
      geom_line(data = polyarchy, aes(x = as.numeric(year), y = v2x_polyarchy), alpha = 0.35) +
      scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
      xlab("Year") +  ylab("Electoral Democracy Index") + ylim(0,1) +
      theme_bw() +
      ggtitle(sprintf("%s", country))
    
    p
    
  }
}




