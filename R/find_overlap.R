#' Find the overlap between episodes of democratization and autocratization.
#'
#' @param episodes The outcome of get_eps(), Episodes of Regime Transformation (ERT), 
#' to be used for finding potential overlaps (depending on individual parameter setting). By default with standard parameters.
#'
#' @return A summary message and data frame showing the overlaps between democratization and autocratization episodes.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @export
#'
#' @examples
#' #Don't run
#' #Find the overlap between democratization and autocratization episodes
#'
#' #overlap <- find_overlap(episodes)
#'
find_overlap <-function(start_incl  = 0.01,
                        cum_incl  = 0.1,
                        year_turn = 0.03,
                        cum_turn = 0.1,
                        tolerance = 5)
{
  
  episodes = ERT::get_eps(data = ERT::vdem,
                          start_incl = start_incl,
                          cum_incl = cum_incl,
                          year_turn = year_turn,
                          cum_turn = cum_turn,
                          tolerance = tolerance)
  
  aut_ep <- country_name <- year <- dem_ep <- country_text_id <- aut_ep_id <- dem_ep_id <- year_diff <-
    conseq_episode <- overlap_counter <- overlap_period <- NULL
  
  # Find overlapping episodes
  merged <- episodes
  aut <- merged %>% filter(aut_ep == 1) %>% dplyr::select(country_name, country_text_id, year, aut_ep_id, dem_ep_id)
  dem <- merged  %>% filter(dem_ep == 1) %>% dplyr::select(country_name, country_text_id, year, aut_ep_id, dem_ep_id)
  overlap1 <- rbind(aut,dem)[duplicated(rbind(aut,dem)),]
  
  # Stop if there are no overlapping episodes
  if(nrow(overlap1) == 0) {
   print("No overlapping episodes in the data.")
  }
  
  if(nrow(overlap1) > 0) {
    
  # Identify sequences of overlapping years 
  overlap <- overlap1 %>% 
    arrange(country_name, year) %>% 
    group_by(country_name) %>%
    mutate(year_diff = year - dplyr::lag(year), 
           conseq_episode = ifelse(dplyr::lead(as.numeric(year_diff)) < 2, 0, 1)) %>% 
    ungroup()  %>%
    mutate(conseq_episode = ifelse(is.na(conseq_episode), 1, conseq_episode),
           overlap_counter = dplyr::lag(cumsum(conseq_episode == 1L)),
           overlap_counter = ifelse(is.na(overlap_counter), 0, overlap_counter),
           overlap_counter = overlap_counter + 1) %>% 
    group_by(overlap_counter) %>% 
    mutate(overlap_period = paste0(min(year), "-", max(year))) %>% 
    ungroup() %>% 
    select(country_name, country_text_id, year, overlap_period, aut_ep_id, dem_ep_id)

  # Informat user about the result
  print(paste0("There are ", length(unique(overlap$overlap_period)), " overlapping episodes (", nrow(overlap), " overlapping country-years) in ",
               length(unique(overlap$country_name)), " countries in the episodes data."))
  
  return(overlap)
  }
}
