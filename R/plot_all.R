
plot_all <- function(abs = T,
                     years = c(1900, 2020),
                     start_incl  = 0.01,
                     cum_incl  = 0.1,
                     year_turn = 0.03,
                     cum_turn = 0.1,
                     tolerance = 5) {
  
  eps <- ERT::get_eps(data = ERT::vdem,
                      start_incl = start_incl,
                      cum_incl = cum_incl,
                      year_turn = year_turn,
                      cum_turn = cum_turn,
                      tolerance = tolerance)
  
  
  stopifnot(is.logical(abs), length(abs) == 1)
  
  stopifnot(is.numeric(years), length(years) == 2, years[2] > years[1])
  
  stopifnot(is.numeric(start_incl), length(start_incl) == 1)
  
  stopifnot(is.numeric(cum_incl), length(cum_incl) == 1)
  
  stopifnot(is.numeric(year_turn), length(year_turn) == 1)
  
  stopifnot(is.numeric(cum_turn), length(cum_turn) == 1)
  
  stopifnot(is.numeric(tolerance), length(tolerance) == 1)
  
  #perhaps this is redundant 
  if(min(years)<min(ERT::vdem$year) | max(years)>max(ERT::vdem$year))
    stop("Error: Data not available for time range")
  
  if (isTRUE(abs)) {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      {if(nrow(.) == 0) stop("No episodes during selected time period. No plot generated") else .} %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(dem_eps = sum(dem_ep),
                       aut_eps = sum(aut_ep)) %>%
      tidyr::pivot_longer(cols = c(dem_eps, aut_eps), names_to = "ep_type", values_to = "countries")
    
  } else {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(dem_eps = sum(dem_ep) / length(unique(country_id)),
                       aut_eps = sum(aut_ep) / length(unique(country_id))) %>%
      tidyr::pivot_longer(cols = c(dem_eps, aut_eps), names_to = "ep_type", values_to = "countries")
  }
  
  p <-  ggplot2::ggplot(data = eps_year, aes(x = year, y = countries, group = ep_type, linetype = ep_type)) +
    geom_line() +
    scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
    scale_linetype(name = "", breaks = c("aut_eps", "dem_eps"), labels = c("Autocratization", "Democratization")) +
    xlab("Year") +
    theme_classic() +
    theme(legend.position = "bottom")
  
  if (isTRUE(abs)) {
    p +  ylab("Number of Countries")
  }  else {
    p +  ylab("Countries (%)")
  }
}