# resample the gage data down to the same steps as timesteps

process.storm_gage_height <- function(viz = as.viz("storm-gage-height")) {
  library(dplyr)
  deps <- readDepends(viz)
  checkRequired(deps, c("gage-height", "timesteps"))
  
  times <- as.POSIXct(strptime(unlist(deps[["timesteps"]]),'%b %d %I:%M %p'), tz = "America/Puerto_Rico")
  
  no_value <- -999999
  
  gage_height <- deps[["gage-height"]]
  
  
  gage_mins <- gage_height %>% 
    filter(p_Inst != no_value) %>%
    group_by(site_no) %>%
    summarize(min = min(p_Inst))
  
  gage_height <- left_join(gage_height, gage_mins, by="site_no") %>%
    mutate(p_Inst = ifelse(p_Inst == no_value, min, p_Inst)) %>%
    select(-min)
  
  sites <- gage_mins$site_no
  
  timestep.q <- list()
  
  for(site in sites){
    gage_tvp <- filter(gage_height, site_no == site) %>%
      select(dateTime, p_Inst)
    
    out <- approx(x = gage_tvp$dateTime, y = gage_tvp$p_Inst, 
                  xout = times)
    out$y[is.na(out$y)] <- gage_mins$min[gage_mins$site_no == site]
    timestep.q[[site]] <- out$y
  }

  saveRDS(timestep.q, file = viz[['location']])
}