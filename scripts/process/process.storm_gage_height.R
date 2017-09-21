# resample the gage data down to the same steps as timesteps

process.storm_gage_height <- function(viz = as.viz("storm-gage-height")) {
  library(dplyr)
  deps <- readDepends(viz)
  checkRequired(deps, c("gage-height", "timesteps"))
  
  times <- as.POSIXct(strptime(unlist(deps[["timesteps"]]),'%b %d %I:%M %p'), tz = "America/Puerto_Rico")
  
  no_value <- -999999
  
  
  problem_gages <- data.frame(site_no = c("50106100","50136400"), 
                              new_nil = c(3.25,1.5), 
                              stringsAsFactors = FALSE)
  
  gage_height <- deps[["gage-height"]]
  
  gage_nils <- gage_height %>% 
    filter(p_Inst != no_value) %>%
    group_by(site_no) %>%
    summarize(nil = (min(p_Inst)-0.1)) %>% # this -0.1 makes these mins unique but not mess with scaling.
    left_join(problem_gages, by="site_no") %>%
    mutate(nil = ifelse(!is.na(new_nil), new_nil, nil)) %>%
    select(-new_nil)
  
  gage_height <- left_join(gage_height, gage_nils, by="site_no") %>%
    mutate(p_Inst = ifelse(p_Inst == no_value, nil, p_Inst)) %>%
    mutate(p_Inst = ifelse(p_Inst < nil, nil, p_Inst)) %>%
    select(-nil)
  
  sites <- gage_nils$site_no
  
  timestep.q <- list()
  
  for(site in sites){
    gage_tvp <- filter(gage_height, site_no == site) %>%
      select(dateTime, p_Inst)
    
    out <- approx(x = gage_tvp$dateTime, y = gage_tvp$p_Inst, 
                  xout = times)
    out$y[is.na(out$y)] <- gage_nils$nil[gage_nils$site_no == site]
    timestep.q[[site]] <- out$y
  }
  
  gage_nils <- setNames(gage_nils$nil, gage_nils$site_no)
  
  saveRDS(list(gage_mask_vals = gage_nils, timestep_q = timestep.q), file = viz[['location']])
}