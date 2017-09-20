# resample the gage data down to the same steps as timesteps

process.storm_gage_height <- function(viz = as.viz("storm-gage-height")) {
  library(dplyr)
  deps <- readDepends(viz)
  checkRequired(deps, c("gage-height", "timesteps"))
  
  times <- as.POSIXct(strptime(unlist(deps[["timesteps"]]),'%b %d %I:%M %p'), tz = "America/Puerto_Rico")
  
  gage_height <- deps[["gage-height"]]
  attr(gage_height$dateTime, "tzone") <- "America/Puerto_Rico"
  
  gage_height <- gage_height %>% 
    group_by(site_no)
  
  interp_q <- function(site.no) {
    use.i <- gage_height$site_no == site.no
    approx(x = gage_height$dateTime[use.i], y = gage_height$p_Inst[use.i], xout = times)
  }
  sites <- unique(gage_height$site_no)
  timestep.q <- lapply(sites, interp_q) %>% setNames(sites)
  
  saveRDS(timestep.q, file = viz[['location']])
}