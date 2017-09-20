# resample the gage data down to the same steps as timesteps

process.storm_gage_height <- function(viz = as.viz("storm-gage-height")) {
  library(dplyr)
  deps <- readDepends(viz)
  checkRequired(deps, c("gage-height", "timesteps"))
  
  times <- as.POSIXct(strptime(unlist(deps[["timesteps"]]),'%b %d %I:%M %p'), tz = "America/Puerto_Rico")
  
  gage_height <- deps[["gage-height"]]
  
  gage_height <- gage_height %>% 
    group_by(site_no)
  
  interp_q <- function(site.no) {
    use.i <- gage_height$site_no == site.no
    gage_obs <- gage_height$p_Inst[use.i]
    gage_obs[which(gage_obs == -999999.00)] <- min(gage_obs[which(gage_obs > 0)])
    out <- approx(x = gage_height$dateTime[use.i], y = gage_obs, xout = times)
    out$y[which(is.na(out$y))] <- min(gage_obs[which(gage_obs > 0)])
    out
  }
  sites <- unique(gage_height$site_no)
  timestep.q <- lapply(sites, interp_q) %>% setNames(sites)
  
  saveRDS(timestep.q, file = viz[['location']])
}