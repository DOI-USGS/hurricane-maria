fetch.gage_data <- function(viz = as.viz("gage-data")){
  library('dplyr')
  depData <- readDepends(viz)
  
  sites <- depData[["nws-data"]][['site_no']]
  config_data <- depData[["config"]]
  
  times <- as.POSIXct(strptime(depData[['timesteps']]$times, format='%b %d %I:%M %p', tz="America/Puerto_Rico"))
  
  gage <- dataRetrieval::readNWISuv(sites, parameterCd = config_data$pCode,
                            startDate = as.Date(times[1], tz="America/Puerto_Rico"), 
                            endDate = as.Date(tail(times, 1L), tz="America/Puerto_Rico")) %>%
    rename(p_Inst = X_00065_00000,
           p_Inst_cd = X_00065_00000_cd)
  
  attr(gage$dateTime, "tzone") <- "America/Puerto_Rico"
  
  saveRDS(gage, file=viz[['location']])
}

fetchTimestamp.gage_data <- neverCurrent
