fetch.gage_data <- function(viz = as.viz("gage-data")){
  
  depData <- readDepends(viz)
  
  sites <- depData[["nws-data"]][['site_no']]
  config_data <- depData[["config"]]
  dates <- depData[["dates"]]
  
  gage <- dataRetrieval::readNWISuv(sites, parameterCd = config_data$pCode,
                            startDate = dates$startDate, endDate = dates$endDate) %>%
    rename(p_Inst = X_00065_00000,
           p_Inst_cd = X_00065_00000_cd)
  
  saveRDS(gage, file=viz[['location']])
}

fetchTimestamp.gage_data <- vizlab:::fetchTimestamp.file